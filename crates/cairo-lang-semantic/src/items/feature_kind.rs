use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, ModuleId};
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::db::{FilesGroup, default_crate_settings};
use cairo_lang_filesystem::ids::{CrateId, StrRef};
use cairo_lang_syntax::attribute::consts::{
    ALLOW_ATTR, DEPRECATED_ATTR, FEATURE_ATTR, INTERNAL_ATTR, UNSTABLE_ATTR,
};
use cairo_lang_syntax::attribute::structured::{
    self, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;
use salsa::Database;

use crate::SemanticDiagnostic;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};

/// The kind of a feature for an item.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub enum FeatureKind<'db> {
    /// The feature of the item is stable.
    Stable,
    /// The feature of the item is unstable, with the given name to allow.
    Unstable { feature: StrRef<'db>, note: Option<StrRef<'db>> },
    /// The feature of the item is deprecated, with the given name to allow, and an optional note
    /// to appear in diagnostics.
    Deprecated { feature: StrRef<'db>, note: Option<StrRef<'db>> },
    /// This feature is for internal corelib use only. Using it in user code is not advised.
    Internal { feature: StrRef<'db>, note: Option<StrRef<'db>> },
}
impl<'db> FeatureKind<'db> {
    pub fn from_ast(
        db: &'db dyn Database,
        diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
        attrs: &ast::AttributeList<'db>,
    ) -> Self {
        let unstable_attrs = attrs.query_attr(db, UNSTABLE_ATTR).collect_vec();
        let deprecated_attrs = attrs.query_attr(db, DEPRECATED_ATTR).collect_vec();
        let internal_attrs = attrs.query_attr(db, INTERNAL_ATTR).collect_vec();
        if unstable_attrs.is_empty() && deprecated_attrs.is_empty() && internal_attrs.is_empty() {
            return Self::Stable;
        };
        if unstable_attrs.len() + deprecated_attrs.len() + internal_attrs.len() > 1 {
            add_diag(diagnostics, attrs.stable_ptr(db), FeatureMarkerDiagnostic::MultipleMarkers);
            return Self::Stable;
        }

        if !unstable_attrs.is_empty() {
            let attr = unstable_attrs.into_iter().next().unwrap().structurize(db);
            let [feature, note, _] =
                parse_feature_attr(db, diagnostics, &attr, ["feature", "note", "since"]);
            feature.map(|feature| Self::Unstable { feature, note }).ok_or(attr)
        } else if !deprecated_attrs.is_empty() {
            let attr = deprecated_attrs.into_iter().next().unwrap().structurize(db);
            let [feature, note, _] =
                parse_feature_attr(db, diagnostics, &attr, ["feature", "note", "since"]);
            feature.map(|feature| Self::Deprecated { feature, note }).ok_or(attr)
        } else {
            let attr = internal_attrs.into_iter().next().unwrap().structurize(db);
            let [feature, note, _] =
                parse_feature_attr(db, diagnostics, &attr, ["feature", "note", "since"]);
            feature.map(|feature| Self::Internal { feature, note }).ok_or(attr)
        }
        .unwrap_or_else(|attr| {
            add_diag(diagnostics, attr.stable_ptr, FeatureMarkerDiagnostic::MissingAllowFeature);
            Self::Stable
        })
    }
}

/// A trait for retrieving the `FeatureKind` of an item.
pub trait HasFeatureKind<'db> {
    /// Returns the `FeatureKind` associated with the implementing struct.
    fn feature_kind(&self) -> &FeatureKind<'db>;
}

/// Diagnostics for feature markers.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum FeatureMarkerDiagnostic {
    /// Multiple markers on the same item.
    MultipleMarkers,
    /// Every marker must have a feature argument, to allow ignoring the warning.
    MissingAllowFeature,
    /// Unsupported argument in the feature marker attribute.
    UnsupportedArgument,
    /// Duplicated argument in the feature marker attribute.
    DuplicatedArgument,
}

/// Parses the feature attribute.
fn parse_feature_attr<'db, const EXTRA_ALLOWED: usize>(
    db: &'db dyn Database,
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
    attr: &structured::Attribute<'db>,
    allowed_args: [&str; EXTRA_ALLOWED],
) -> [Option<StrRef<'db>>; EXTRA_ALLOWED] {
    let mut arg_values = std::array::from_fn(|_| None);
    for AttributeArg { variant, arg, .. } in &attr.args {
        let AttributeArgVariant::Named { value: ast::Expr::String(value), name } = variant else {
            add_diag(diagnostics, arg.stable_ptr(db), FeatureMarkerDiagnostic::UnsupportedArgument);
            continue;
        };
        let Some(i) = allowed_args.iter().position(|x| *x == name.text) else {
            add_diag(diagnostics, name.stable_ptr, FeatureMarkerDiagnostic::UnsupportedArgument);
            continue;
        };
        if arg_values[i].is_some() {
            add_diag(diagnostics, name.stable_ptr, FeatureMarkerDiagnostic::DuplicatedArgument);
        } else {
            arg_values[i] = Some(value.text(db).into());
        }
    }
    arg_values
}

/// Helper for adding a marker diagnostic.
fn add_diag<'db>(
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
    stable_ptr: impl TypedStablePtr<'db>,
    diagnostic: FeatureMarkerDiagnostic,
) {
    diagnostics.add(SemanticDiagnostic::new(
        StableLocation::new(stable_ptr.untyped()),
        SemanticDiagnosticKind::FeatureMarkerDiagnostic(diagnostic),
    ));
}

/// The feature configuration on an item.
/// May be accumulated, or overridden by inner items.
#[derive(Clone, Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct FeatureConfig<'db> {
    /// The current set of allowed features.
    pub allowed_features: OrderedHashSet<StrRef<'db>>,
    /// Whether to allow all deprecated features.
    pub allow_deprecated: bool,
    /// Whether to allow unused imports.
    pub allow_unused_imports: bool,
}

impl<'db> FeatureConfig<'db> {
    /// Overrides the current configuration with another one.
    ///
    /// Returns the data required to restore the configuration.
    pub fn override_with(&mut self, other: Self) -> FeatureConfigRestore<'db> {
        let mut restore = FeatureConfigRestore {
            features_to_remove: vec![],
            allow_deprecated: self.allow_deprecated,
            allow_unused_imports: self.allow_unused_imports,
        };
        for feature_name in other.allowed_features {
            if self.allowed_features.insert(feature_name) {
                restore.features_to_remove.push(feature_name);
            }
        }
        self.allow_deprecated |= other.allow_deprecated;
        self.allow_unused_imports |= other.allow_unused_imports;
        restore
    }

    /// Restores the configuration to a previous state.
    pub fn restore(&mut self, restore: FeatureConfigRestore<'db>) {
        for feature_name in restore.features_to_remove {
            self.allowed_features.swap_remove(&feature_name);
        }
        self.allow_deprecated = restore.allow_deprecated;
        self.allow_unused_imports = restore.allow_unused_imports;
    }
}

/// The data required to restore the feature configuration after an override.
pub struct FeatureConfigRestore<'db> {
    /// The features to remove from the configuration after the override.
    features_to_remove: Vec<StrRef<'db>>,
    /// The previous state of the allow deprecated flag.
    allow_deprecated: bool,
    /// The previous state of the allow unused imports flag.
    allow_unused_imports: bool,
}

/// Returns the allowed features of an object which supports attributes.
pub fn feature_config_from_ast_item<'db>(
    db: &'db dyn SemanticGroup,
    crate_id: CrateId<'db>,
    syntax: &impl QueryAttrs<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> FeatureConfig<'db> {
    let mut config = FeatureConfig::default();
    process_feature_attr_kind(
        db,
        syntax,
        FEATURE_ATTR,
        || SemanticDiagnosticKind::UnsupportedFeatureAttrArguments,
        diagnostics,
        |value| {
            if let ast::Expr::String(value) = value {
                config.allowed_features.insert(value.text(db).into());
                true
            } else {
                false
            }
        },
    );
    process_feature_attr_kind(
        db,
        syntax,
        ALLOW_ATTR,
        || SemanticDiagnosticKind::UnsupportedAllowAttrArguments,
        diagnostics,
        |value| match value.as_syntax_node().get_text_without_trivia(db) {
            "deprecated" => {
                config.allow_deprecated = true;
                true
            }
            "unused_imports" => {
                config.allow_unused_imports = true;
                true
            }
            other => db.declared_allows(crate_id).contains(other),
        },
    );
    config
}

/// Processes the feature attribute kind.
fn process_feature_attr_kind<'db>(
    db: &'db dyn Database,
    syntax: &impl QueryAttrs<'db>,
    attr: &'db str,
    diagnostic_kind: impl Fn() -> SemanticDiagnosticKind<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
    mut process: impl FnMut(&ast::Expr<'db>) -> bool,
) {
    for attr_syntax in syntax.query_attr(db, attr) {
        let attr = attr_syntax.structurize(db);
        let success = (|| {
            let [arg] = &attr.args[..] else {
                return None;
            };
            let value = try_extract_matches!(&arg.variant, AttributeArgVariant::Unnamed)?;
            process(value).then_some(())
        })()
        .is_none();
        if success {
            diagnostics.report(attr.args_stable_ptr.untyped(), diagnostic_kind());
        }
    }
}

/// Extracts the allowed features of an element, considering its parent modules as well as its
/// attributes.
pub fn feature_config_from_item_and_parent_modules<'db>(
    db: &'db dyn SemanticGroup,
    element_id: &impl LanguageElementId<'db>,
    syntax: &impl QueryAttrs<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
) -> FeatureConfig<'db> {
    let defs_db = db;
    let mut current_module_id = element_id.parent_module(defs_db);
    let crate_id = current_module_id.owning_crate(defs_db);
    let mut config_stack = vec![feature_config_from_ast_item(db, crate_id, syntax, diagnostics)];
    let mut config = loop {
        match current_module_id {
            ModuleId::CrateRoot(crate_id) => {
                let settings = db
                    .crate_config(crate_id)
                    .map(|config| config.settings.edition.ignore_visibility())
                    .unwrap_or_else(|| default_crate_settings(db).edition.ignore_visibility());
                break FeatureConfig {
                    allowed_features: OrderedHashSet::default(),
                    allow_deprecated: false,
                    allow_unused_imports: settings,
                };
            }
            ModuleId::Submodule(id) => {
                current_module_id = id.parent_module(defs_db);
                let module = &current_module_id.module_data(db).unwrap().submodules(db)[&id];
                // TODO(orizi): Add parent module diagnostics.
                let ignored = &mut SemanticDiagnostics::default();
                config_stack.push(feature_config_from_ast_item(db, crate_id, module, ignored));
            }
            ModuleId::MacroCall { id: macro_call_id, generated_file_id: _ } => {
                current_module_id = macro_call_id.parent_module(db);
            }
        }
    };
    for module_config in config_stack.into_iter().rev() {
        config.override_with(module_config);
    }
    config
}
