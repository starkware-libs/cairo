use std::iter::Peekable;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GenericKind, GenericParamId, GenericTypeId, ImplDefId, LanguageElementId, ModuleId,
    ModuleItemId, TopLevelLanguageElementId, TraitId, TraitItemId, UseId, VariantId,
};
use cairo_lang_diagnostics::{Maybe, skip_diagnostic};
use cairo_lang_filesystem::db::{
    CORELIB_CRATE_NAME, CrateSettings, FilesGroup, default_crate_settings,
};
use cairo_lang_filesystem::ids::{CodeMapping, CrateId, CrateLongId, SmolStrId};
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::consts::DEPRECATED_ATTR;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, extract_matches, require, try_extract_matches};
pub use item::{ResolvedConcreteItem, ResolvedGenericItem};
use itertools::Itertools;
use salsa::Database;
use syntax::node::TypedStablePtr;
use syntax::node::helpers::QueryAttrs;

use crate::corelib::{CorelibSemantic, core_submodule, get_submodule};
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{
    ComputationContext, Environment, ExpansionOffset, compute_expr_semantic,
    get_statement_item_by_name,
};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::{Inference, InferenceData, InferenceId};
use crate::items::constant::{
    ConstValue, ConstantSemantic, ImplConstantId, resolve_const_expr_and_evaluate,
};
use crate::items::enm::{EnumSemantic, SemanticEnumEx};
use crate::items::feature_kind::{
    FeatureConfig, FeatureConfigRestore, FeatureKind, HasFeatureKind, feature_config_from_ast_item,
    feature_config_from_item_and_parent_modules,
};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::generics::generic_params_to_args;
use crate::items::imp::{
    ConcreteImplId, ConcreteImplLongId, DerefInfo, ImplImplId, ImplLongId, ImplLookupContext,
    ImplLookupContextId, ImplSemantic,
};
use crate::items::impl_alias::ImplAliasSemantic;
use crate::items::macro_call::MacroCallSemantic;
use crate::items::module::{ModuleItemInfo, ModuleSemantic};
use crate::items::module_type_alias::ModuleTypeAliasSemantic;
use crate::items::trt::{
    ConcreteTraitConstantLongId, ConcreteTraitGenericFunctionLongId, ConcreteTraitId,
    ConcreteTraitImplLongId, ConcreteTraitLongId, ConcreteTraitTypeId, TraitSemantic,
};
use crate::items::us::{UseAsPathSegments, UseSemantic, get_use_path_segments};
use crate::items::{TraitOrImplContext, visibility};
use crate::keyword::{
    CRATE_KW, MACRO_CALL_SITE, MACRO_DEF_SITE, SELF_PARAM_KW, SELF_TYPE_KW, SUPER_KW,
};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{
    ConcreteEnumLongId, ImplTypeId, TypesSemantic, are_coupons_enabled, resolve_type,
};
use crate::{
    ConcreteFunction, ConcreteTypeId, ConcreteVariant, FunctionId, FunctionLongId,
    GenericArgumentId, GenericParam, Member, Mutability, TypeId, TypeLongId,
};

#[cfg(test)]
mod test;

mod item;

// Remove when this becomes an actual crate.
const STARKNET_CRATE_NAME: &str = "starknet";

/// Lookback maps for item resolving. Can be used to quickly check what is the semantic resolution
/// of any path segment.
#[derive(Clone, Default, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct ResolvedItems<'db> {
    pub concrete: UnorderedHashMap<ast::TerminalIdentifierPtr<'db>, ResolvedConcreteItem<'db>>,
    pub generic: UnorderedHashMap<ast::TerminalIdentifierPtr<'db>, ResolvedGenericItem<'db>>,
}
impl<'db> ResolvedItems<'db> {
    // Relates a path segment to a ResolvedConcreteItem, and adds to a resolved_items map. This will
    // be used in "Go to definition".
    pub fn mark_concrete(
        &mut self,
        db: &'db dyn Database,
        segment: &syntax::node::ast::PathSegment<'db>,
        resolved_item: ResolvedConcreteItem<'db>,
    ) -> ResolvedConcreteItem<'db> {
        let identifier = segment.identifier_ast(db);
        if let Some(generic_item) = resolved_item.generic(db) {
            // Mark the generic item as well, for language server resolved_items.
            self.generic.insert(identifier.stable_ptr(db), generic_item);
        }
        self.concrete.insert(identifier.stable_ptr(db), resolved_item.clone());
        resolved_item
    }
    // Relates a path segment to a ResolvedGenericItem, and adds to a resolved_items map. This will
    // be used in "Go to definition".
    pub fn mark_generic(
        &mut self,
        db: &'db dyn Database,
        segment: &syntax::node::ast::PathSegment<'db>,
        resolved_item: ResolvedGenericItem<'db>,
    ) -> ResolvedGenericItem<'db> {
        let identifier = segment.identifier_ast(db);
        self.generic.insert(identifier.stable_ptr(db), resolved_item.clone());
        resolved_item
    }
}

/// The enriched members of a type, including direct members of structs, as well as members of
/// targets of `Deref` and `DerefMut` of the type.
#[derive(Debug, PartialEq, Eq, DebugWithDb, Clone, salsa::Update)]
#[debug_db(dyn Database)]
pub struct EnrichedMembers<'db> {
    /// A map from member names to their semantic representation and the number of deref operations
    /// needed to access them.
    pub members: OrderedHashMap<SmolStrId<'db>, (Member<'db>, usize)>,
    /// The sequence of deref needed to access the members.
    pub deref_chain: Arc<Vec<DerefInfo<'db>>>,
    // The number of derefs that were explored.
    pub explored_derefs: usize,
}
impl<'db> EnrichedMembers<'db> {
    /// Returns `EnrichedTypeMemberAccess` for a single member if exists.
    pub fn get_member(&self, name: SmolStrId<'db>) -> Option<EnrichedTypeMemberAccess<'db>> {
        let (member, n_derefs) = self.members.get(&name)?;
        Some(EnrichedTypeMemberAccess {
            member: member.clone(),
            deref_functions: self
                .deref_chain
                .iter()
                .map(|deref_info| (deref_info.function_id, deref_info.self_mutability))
                .take(*n_derefs)
                .collect(),
        })
    }
}

/// The enriched member of a type, including the member itself and the deref functions needed to
/// access it.
pub struct EnrichedTypeMemberAccess<'db> {
    /// The member itself.
    pub member: Member<'db>,
    /// The sequence of deref functions needed to access the member.
    pub deref_functions: Vec<(FunctionId<'db>, Mutability)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn Database)]
enum MacroContextModifier {
    /// The path is resolved in the macro definition site.
    DefSite,
    /// The path is resolved in the macro call site.
    CallSite,
    /// No modifier, the path is resolved in the current resolver context.
    None,
}

#[derive(Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct ResolverData<'db> {
    /// Current module in which to resolve the path.
    pub module_id: ModuleId<'db>,
    /// Named generic parameters accessible to the resolver.
    generic_param_by_name: OrderedHashMap<SmolStrId<'db>, GenericParamId<'db>>,
    /// All generic parameters accessible to the resolver.
    pub generic_params: Vec<GenericParamId<'db>>,
    /// The enriched members per type and its mutability in the resolver context.
    pub type_enriched_members: OrderedHashMap<(TypeId<'db>, bool), EnrichedMembers<'db>>,
    /// Lookback map for resolved identifiers in path. Used in "Go to definition".
    pub resolved_items: ResolvedItems<'db>,
    /// Inference data for the resolver.
    pub inference_data: InferenceData<'db>,
    /// The trait/impl context the resolver is currently in. Used to resolve "Self::" paths.
    pub trait_or_impl_ctx: TraitOrImplContext<'db>,
    /// The configuration of allowed features.
    pub feature_config: FeatureConfig<'db>,
    /// The set of used `use` items in the current context.
    pub used_uses: OrderedHashSet<UseId<'db>>,
}
impl<'db> ResolverData<'db> {
    pub fn new(module_id: ModuleId<'db>, inference_id: InferenceId<'db>) -> Self {
        Self {
            module_id,
            generic_param_by_name: Default::default(),
            generic_params: Default::default(),
            type_enriched_members: Default::default(),
            resolved_items: Default::default(),
            inference_data: InferenceData::new(inference_id),
            trait_or_impl_ctx: TraitOrImplContext::None,
            feature_config: Default::default(),
            used_uses: Default::default(),
        }
    }
    pub fn clone_with_inference_id(
        &self,
        db: &'db dyn Database,
        inference_id: InferenceId<'db>,
    ) -> Self {
        Self {
            module_id: self.module_id,
            generic_param_by_name: self.generic_param_by_name.clone(),
            generic_params: self.generic_params.clone(),
            type_enriched_members: self.type_enriched_members.clone(),
            resolved_items: self.resolved_items.clone(),
            inference_data: self.inference_data.clone_with_inference_id(db, inference_id),
            trait_or_impl_ctx: self.trait_or_impl_ctx,
            feature_config: self.feature_config.clone(),
            used_uses: self.used_uses.clone(),
        }
    }
}

/// Resolving data needed for resolving macro expanded code in the correct context.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct ResolverMacroData<'db> {
    /// The module file id of the macro definition site. It is used if the path begins with
    /// `$defsite`.
    pub defsite_module_id: ModuleId<'db>,
    /// The module file id of the macro call site. Items are resolved in this context in two cases:
    /// 1. The path begins with `$callsite`.
    /// 2. The path was supplied as a macro argument. In other words, the path is an expansion of a
    ///    placeholder and is not a part of the macro expansion template.
    pub callsite_module_id: ModuleId<'db>,
    /// This is the mappings of the macro expansion. It is used to determine if a part of the
    /// code came from a macro argument or from the macro expansion template.
    pub expansion_mappings: Arc<[CodeMapping]>,
    /// The parent macro data. Exists in case of a macro calling another macro, and is used if we
    /// climb to the callsite environment.
    pub parent_macro_call_data: Option<Arc<ResolverMacroData<'db>>>,
}

/// Information for resolving a path in a macro context.
struct MacroResolutionInfo<'db> {
    /// The module where the resolved path is defined.
    base: ModuleId<'db>,
    /// The macro call data of the current resolution.
    data: Option<Arc<ResolverMacroData<'db>>>,
    /// The macro context modifier.
    modifier: MacroContextModifier,
}
impl<'db> MacroResolutionInfo<'db> {
    fn from_resolver(resolver: &Resolver<'db>) -> Self {
        Self {
            base: resolver.data.module_id,
            data: resolver.macro_call_data.clone(),
            modifier: MacroContextModifier::None,
        }
    }
}

/// Resolves paths semantically.
pub struct Resolver<'db> {
    db: &'db dyn Database,
    pub data: ResolverData<'db>,
    /// The resolving context for macro related resolving. Should be `Some` only if the current
    /// code is an expansion of a macro.
    pub macro_call_data: Option<Arc<ResolverMacroData<'db>>>,
    /// If true, allow resolution of path through the module definition, without a modifier.
    /// Should be true only within plugin macros generated code, or item macro call generated code.
    pub default_module_allowed: bool,
    pub owning_crate_id: CrateId<'db>,
    pub settings: CrateSettings,
}
impl<'db> Deref for Resolver<'db> {
    type Target = ResolverData<'db>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for Resolver<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
impl<'db> Resolver<'db> {
    /// Extracts the allowed node from the syntax, and sets it as the allowed features of the
    /// resolver.
    pub fn set_feature_config(
        &mut self,
        element_id: &impl LanguageElementId<'db>,
        syntax: &impl QueryAttrs<'db>,
        diagnostics: &mut SemanticDiagnostics<'db>,
    ) {
        self.feature_config =
            feature_config_from_item_and_parent_modules(self.db, element_id, syntax, diagnostics);
    }

    /// Extends the current feature config with the contents of those extracted from the given item,
    /// returns the original feature config for restoring it later.
    /// IMPORTANT: don't forget to call `restore_feature_config`!
    pub fn extend_feature_config_from_item(
        &mut self,
        db: &'db dyn Database,
        crate_id: CrateId<'db>,
        diagnostics: &mut SemanticDiagnostics<'db>,
        item: &impl QueryAttrs<'db>,
    ) -> FeatureConfigRestore<'db> {
        self.data.feature_config.override_with(feature_config_from_ast_item(
            db,
            crate_id,
            item,
            diagnostics,
        ))
    }

    /// Restores the feature config to its state before [Self::extend_feature_config_from_item],
    /// using the restoration state returned by that method.
    pub fn restore_feature_config(&mut self, restore: FeatureConfigRestore<'db>) {
        self.data.feature_config.restore(restore);
    }
}

pub enum ResolutionContext<'a, 'mt> {
    /// Default resolution.
    Default,
    /// The resolution is of a module item.
    ModuleItem(ModuleItemId<'a>),
    /// The resolution is in a statement environment.
    Statement(&'mt mut Environment<'a>),
}

/// The result of resolving an item using `use *` imports.
enum UseStarResult<'db> {
    /// A unique path was found, considering only the `use *` imports.
    UniquePathFound(ModuleItemInfo<'db>),
    /// The path is ambiguous, considering only the `use *` imports.
    AmbiguousPath(Vec<ModuleItemId<'db>>),
    /// The path was not found, considering only the `use *` imports.
    PathNotFound,
    /// Item is not visible in the current module, considering only the `use *` imports.
    ItemNotVisible(ModuleItemId<'db>, Vec<ModuleId<'db>>),
}

/// A trait for things that can be interpreted as a path of segments.
pub trait AsSegments<'db> {
    fn to_segments(self, db: &'db dyn Database) -> Vec<ast::PathSegment<'db>>;
    /// Returns placeholder marker `$` if the path prefixed with one, indicating a resolver site
    /// modifier.
    fn placeholder_marker(&self, db: &'db dyn Database) -> Option<ast::TerminalDollar<'db>>;
    /// The offset of the path in the file.
    fn offset(&self, db: &'db dyn Database) -> Option<TextOffset>;
}
impl<'db> AsSegments<'db> for &ast::ExprPath<'db> {
    fn to_segments(self, db: &'db dyn Database) -> Vec<ast::PathSegment<'db>> {
        self.segments(db).elements_vec(db)
    }
    fn placeholder_marker(&self, db: &'db dyn Database) -> Option<ast::TerminalDollar<'db>> {
        match self.dollar(db) {
            ast::OptionTerminalDollar::Empty(_) => None,
            ast::OptionTerminalDollar::TerminalDollar(dollar) => Some(dollar),
        }
    }

    fn offset(&self, db: &'db dyn Database) -> Option<TextOffset> {
        Some(self.as_syntax_node().offset(db))
    }
}
impl<'db> AsSegments<'db> for Vec<ast::PathSegment<'db>> {
    fn to_segments(self, _: &'db dyn Database) -> Vec<ast::PathSegment<'db>> {
        self
    }
    fn placeholder_marker(&self, _: &'db dyn Database) -> Option<ast::TerminalDollar<'db>> {
        // A dollar can prefix only the first segment of a path, thus irrelevant to a list of
        // segments.
        None
    }
    fn offset(&self, db: &'db dyn Database) -> Option<TextOffset> {
        self.first().map(|segment| segment.as_syntax_node().offset(db))
    }
}
impl<'db> AsSegments<'db> for UseAsPathSegments<'db> {
    fn to_segments(self, _: &'db dyn Database) -> Vec<ast::PathSegment<'db>> {
        self.segments
    }

    fn placeholder_marker(&self, _: &'db dyn Database) -> Option<ast::TerminalDollar<'db>> {
        self.is_placeholder.clone()
    }

    fn offset(&self, db: &'db dyn Database) -> Option<TextOffset> {
        if let Some(ref dollar) = self.is_placeholder {
            Some(dollar.as_syntax_node().offset(db))
        } else {
            self.segments.first().map(|segment| segment.as_syntax_node().offset(db))
        }
    }
}

impl<'db> Resolver<'db> {
    pub fn new(
        db: &'db dyn Database,
        module_id: ModuleId<'db>,
        inference_id: InferenceId<'db>,
    ) -> Self {
        Self::with_data(db, ResolverData::new(module_id, inference_id))
    }

    pub fn with_data(db: &'db dyn Database, data: ResolverData<'db>) -> Self {
        let owning_crate_id = data.module_id.owning_crate(db);
        let macro_call_data = match data.module_id {
            ModuleId::CrateRoot(_) | ModuleId::Submodule(_) => None,
            ModuleId::MacroCall { id, .. } => match db.priv_macro_call_data(id) {
                Ok(data) => Some(
                    ResolverMacroData {
                        defsite_module_id: data.defsite_module_id,
                        callsite_module_id: data.callsite_module_id,
                        expansion_mappings: data.expansion_mappings.clone(),
                        parent_macro_call_data: data.parent_macro_call_data,
                    }
                    .into(),
                ),
                Err(_) => None,
            },
        };
        Self {
            owning_crate_id,
            settings: db
                .crate_config(owning_crate_id)
                .map(|c| c.settings.clone())
                .unwrap_or_default(),
            db,
            data,
            default_module_allowed: macro_call_data.is_some(),
            macro_call_data,
        }
    }

    pub fn inference(&mut self) -> Inference<'db, '_> {
        self.data.inference_data.inference(self.db)
    }

    /// Adds a generic param to an existing resolver.
    /// This is required since a resolver needs to exist before resolving the generic params,
    /// and thus, they are added to the Resolver only after they are resolved.
    pub fn add_generic_param(&mut self, generic_param_id: GenericParamId<'db>) {
        self.generic_params.push(generic_param_id);
        if let Some(name) = generic_param_id.name(self.db) {
            self.generic_param_by_name.insert(name, generic_param_id);
        }
    }

    pub fn set_default_module_allowed(&mut self, default_module_allowed: bool) {
        self.default_module_allowed = default_module_allowed;
    }

    /// Return the module_id, with respect to the macro context modifier, see
    /// [`MacroContextModifier`].
    fn active_module_id(&self, info: &MacroResolutionInfo<'db>) -> ModuleId<'db> {
        if let Some(data) = &info.data {
            match info.modifier {
                MacroContextModifier::DefSite => data.defsite_module_id,
                MacroContextModifier::CallSite => data.callsite_module_id,
                MacroContextModifier::None => info.base,
            }
        } else {
            assert_eq!(info.modifier, MacroContextModifier::None);
            info.base
        }
    }

    /// Return the module_id, with respect to the macro context modifier, see
    /// [`MacroContextModifier`].
    fn try_get_active_module_id(&self, info: &MacroResolutionInfo<'db>) -> Option<ModuleId<'db>> {
        if let Some(data) = &info.data {
            match info.modifier {
                MacroContextModifier::DefSite => Some(data.defsite_module_id),
                MacroContextModifier::CallSite => Some(data.callsite_module_id),
                MacroContextModifier::None => (data.callsite_module_id != info.base
                    || self.default_module_allowed)
                    .then_some(info.base),
            }
        } else {
            Some(info.base)
        }
    }

    /// Returns the owning crate id of the active module file id, with respect to the macro context
    /// modifier, see [`MacroContextModifier`].
    fn active_owning_crate_id(&self, info: &MacroResolutionInfo<'db>) -> CrateId<'db> {
        self.active_module_id(info).owning_crate(self.db)
    }

    /// Returns the active settings of the owning crate, with respect to the macro context
    /// modifier, see [`MacroContextModifier`].
    fn active_settings(&self, info: &MacroResolutionInfo<'db>) -> &CrateSettings {
        self.db
            .crate_config(self.active_owning_crate_id(info))
            .map(|c| &c.settings)
            .unwrap_or_else(|| default_crate_settings(self.db))
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    /// Item not inside a statement.
    pub fn resolve_concrete_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        path: impl AsSegments<'db>,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem<'db>> {
        self.resolve_concrete_path_ex(diagnostics, path, item_type, ResolutionContext::Default)
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_concrete_path_ex(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        path: impl AsSegments<'db>,
        item_type: NotFoundItemType,
        ctx: ResolutionContext<'db, '_>,
    ) -> Maybe<ResolvedConcreteItem<'db>> {
        Resolution::new(self, diagnostics, path, item_type, ctx)?.full::<ResolvedConcreteItem<'db>>(
            ResolutionCallbacks {
                _phantom: PhantomData,
                first: |resolution| resolution.first_concrete(),
                next: |resolution, item, segment| resolution.next_concrete(item, segment),
                validate: |_, _| Ok(()),
                mark: |resolved_items: &mut ResolvedItems<'db>, db, segment, item| {
                    resolved_items.mark_concrete(db, segment, item);
                },
            },
        )
    }

    /// Resolves a generic item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_generic_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        path: impl AsSegments<'db>,
        item_type: NotFoundItemType,
        ctx: ResolutionContext<'db, '_>,
    ) -> Maybe<ResolvedGenericItem<'db>> {
        self.resolve_generic_path_inner(diagnostics, path, item_type, false, ctx)
    }

    /// Resolves a generic item from a `use` statement path.
    ///
    /// Useful for resolving paths from `use` items and statements.
    pub fn resolve_use_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        use_path: ast::UsePath<'db>,
        ctx: ResolutionContext<'db, '_>,
    ) -> Maybe<ResolvedGenericItem<'db>> {
        let mut segments = get_use_path_segments(self.db, use_path.clone())?;
        // Remove the last segment if it's `self`.
        if let Some(last) = segments.segments.last()
            && last.identifier(self.db).long(self.db) == SELF_PARAM_KW
        {
            // If the `self` keyword is used in a non-multi-use path, report an error.
            if use_path.as_syntax_node().parent_kind(self.db).unwrap() != SyntaxKind::UsePathList {
                diagnostics.report(use_path.stable_ptr(self.db), UseSelfNonMulti);
            }
            segments.segments.pop();
        }
        if segments.segments.is_empty() {
            return Err(diagnostics.report(use_path.stable_ptr(self.db), UseSelfEmptyPath));
        }
        self.resolve_generic_path(diagnostics, segments, NotFoundItemType::Identifier, ctx)
    }

    /// Resolves a generic item, given a concrete item path, while ignoring the generic args.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_generic_path_with_args(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        path: impl AsSegments<'db>,
        item_type: NotFoundItemType,
        ctx: ResolutionContext<'db, '_>,
    ) -> Maybe<ResolvedGenericItem<'db>> {
        self.resolve_generic_path_inner(diagnostics, path, item_type, true, ctx)
    }

    /// Resolves a generic item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    /// If `allow_generic_args` is true a path with generic args will be processed, but the generic
    /// params will be ignored.
    fn resolve_generic_path_inner(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        path: impl AsSegments<'db>,
        item_type: NotFoundItemType,
        allow_generic_args: bool,
        ctx: ResolutionContext<'db, '_>,
    ) -> Maybe<ResolvedGenericItem<'db>> {
        let validate = |diagnostics: &mut SemanticDiagnostics<'db>,
                        segment: &ast::PathSegment<'db>| {
            match segment {
                ast::PathSegment::WithGenericArgs(generic_args) if !allow_generic_args => {
                    Err(diagnostics.report(generic_args.stable_ptr(self.db), UnexpectedGenericArgs))
                }
                _ => Ok(()),
            }
        };
        Resolution::new(self, diagnostics, path, item_type, ctx)?.full::<ResolvedGenericItem<'_>>(
            ResolutionCallbacks {
                _phantom: PhantomData,
                first: |resolution| resolution.first_generic(allow_generic_args),
                next: |resolution, item, segment| resolution.next_generic(item, segment),
                validate,
                mark: |resolved_items, db, segment, item| {
                    resolved_items.mark_generic(db, segment, item);
                },
            },
        )
    }

    /// Specializes a ResolvedGenericItem that came from a ModuleItem.
    fn specialize_generic_module_item(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        identifier: &syntax::node::ast::TerminalIdentifier<'db>,
        generic_item: ResolvedGenericItem<'db>,
        generic_args_syntax: Option<Vec<ast::GenericArg<'db>>>,
    ) -> Maybe<ResolvedConcreteItem<'db>> {
        let db: &'db dyn Database = self.db;
        Ok(match generic_item {
            ResolvedGenericItem::GenericConstant(id) => {
                ResolvedConcreteItem::Constant(db.constant_const_value(id)?)
            }
            ResolvedGenericItem::Module(module_id) => {
                if generic_args_syntax.is_some() {
                    return Err(
                        diagnostics.report(identifier.stable_ptr(db), UnexpectedGenericArgs)
                    );
                }
                ResolvedConcreteItem::Module(module_id)
            }
            ResolvedGenericItem::GenericFunction(generic_function) => {
                ResolvedConcreteItem::Function(self.specialize_function(
                    diagnostics,
                    identifier.stable_ptr(db).untyped(),
                    generic_function,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericType(generic_type) => {
                ResolvedConcreteItem::Type(self.specialize_type(
                    diagnostics,
                    identifier.stable_ptr(db).untyped(),
                    generic_type,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericTypeAlias(module_type_alias_id) => {
                let ty = self.db.module_type_alias_resolved_type(module_type_alias_id)?;
                let generic_params =
                    self.db.module_type_alias_generic_params(module_type_alias_id)?;
                let generic_args = self.resolve_generic_args(
                    diagnostics,
                    GenericSubstitution::default(),
                    &generic_params,
                    &generic_args_syntax.unwrap_or_default(),
                    identifier.stable_ptr(db).untyped(),
                )?;
                ResolvedConcreteItem::Type(
                    GenericSubstitution::new(&generic_params, &generic_args).substitute(db, ty)?,
                )
            }
            ResolvedGenericItem::GenericImplAlias(impl_alias_id) => {
                let impl_id = db.impl_alias_resolved_impl(impl_alias_id)?;
                let generic_params = db.impl_alias_generic_params(impl_alias_id)?;
                let generic_args = self.resolve_generic_args(
                    diagnostics,
                    GenericSubstitution::default(),
                    &generic_params,
                    &generic_args_syntax.unwrap_or_default(),
                    identifier.stable_ptr(db).untyped(),
                )?;
                ResolvedConcreteItem::Impl(
                    GenericSubstitution::new(&generic_params, &generic_args)
                        .substitute(db, impl_id)?,
                )
            }
            ResolvedGenericItem::Trait(trait_id) => {
                ResolvedConcreteItem::Trait(self.specialize_trait(
                    diagnostics,
                    identifier.stable_ptr(db).untyped(),
                    trait_id,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::Impl(impl_def_id) => ResolvedConcreteItem::Impl(
                ImplLongId::Concrete(self.specialize_impl(
                    diagnostics,
                    identifier.stable_ptr(db).untyped(),
                    impl_def_id,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
                .intern(self.db),
            ),
            ResolvedGenericItem::Macro(macro_declaration_id) => {
                ResolvedConcreteItem::Macro(macro_declaration_id)
            }
            ResolvedGenericItem::Variant(var) => {
                ResolvedConcreteItem::Variant(self.specialize_variant(
                    diagnostics,
                    identifier.stable_ptr(db).untyped(),
                    var.id,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::Variable(_) => panic!("Variable is not a module item."),
            ResolvedGenericItem::TraitItem(id) => {
                panic!("`{}` is not a module item.", id.full_path(db))
            }
        })
    }

    /// Resolves an item using the `use *` imports.
    fn resolve_path_using_use_star(
        &mut self,
        module_id: ModuleId<'db>,
        ident: SmolStrId<'db>,
    ) -> UseStarResult<'db> {
        let mut item_info = None;
        let mut module_items_found: OrderedHashSet<ModuleItemId<'_>> = OrderedHashSet::default();
        let mut other_containing_modules = vec![];
        for (item_module_id, info) in self.db.module_imported_modules((), module_id).iter() {
            // Not checking the main module to prevent cycles.
            if *item_module_id == module_id {
                continue;
            }
            if let Some(inner_item_info) =
                self.resolve_item_in_module_or_expanded_macro(*item_module_id, ident)
            {
                if info.user_modules.iter().any(|user_module_id| {
                    self.is_item_visible(*item_module_id, &inner_item_info, *user_module_id)
                }) {
                    item_info = Some(inner_item_info.clone());
                    module_items_found.insert(inner_item_info.item_id);
                } else {
                    other_containing_modules.push(*item_module_id);
                }
            }
        }
        if module_items_found.len() > 1 {
            return UseStarResult::AmbiguousPath(module_items_found.iter().cloned().collect());
        }
        match item_info {
            Some(item_info) => UseStarResult::UniquePathFound(item_info),
            None => {
                for item_module_id in &other_containing_modules {
                    if let Some(inner_item_info) =
                        self.resolve_item_in_module_or_expanded_macro(*item_module_id, ident)
                    {
                        item_info = Some(inner_item_info.clone());
                        module_items_found.insert(inner_item_info.item_id);
                    }
                }
                if let Some(item_info) = item_info {
                    if module_items_found.len() > 1 {
                        UseStarResult::AmbiguousPath(module_items_found.iter().cloned().collect())
                    } else {
                        UseStarResult::ItemNotVisible(item_info.item_id, other_containing_modules)
                    }
                } else {
                    UseStarResult::PathNotFound
                }
            }
        }
    }

    /// Resolves an item in a module and falls back to the expanded macro code of the module.
    fn resolve_item_in_module_or_expanded_macro(
        &mut self,
        module_id: ModuleId<'db>,
        ident: SmolStrId<'db>,
    ) -> Option<ModuleItemInfo<'db>> {
        if let Ok(Some(info)) = self.db.module_item_info_by_name(module_id, ident) {
            self.insert_used_use(info.item_id);
            return Some(info);
        }
        let mut stack = vec![(module_id, false)];
        loop {
            let (module_id, expose) = stack.pop()?;
            if expose && let Ok(Some(info)) = self.db.module_item_info_by_name(module_id, ident) {
                self.insert_used_use(info.item_id);
                return Some(info);
            }
            if let Ok(macro_calls) = self.db.module_macro_calls_ids(module_id) {
                for macro_call in macro_calls {
                    if let Ok(macro_module_id) = self.db.macro_call_module_id(*macro_call) {
                        let expose = expose
                            || matches!(
                                macro_module_id,
                                ModuleId::MacroCall { is_expose: true, .. }
                            );
                        stack.push((macro_module_id, expose));
                    }
                }
            }
        }
    }

    /// Determines whether the first identifier of a path is a local item.
    pub fn determine_base_item_in_local_scope(
        &mut self,
        identifier: &ast::TerminalIdentifier<'db>,
    ) -> Option<ResolvedConcreteItem<'db>> {
        let db = self.db;
        let ident = identifier.text(db);

        // If a generic param with this name is found, use it.
        if let Some(generic_param_id) = self.data.generic_param_by_name.get(&ident) {
            let item = match generic_param_id.kind(self.db) {
                GenericKind::Type => ResolvedConcreteItem::Type(
                    TypeLongId::GenericParameter(*generic_param_id).intern(self.db),
                ),
                GenericKind::Const => ResolvedConcreteItem::Constant(
                    ConstValue::Generic(*generic_param_id).intern(self.db),
                ),
                GenericKind::Impl => ResolvedConcreteItem::Impl(
                    ImplLongId::GenericParameter(*generic_param_id).intern(self.db),
                ),
                GenericKind::NegImpl => return None,
            };
            return Some(item);
        }
        // TODO(spapini): Resolve local variables.

        None
    }

    pub fn prelude_submodule(&self) -> ModuleId<'db> {
        self.prelude_submodule_ex(&MacroResolutionInfo::from_resolver(self))
    }

    /// Returns the crate's `prelude` submodule.
    fn prelude_submodule_ex(&self, info: &MacroResolutionInfo<'db>) -> ModuleId<'db> {
        let active_settings = self.active_settings(info);
        let prelude_submodule_name = active_settings.edition.prelude_submodule_name(self.db);
        let core_prelude_submodule = core_submodule(self.db, SmolStrId::from(self.db, "prelude"));
        get_submodule(self.db, core_prelude_submodule, prelude_submodule_name).unwrap_or_else(
            || {
                panic!(
                    "expected prelude submodule `{}` not found in `core::prelude`.",
                    prelude_submodule_name.long(self.db)
                )
            },
        )
    }

    /// Specializes a trait.
    fn specialize_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
        trait_id: TraitId<'db>,
        generic_args: &[ast::GenericArg<'db>],
    ) -> Maybe<ConcreteTraitId<'db>> {
        // TODO(lior): Should we report diagnostic if `trait_generic_params` failed?
        let generic_params = self
            .db
            .trait_generic_params(trait_id)
            .map_err(|_| diagnostics.report(stable_ptr, UnknownTrait))?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(ConcreteTraitLongId { trait_id, generic_args }.intern(self.db))
    }

    /// Specializes an impl.
    fn specialize_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
        impl_def_id: ImplDefId<'db>,
        generic_args: &[ast::GenericArg<'db>],
    ) -> Maybe<ConcreteImplId<'db>> {
        // TODO(lior): Should we report diagnostic if `impl_def_generic_params` failed?
        let generic_params = self
            .db
            .impl_def_generic_params(impl_def_id)
            .map_err(|_| diagnostics.report(stable_ptr, UnknownImpl))?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(ConcreteImplLongId { impl_def_id, generic_args }.intern(self.db))
    }

    /// Specializes a variant.
    fn specialize_variant(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
        variant_id: VariantId<'db>,
        generic_args: &[ast::GenericArg<'db>],
    ) -> Maybe<ConcreteVariant<'db>> {
        let concrete_enum_id = ConcreteEnumLongId {
            enum_id: variant_id.enum_id(self.db),
            generic_args: self.resolve_generic_args(
                diagnostics,
                GenericSubstitution::default(),
                self.db.enum_generic_params(variant_id.enum_id(self.db))?,
                generic_args,
                stable_ptr,
            )?,
        }
        .intern(self.db);
        self.db.concrete_enum_variant(
            concrete_enum_id,
            &self.db.variant_semantic(variant_id.enum_id(self.db), variant_id)?,
        )
    }

    /// Specializes a generic function.
    pub fn specialize_function(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
        generic_function: GenericFunctionId<'db>,
        generic_args: &[ast::GenericArg<'db>],
    ) -> Maybe<FunctionId<'db>> {
        // TODO(lior): Should we report diagnostic if `impl_def_generic_params` failed?
        let generic_params = generic_function.generic_params(self.db)?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(FunctionLongId { function: ConcreteFunction { generic_function, generic_args } }
            .intern(self.db))
    }

    /// Specializes a generic type.
    pub fn specialize_type(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
        generic_type: GenericTypeId<'db>,
        generic_args: &[ast::GenericArg<'db>],
    ) -> Maybe<TypeId<'db>> {
        let generic_params = self
            .db
            .generic_type_generic_params(generic_type)
            .map_err(|_| diagnostics.report(stable_ptr, UnknownType))?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(TypeLongId::Concrete(ConcreteTypeId::new(self.db, generic_type, generic_args))
            .intern(self.db))
    }

    /// Returns the current impl lookup context.
    pub fn impl_lookup_context(&self) -> ImplLookupContextId<'db> {
        self.impl_lookup_context_ex(&MacroResolutionInfo::from_resolver(self))
    }

    /// Returns the current impl lookup context, with respect to the macro context modifier,
    /// see [`MacroContextModifier`].
    fn impl_lookup_context_ex(&self, info: &MacroResolutionInfo<'db>) -> ImplLookupContextId<'db> {
        let mut lookup_context = ImplLookupContext::new(
            self.active_module_id(info),
            self.generic_params.clone(),
            self.db,
        );

        if let TraitOrImplContext::Impl(impl_def_id) = &self.trait_or_impl_ctx {
            let Ok(generic_params) = self.db.impl_def_generic_params(*impl_def_id) else {
                return lookup_context.intern(self.db);
            };
            let generic_args = generic_params_to_args(generic_params, self.db);
            let impl_id: ConcreteImplId<'_> =
                ConcreteImplLongId { impl_def_id: *impl_def_id, generic_args }.intern(self.db);
            lookup_context.insert_impl(ImplLongId::Concrete(impl_id).intern(self.db), self.db);
        }
        lookup_context.intern(self.db)
    }

    /// Resolves generic arguments.
    /// For each generic argument, if the syntax is provided, it will be resolved by the inference.
    /// Otherwise, resolved by type.
    pub fn resolve_generic_args(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        mut substitution: GenericSubstitution<'db>,
        generic_params: &[GenericParam<'db>],
        generic_args_syntax: &[ast::GenericArg<'db>],
        stable_ptr: SyntaxStablePtrId<'db>,
    ) -> Maybe<Vec<GenericArgumentId<'db>>> {
        let mut resolved_args = vec![];
        let arg_syntax_per_param = self.get_arg_syntax_per_param(
            diagnostics,
            &generic_params.iter().map(|generic_param| generic_param.id()).collect_vec(),
            generic_args_syntax,
        )?;

        for generic_param in generic_params {
            let generic_param = substitution.substitute(self.db, generic_param.clone())?;
            let generic_arg = self.resolve_generic_arg(
                &generic_param,
                arg_syntax_per_param
                    .get(&generic_param.id())
                    .and_then(|arg_syntax| {
                        if let ast::GenericArgValue::Expr(expr) = arg_syntax {
                            Some(expr.expr(self.db))
                        } else {
                            None
                        }
                    })
                    .as_ref(),
                stable_ptr,
                diagnostics,
            )?;
            resolved_args.push(generic_arg);
            substitution.insert(generic_param.id(), generic_arg);
        }

        Ok(resolved_args)
    }

    /// Returns a map of generic param id -> its assigned arg syntax.
    pub fn get_arg_syntax_per_param(
        &self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        generic_params: &[GenericParamId<'db>],
        generic_args_syntax: &[ast::GenericArg<'db>],
    ) -> Maybe<UnorderedHashMap<GenericParamId<'db>, ast::GenericArgValue<'db>>> {
        let db = self.db;
        let mut arg_syntax_per_param =
            UnorderedHashMap::<GenericParamId<'_>, ast::GenericArgValue<'_>>::default();
        let mut last_named_arg_index = None;
        let generic_param_by_name = generic_params
            .iter()
            .enumerate()
            .filter_map(|(i, param)| Some((param.name(self.db)?, (i, param))))
            .collect::<UnorderedHashMap<_, _>>();
        for (idx, generic_arg_syntax) in generic_args_syntax.iter().enumerate() {
            match generic_arg_syntax {
                ast::GenericArg::Named(arg_syntax) => {
                    let name = arg_syntax.name(db).text(db);
                    let Some((index, generic_param_id)) = generic_param_by_name.get(&name) else {
                        return Err(diagnostics
                            .report(arg_syntax.stable_ptr(db), UnknownGenericParam(name)));
                    };
                    if let Some(prev_index) = last_named_arg_index
                        && prev_index > index
                    {
                        return Err(diagnostics
                            .report(arg_syntax.stable_ptr(db), GenericArgOutOfOrder(name)));
                    }
                    last_named_arg_index = Some(index);
                    if arg_syntax_per_param
                        .insert(**generic_param_id, arg_syntax.value(db))
                        .is_some()
                    {
                        return Err(diagnostics
                            .report(arg_syntax.stable_ptr(db), GenericArgDuplicate(name)));
                    }
                }
                ast::GenericArg::Unnamed(arg_syntax) => {
                    if last_named_arg_index.is_some() {
                        return Err(diagnostics
                            .report(arg_syntax.stable_ptr(db), PositionalGenericAfterNamed));
                    }
                    let generic_param = generic_params.get(idx).ok_or_else(|| {
                        diagnostics.report(
                            arg_syntax.stable_ptr(db),
                            TooManyGenericArguments {
                                expected: generic_params.len(),
                                actual: generic_args_syntax.len(),
                            },
                        )
                    })?;
                    assert_eq!(
                        arg_syntax_per_param.insert(*generic_param, arg_syntax.value(db)),
                        None,
                        "Unexpected duplication in ordered params."
                    );
                }
            }
        }
        Ok(arg_syntax_per_param)
    }

    /// Resolves a generic argument.
    /// If no syntax Expr is provided, inference will be used.
    /// If a syntax Expr is provided, it will be resolved by type.
    fn resolve_generic_arg(
        &mut self,
        generic_param: &GenericParam<'db>,
        generic_arg_syntax_opt: Option<&ast::Expr<'db>>,
        stable_ptr: SyntaxStablePtrId<'db>,
        diagnostics: &mut SemanticDiagnostics<'db>,
    ) -> Result<GenericArgumentId<'db>, cairo_lang_diagnostics::DiagnosticAdded> {
        let Some(generic_arg_syntax) = generic_arg_syntax_opt else {
            let lookup_context = self.impl_lookup_context();
            let inference = &mut self.data.inference_data.inference(self.db);
            return inference
                .infer_generic_arg(generic_param, lookup_context, Some(stable_ptr))
                .map_err(|err_set| {
                    inference.report_on_pending_error(err_set, diagnostics, stable_ptr)
                });
        };
        let db = self.db;
        Ok(match generic_param {
            GenericParam::Type(_) => {
                GenericArgumentId::Type(resolve_type(db, diagnostics, self, generic_arg_syntax))
            }
            GenericParam::Const(const_param) => {
                // TODO(spapini): Currently no bound checks are performed. Move literal validation
                // to inference finalization and use inference here. This will become more relevant
                // when we support constant expressions, which need inference.
                let mut ctx = ComputationContext::new_global(db, diagnostics, self);
                let value = compute_expr_semantic(&mut ctx, generic_arg_syntax);
                GenericArgumentId::Constant(resolve_const_expr_and_evaluate(
                    db,
                    &mut ctx,
                    &value,
                    generic_arg_syntax.stable_ptr(db).untyped(),
                    const_param.ty,
                    false,
                ))
            }

            GenericParam::Impl(param) => {
                let expr_path = try_extract_matches!(generic_arg_syntax, ast::Expr::Path)
                    .ok_or_else(|| {
                        diagnostics.report(generic_arg_syntax.stable_ptr(db), UnknownImpl)
                    })?;
                let resolved_impl = match self.resolve_concrete_path(
                    diagnostics,
                    expr_path,
                    NotFoundItemType::Impl,
                )? {
                    ResolvedConcreteItem::Impl(resolved_impl) => resolved_impl,
                    ResolvedConcreteItem::SelfTrait(concrete_trait_id) => {
                        ImplLongId::SelfImpl(concrete_trait_id).intern(self.db)
                    }
                    _ => {
                        return Err(
                            diagnostics.report(generic_arg_syntax.stable_ptr(db), UnknownImpl)
                        );
                    }
                };
                let impl_def_concrete_trait = self.db.impl_concrete_trait(resolved_impl)?;
                let expected_concrete_trait = param.concrete_trait?;
                if let Err(err_set) = self
                    .inference()
                    .conform_traits(impl_def_concrete_trait, expected_concrete_trait)
                {
                    let diag_added = diagnostics.report(
                        generic_arg_syntax.stable_ptr(db),
                        TraitMismatch {
                            expected_trt: expected_concrete_trait,
                            actual_trt: impl_def_concrete_trait,
                        },
                    );
                    self.inference().consume_reported_error(err_set, diag_added);
                } else {
                    for (trait_ty, ty1) in param.type_constraints.iter() {
                        let ty0 = TypeLongId::ImplType(ImplTypeId::new(
                            resolved_impl,
                            *trait_ty,
                            self.db,
                        ))
                        .intern(self.db);
                        let _ = self.inference().conform_ty(ty0, *ty1).map_err(|err_set| {
                            self.inference().report_on_pending_error(
                                err_set,
                                diagnostics,
                                stable_ptr,
                            )
                        });
                    }
                }
                GenericArgumentId::Impl(resolved_impl)
            }
            GenericParam::NegImpl(_) => {
                return Err(
                    diagnostics.report(generic_arg_syntax.stable_ptr(db), ArgPassedToNegativeImpl)
                );
            }
        })
    }

    /// Should visibility checks not actually happen for lookups in this module.
    // TODO(orizi): Remove this check when performing a major Cairo update.
    pub fn ignore_visibility_checks(&self, module_id: ModuleId<'db>) -> bool {
        self.ignore_visibility_checks_ex(module_id, &MacroResolutionInfo::from_resolver(self))
    }

    /// Same as [Self::ignore_visibility_checks], but takes into account the current macro context
    /// modifier.
    fn ignore_visibility_checks_ex(
        &self,
        module_id: ModuleId<'db>,
        info: &MacroResolutionInfo<'db>,
    ) -> bool {
        let module_crate = module_id.owning_crate(self.db);
        let module_edition =
            self.db.crate_config(module_crate).map(|c| c.settings.edition).unwrap_or_default();
        module_edition.ignore_visibility()
            || self.active_settings(info).edition.ignore_visibility()
                && module_crate == self.db.core_crate()
    }

    /// Validates whether a given item is allowed based on its feature kind.
    /// This function checks if the item's feature kind is allowed in the current
    /// configuration. If the item uses an unstable, deprecated, or internal feature
    /// that is not permitted, a corresponding diagnostic error is reported.
    pub fn validate_feature_constraints<T: HasFeatureKind<'db>>(
        &self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        identifier: &ast::TerminalIdentifier<'db>,
        item_info: &T,
    ) {
        let db = self.db;
        match item_info.feature_kind() {
            FeatureKind::Unstable { feature, note }
                if !self.data.feature_config.allowed_features.contains(feature) =>
            {
                diagnostics.report(
                    identifier.stable_ptr(db),
                    UnstableFeature { feature_name: *feature, note: *note },
                );
            }
            FeatureKind::Deprecated { feature, note }
                if !self
                    .data
                    .feature_config
                    .allowed_lints
                    .contains(&SmolStrId::from(self.db, DEPRECATED_ATTR))
                    && !self.data.feature_config.allowed_features.contains(feature) =>
            {
                diagnostics.report(
                    identifier.stable_ptr(db),
                    DeprecatedFeature { feature_name: *feature, note: *note },
                );
            }
            FeatureKind::Internal { feature, note }
                if !self.data.feature_config.allowed_features.contains(feature) =>
            {
                diagnostics.report(
                    identifier.stable_ptr(db),
                    InternalFeature { feature_name: *feature, note: *note },
                );
            }
            _ => {}
        }
    }

    /// Checks if an item is visible from the current module.
    pub fn is_item_visible(
        &self,
        containing_module_id: ModuleId<'db>,
        item_info: &ModuleItemInfo<'db>,
        user_module: ModuleId<'db>,
    ) -> bool {
        self.is_item_visible_ex(
            containing_module_id,
            item_info,
            user_module,
            &MacroResolutionInfo::from_resolver(self),
        )
    }

    /// Same as [Self::is_item_visible], but takes into account the current macro context modifier.
    fn is_item_visible_ex(
        &self,
        mut containing_module_id: ModuleId<'db>, // must never be a macro.
        item_info: &ModuleItemInfo<'db>,
        user_module: ModuleId<'db>,
        info: &MacroResolutionInfo<'db>,
    ) -> bool {
        let db = self.db;
        if containing_module_id == user_module {
            return true;
        }
        while let ModuleId::MacroCall { id, .. } = containing_module_id {
            containing_module_id = id.parent_module(self.db);
        }
        self.ignore_visibility_checks_ex(containing_module_id, info)
            || visibility::peek_visible_in(
                db,
                item_info.visibility,
                containing_module_id,
                user_module,
            )
    }

    /// Inserts an item into the used uses set, if it is indeed a use.
    pub fn insert_used_use(&mut self, item_id: ModuleItemId<'db>) {
        if let ModuleItemId::Use(use_id) = item_id {
            self.data.used_uses.insert(use_id);
        }
    }

    // TODO(yuval): on a breaking version change, consider changing warnings to errors.
    /// Warns about the use of a trait in a path inside the same trait or an impl of it, and the use
    /// of an impl in a path inside the same impl, additionally, converts a `Self` equivalent
    /// trait to be resolved as `Self`.
    /// That is, warns about using the actual path equivalent to `Self`, where `Self` can be used.
    fn handle_same_impl_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        specialized_item: &mut ResolvedConcreteItem<'db>,
        generic_args_syntax_slice: &[ast::GenericArg<'db>],
        segment_stable_ptr: SyntaxStablePtrId<'db>,
    ) {
        match *specialized_item {
            ResolvedConcreteItem::Trait(current_segment_concrete_trait) => {
                match self.trait_or_impl_ctx {
                    TraitOrImplContext::None => {}
                    TraitOrImplContext::Trait(ctx_trait) => {
                        if self
                            .warn_trait_in_same_trait(
                                diagnostics,
                                current_segment_concrete_trait.trait_id(self.db),
                                generic_args_syntax_slice,
                                ctx_trait,
                                segment_stable_ptr,
                            )
                            .is_err()
                        {
                            *specialized_item =
                                ResolvedConcreteItem::SelfTrait(current_segment_concrete_trait);
                        }
                    }
                    TraitOrImplContext::Impl(ctx_impl_def_id) => {
                        self.warn_trait_in_its_impl(
                            diagnostics,
                            current_segment_concrete_trait,
                            ctx_impl_def_id,
                            segment_stable_ptr,
                        )
                        .ok();
                    }
                };
            }
            ResolvedConcreteItem::Impl(current_segment_impl_id) => {
                if let TraitOrImplContext::Impl(ctx_impl) = self.trait_or_impl_ctx {
                    // A ResolvedConcreteItem::Impl returned by
                    // `specialize_generic_module_item` must be ImplLongId::Concrete.
                    let current_segment_concrete_impl_id = extract_matches!(
                        current_segment_impl_id.long(self.db),
                        ImplLongId::Concrete
                    );
                    self.warn_impl_in_same_impl(
                        diagnostics,
                        current_segment_concrete_impl_id.impl_def_id(self.db),
                        generic_args_syntax_slice,
                        ctx_impl,
                        segment_stable_ptr,
                    )
                    .ok();
                }
            }
            _ => {}
        };
    }

    /// Raises a warning diagnostic (and returns an error) if the segment describes an impl in the
    /// context of that impl (that is, could be expressed as `Self`).
    fn warn_impl_in_same_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        current_segment_impl_def_id: ImplDefId<'db>,
        current_segment_generic_args: &[ast::GenericArg<'db>],
        ctx_impl: ImplDefId<'db>,
        segment_stable_ptr: SyntaxStablePtrId<'db>,
    ) -> Maybe<()> {
        if current_segment_impl_def_id != ctx_impl {
            return Ok(());
        }

        let generic_params = self.db.impl_def_generic_params(ctx_impl)?;
        self.compare_segment_args_to_params(
            diagnostics,
            current_segment_generic_args,
            generic_params,
            segment_stable_ptr,
            ImplInImplMustBeExplicit,
            ImplItemForbiddenInTheImpl,
        )
    }

    /// Raises a warning diagnostic (and returns an error) if the segment is of a concrete trait in
    /// the context of an impl of that concrete trait (that is, could be expressed as `Self`).
    fn warn_trait_in_its_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        current_segment_concrete_trait_id: ConcreteTraitId<'db>,
        impl_ctx: ImplDefId<'db>,
        segment_stable_ptr: SyntaxStablePtrId<'db>,
    ) -> Maybe<()> {
        let ctx_impl_trait = self.db.impl_def_trait(impl_ctx)?;
        if current_segment_concrete_trait_id.trait_id(self.db) != ctx_impl_trait {
            return Ok(());
        }

        let ctx_impl_concrete_trait = self.db.impl_def_concrete_trait(impl_ctx)?;
        if ctx_impl_concrete_trait.generic_args(self.db)
            == current_segment_concrete_trait_id.generic_args(self.db)
        {
            return Err(diagnostics.report(segment_stable_ptr, TraitItemForbiddenInItsImpl));
        }
        Ok(())
    }

    /// Raises a warning diagnostic (and returns an error) if the segment describes a trait in the
    /// context of that trait (that is, could be expressed as `Self`).
    fn warn_trait_in_same_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        current_segment_trait_id: TraitId<'db>,
        current_segment_generic_args: &[ast::GenericArg<'db>],
        ctx_trait: TraitId<'db>,
        segment_stable_ptr: SyntaxStablePtrId<'db>,
    ) -> Maybe<()> {
        if current_segment_trait_id != ctx_trait {
            return Ok(());
        }

        let generic_params = self.db.trait_generic_params(ctx_trait)?;
        self.compare_segment_args_to_params(
            diagnostics,
            current_segment_generic_args,
            generic_params,
            segment_stable_ptr,
            TraitInTraitMustBeExplicit,
            TraitItemForbiddenInTheTrait,
        )
    }

    /// Check if the given generic arguments exactly match the given generic parameters.
    /// This is used to check if a path segment is forbidden in the context of the item referred to
    /// by the segment.
    ///
    /// Fails if not all arguments are explicitly specified or if they are all specified and exactly
    /// the same as the parameters.
    fn compare_segment_args_to_params(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        current_segment_generic_args: &[ast::GenericArg<'db>],
        generic_params: &[GenericParam<'db>],
        segment_stable_ptr: SyntaxStablePtrId<'db>,
        must_be_explicit_error: SemanticDiagnosticKind<'db>,
        item_forbidden_in_itself_explicit_error: SemanticDiagnosticKind<'db>,
    ) -> Maybe<()> {
        // This assumes the current segment item and the context items are equal. In this specific
        // case we disallow implicit arguments.
        if current_segment_generic_args.len() < generic_params.len() {
            return Err(diagnostics.report(segment_stable_ptr, must_be_explicit_error));
        }
        let resolved_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            generic_params,
            current_segment_generic_args,
            segment_stable_ptr,
        )?;

        if generic_params
            .iter()
            .zip_eq(resolved_args.iter())
            .all(|(gparam, garg)| gparam.as_arg(self.db) == *garg)
        {
            return Err(
                diagnostics.report(segment_stable_ptr, item_forbidden_in_itself_explicit_error)
            );
        }
        Ok(())
    }

    /// Specializes a ResolvedGenericItem that came from a Statement Environment.
    fn specialize_generic_statement_arg(
        &mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        segment: &ast::PathSegment<'db>,
        identifier: &ast::TerminalIdentifier<'db>,
        inner_generic_item: ResolvedGenericItem<'db>,
        generic_args_syntax: Option<Vec<ast::GenericArg<'db>>>,
    ) -> ResolvedConcreteItem<'db> {
        let segment_stable_ptr = segment.stable_ptr(self.db).untyped();
        let mut specialized_item = self
            .specialize_generic_module_item(
                diagnostics,
                identifier,
                inner_generic_item,
                generic_args_syntax.clone(),
            )
            .unwrap();
        self.handle_same_impl_trait(
            diagnostics,
            &mut specialized_item,
            &generic_args_syntax.unwrap_or_default(),
            segment_stable_ptr,
        );
        specialized_item
    }
}

/// Handles a macro context modifier in the path. Macro context modifiers are used to determine
/// the context in which a path, which was generated by a macro, should be resolved. The
/// supported modifiers are: `$defsite` and `$callsite`.
/// This function checks if the first segment is a valid macro context modifier and does two
/// things:
///  - Peels the modifier from the path segments.
///  - Returns the corresponding `MacroContextModifier`.
///
/// The function returns an error in the following cases:
///  - The modifier is not supported (i.e. starts with `$` but is not one of the supported
///    modifiers).
///  - The path after the modifier is empty.
fn handle_macro_context_modifier<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    segments: &mut Peekable<std::vec::IntoIter<ast::PathSegment<'db>>>,
) -> Maybe<MacroContextModifier> {
    if segments.len() == 1 {
        return Err(diagnostics.report_after(
            segments.next().unwrap().stable_ptr(db),
            EmptyPathAfterResolverModifier,
        ));
    }
    match segments.peek() {
        Some(ast::PathSegment::Simple(path_segment_simple)) => {
            let ident = path_segment_simple.ident(db);
            let ident_text = ident.text(db);
            match ident_text.long(db).as_str() {
                MACRO_DEF_SITE | MACRO_CALL_SITE => {
                    segments.next();
                    if ident_text.long(db) == MACRO_DEF_SITE {
                        Ok(MacroContextModifier::DefSite)
                    } else {
                        Ok(MacroContextModifier::CallSite)
                    }
                }
                _ => Err(diagnostics.report(
                    ident.stable_ptr(db),
                    UnknownResolverModifier { modifier: ident_text },
                )),
            }
        }
        _ => {
            // Empty path segment after a `$`, diagnostic was added above.
            Err(skip_diagnostic())
        }
    }
}

/// Resolves the segment if it's `Self`. Returns the Some(ResolvedConcreteItem) or Some(Err) if
/// segment == `Self` or None otherwise.
fn resolve_self_segment<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    identifier: &ast::TerminalIdentifier<'db>,
    trait_or_impl_ctx: &TraitOrImplContext<'db>,
) -> Option<Maybe<ResolvedConcreteItem<'db>>> {
    require(identifier.text(db).long(db) == SELF_TYPE_KW)?;
    Some(resolve_actual_self_segment(db, diagnostics, identifier, trait_or_impl_ctx))
}

/// Resolves the `Self` segment given that it's actually `Self`.
fn resolve_actual_self_segment<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    identifier: &ast::TerminalIdentifier<'db>,
    trait_or_impl_ctx: &TraitOrImplContext<'db>,
) -> Maybe<ResolvedConcreteItem<'db>> {
    match trait_or_impl_ctx {
        TraitOrImplContext::None => {
            Err(diagnostics.report(identifier.stable_ptr(db), SelfNotSupportedInContext))
        }
        TraitOrImplContext::Trait(trait_id) => {
            let generic_parameters = db.trait_generic_params(*trait_id)?;
            let concrete_trait_id = ConcreteTraitLongId {
                trait_id: *trait_id,
                generic_args: generic_params_to_args(generic_parameters, db),
            }
            .intern(db);
            Ok(ResolvedConcreteItem::SelfTrait(concrete_trait_id))
        }
        TraitOrImplContext::Impl(impl_def_id) => {
            let generic_parameters = db.impl_def_generic_params(*impl_def_id)?;
            let impl_id = ImplLongId::Concrete(
                ConcreteImplLongId {
                    impl_def_id: *impl_def_id,
                    generic_args: generic_params_to_args(generic_parameters, db),
                }
                .intern(db),
            );
            Ok(ResolvedConcreteItem::Impl(impl_id.intern(db)))
        }
    }
}

/// The base module or crate for the path resolving.
enum ResolvedBase<'db> {
    /// The base module is a module.
    Module(ModuleId<'db>),
    /// The base module is a crate.
    Crate(CrateId<'db>),
    /// The base module to address is the statement
    StatementEnvironment(ResolvedGenericItem<'db>),
    /// The item is imported using global use.
    FoundThroughGlobalUse { item_info: ModuleItemInfo<'db>, containing_module: ModuleId<'db> },
    /// The base module is ambiguous.
    Ambiguous(Vec<ModuleItemId<'db>>),
    /// The base module is inaccessible.
    ItemNotVisible(ModuleItemId<'db>, Vec<ModuleId<'db>>),
}

/// The callbacks to be used by `resolve_path_inner`.
struct ResolutionCallbacks<'db, 'a, ResolvedItem, First, Next, Validate, Mark>
where
    First: FnOnce(&mut Resolution<'db, 'a>) -> Maybe<ResolvedItem>,
    Next: FnMut(
        &mut Resolution<'db, 'a>,
        &ResolvedItem,
        &ast::PathSegment<'db>,
    ) -> Maybe<ResolvedItem>,
    Validate: FnMut(&mut SemanticDiagnostics<'db>, &ast::PathSegment<'db>) -> Maybe<()>,
    Mark: FnMut(
        &mut ResolvedItems<'db>,
        &'db dyn Database,
        &syntax::node::ast::PathSegment<'db>,
        ResolvedItem,
    ),
{
    /// Type for the resolved item pointed by the path segments.
    _phantom: PhantomData<(ResolvedItem, &'db (), &'a ())>,
    /// Resolves the first segment of a path.
    first: First,
    /// Given the current resolved item, resolves the next segment.
    next: Next,
    /// An additional validation to perform for each segment. If it fails, the whole resolution
    /// fails.
    validate: Validate,
    /// Marks the resolved item in the resolved items map.
    mark: Mark,
}

/// The state of a path resolution.
struct Resolution<'db, 'a> {
    /// The resolver that is resolving the path.
    resolver: &'a mut Resolver<'db>,
    /// The diagnostics to report errors to.
    diagnostics: &'a mut SemanticDiagnostics<'db>,
    /// The segments of the path to resolve.
    segments: Peekable<std::vec::IntoIter<ast::PathSegment<'db>>>,
    /// The type of item expected by the path.
    expected_item_type: NotFoundItemType,
    /// The context of the path resolution.
    resolution_context: ResolutionContext<'db, 'a>,
    /// The macro resolution info.
    macro_info: MacroResolutionInfo<'db>,
}
impl<'db, 'a> Resolution<'db, 'a> {
    /// Creates a new resolution, for resolving the given `path`.
    /// Panics if the given path is empty.
    /// May return with error if path had bad `$` usage.
    fn new(
        resolver: &'a mut Resolver<'db>,
        diagnostics: &'a mut SemanticDiagnostics<'db>,
        path: impl AsSegments<'db>,
        item_type: NotFoundItemType,
        resolution_context: ResolutionContext<'db, 'a>,
    ) -> Maybe<Self> {
        let db = resolver.db;
        let placeholder_marker = path.placeholder_marker(db);

        let mut cur_offset =
            ExpansionOffset::new(path.offset(db).expect("Trying to resolve an empty path."));
        let elements_vec = path.to_segments(db);
        let mut segments = elements_vec.into_iter().peekable();
        let mut cur_macro_call_data = resolver.macro_call_data.as_ref();
        let mut path_defining_module = resolver.data.module_id;
        // Climb up the macro call data while the current resolved path is being mapped to an
        // argument of a macro call.
        while let Some(macro_call_data) = &cur_macro_call_data {
            let Some(new_offset) = cur_offset.mapped(&macro_call_data.expansion_mappings) else {
                break;
            };
            path_defining_module = macro_call_data.callsite_module_id;
            cur_macro_call_data = macro_call_data.parent_macro_call_data.as_ref();
            cur_offset = new_offset;
        }
        let macro_call_data = cur_macro_call_data.cloned();

        let macro_context_modifier = if let Some(marker) = placeholder_marker {
            if macro_call_data.is_some() {
                handle_macro_context_modifier(db, diagnostics, &mut segments)?
            } else {
                return Err(diagnostics.report(marker.stable_ptr(db), DollarNotSupportedInContext));
            }
        } else {
            MacroContextModifier::None
        };
        let macro_info = MacroResolutionInfo {
            base: path_defining_module,
            data: macro_call_data,
            modifier: macro_context_modifier,
        };
        Ok(Resolution {
            resolver,
            diagnostics,
            segments,
            expected_item_type: item_type,
            resolution_context,
            macro_info,
        })
    }

    /// Resolves an item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    fn full<ResolvedItem: Clone>(
        mut self,
        mut callbacks: ResolutionCallbacks<
            'db,
            'a,
            ResolvedItem,
            impl FnOnce(&mut Resolution<'db, 'a>) -> Maybe<ResolvedItem>,
            impl FnMut(
                &mut Resolution<'db, 'a>,
                &ResolvedItem,
                &ast::PathSegment<'db>,
            ) -> Maybe<ResolvedItem>,
            impl FnMut(&mut SemanticDiagnostics<'db>, &ast::PathSegment<'db>) -> Maybe<()>,
            impl FnMut(
                &mut ResolvedItems<'db>,
                &'db dyn Database,
                &syntax::node::ast::PathSegment<'db>,
                ResolvedItem,
            ),
        >,
    ) -> Maybe<ResolvedItem>
    where
        'db: 'a,
    {
        // Find where the first segment lies in.
        let mut item: ResolvedItem = (callbacks.first)(&mut self)?;

        // Follow modules.
        while let Some(segment) = self.segments.next() {
            (callbacks.validate)(self.diagnostics, &segment)?;
            // `?` is ok here as the rest of the segments have no meaning if the current one can't
            // be resolved.
            item = (callbacks.next)(&mut self, &item, &segment)?;
            let db = self.resolver.db;
            (callbacks.mark)(&mut self.resolver.resolved_items, db, &segment, item.clone());
        }
        Ok(item)
    }

    /// Specializes the item found in the current segment, and checks its usability.
    fn specialize_generic_inner_item(
        &mut self,
        module_id: ModuleId<'db>,
        segment: &ast::PathSegment<'db>,
        identifier: &TerminalIdentifier<'db>,
        inner_item_info: ModuleItemInfo<'db>,
    ) -> Maybe<ResolvedConcreteItem<'db>> {
        let db = self.resolver.db;
        let generic_args_syntax = segment.generic_args(db);
        let segment_stable_ptr = segment.stable_ptr(db).untyped();
        self.validate_module_item_usability(module_id, identifier, &inner_item_info);
        self.resolver.insert_used_use(inner_item_info.item_id);
        let inner_generic_item =
            ResolvedGenericItem::from_module_item(db, inner_item_info.item_id)?;
        let mut specialized_item = self.resolver.specialize_generic_module_item(
            self.diagnostics,
            identifier,
            inner_generic_item.clone(),
            generic_args_syntax.clone(),
        )?;
        self.resolver
            .data
            .resolved_items
            .generic
            .insert(identifier.stable_ptr(db), inner_generic_item);
        self.resolver.handle_same_impl_trait(
            self.diagnostics,
            &mut specialized_item,
            &generic_args_syntax.unwrap_or_default(),
            segment_stable_ptr,
        );
        Ok(specialized_item)
    }

    /// Resolves the first segment of a concrete path.
    fn first_concrete(&mut self) -> Maybe<ResolvedConcreteItem<'db>> {
        if let Some(base_module) =
            self.try_handle_super_segments(|resolved_items, db, segment, module_id| {
                resolved_items.mark_concrete(db, segment, ResolvedConcreteItem::Module(module_id));
            })
        {
            return Ok(ResolvedConcreteItem::Module(base_module?));
        }

        let db = self.resolver.db;
        Ok(match self.segments.peek().unwrap() {
            syntax::node::ast::PathSegment::WithGenericArgs(generic_segment) => {
                let generics_stable_ptr = generic_segment.generic_args(db).stable_ptr(db);
                let identifier = generic_segment.ident(db);
                // Identifier with generic args cannot be a local item.
                match self.determine_base(&identifier)? {
                    ResolvedBase::Module(module_id) => ResolvedConcreteItem::Module(module_id),
                    ResolvedBase::Crate(_) => {
                        // Crates do not have generics.
                        return Err(self
                            .diagnostics
                            .report(generics_stable_ptr, UnexpectedGenericArgs));
                    }
                    ResolvedBase::StatementEnvironment(generic_item) => {
                        let segment = self.segments.next().unwrap();
                        let concrete_item = self.resolver.specialize_generic_statement_arg(
                            self.diagnostics,
                            &segment,
                            &identifier,
                            generic_item,
                            segment.generic_args(db),
                        );
                        self.resolver.resolved_items.mark_concrete(db, &segment, concrete_item)
                    }
                    ResolvedBase::FoundThroughGlobalUse {
                        item_info: inner_module_item,
                        containing_module: module_id,
                    } => {
                        let segment = self.segments.next().unwrap();

                        let concrete_item = self.specialize_generic_inner_item(
                            module_id,
                            &segment,
                            &identifier,
                            inner_module_item,
                        )?;
                        self.resolver.resolved_items.mark_concrete(db, &segment, concrete_item)
                    }
                    ResolvedBase::Ambiguous(module_items) => {
                        return Err(self
                            .diagnostics
                            .report(identifier.stable_ptr(db), AmbiguousPath(module_items)));
                    }
                    ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                        return Err(self.diagnostics.report(
                            identifier.stable_ptr(db),
                            ItemNotVisible(module_item_id, containing_modules),
                        ));
                    }
                }
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(db);

                if let Some(resolved_item) = resolve_self_segment(
                    db,
                    self.diagnostics,
                    &identifier,
                    &self.resolver.data.trait_or_impl_ctx,
                ) {
                    // The first segment is `Self`. Consume it and return.
                    return Ok(self.resolver.resolved_items.mark_concrete(
                        db,
                        &self.segments.next().unwrap(),
                        resolved_item?,
                    ));
                }

                if let Some(local_item) =
                    self.resolver.determine_base_item_in_local_scope(&identifier)
                {
                    self.resolver.resolved_items.mark_concrete(
                        db,
                        &self.segments.next().unwrap(),
                        local_item,
                    )
                } else {
                    match self.determine_base(&identifier)? {
                        // This item lies inside a module.
                        ResolvedBase::Module(module_id) => ResolvedConcreteItem::Module(module_id),
                        ResolvedBase::Crate(crate_id) => {
                            self.resolver.resolved_items.mark_concrete(
                                db,
                                &self.segments.next().unwrap(),
                                ResolvedConcreteItem::Module(ModuleId::CrateRoot(crate_id)),
                            )
                        }
                        ResolvedBase::StatementEnvironment(generic_item) => {
                            let segment = self.segments.next().unwrap();

                            let concrete_item = self.resolver.specialize_generic_statement_arg(
                                self.diagnostics,
                                &segment,
                                &identifier,
                                generic_item,
                                segment.generic_args(db),
                            );
                            self.resolver.resolved_items.mark_concrete(db, &segment, concrete_item)
                        }
                        ResolvedBase::FoundThroughGlobalUse {
                            item_info: inner_module_item,
                            containing_module: module_id,
                        } => {
                            let segment = self.segments.next().unwrap();
                            let concrete_item = self.specialize_generic_inner_item(
                                module_id,
                                &segment,
                                &identifier,
                                inner_module_item,
                            )?;
                            self.resolver.resolved_items.mark_concrete(db, &segment, concrete_item)
                        }
                        ResolvedBase::Ambiguous(module_items) => {
                            return Err(self
                                .diagnostics
                                .report(identifier.stable_ptr(db), AmbiguousPath(module_items)));
                        }
                        ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                            return Err(self.diagnostics.report(
                                identifier.stable_ptr(db),
                                ItemNotVisible(module_item_id, containing_modules),
                            ));
                        }
                    }
                }
            }
            // A diagnostic for the missing segment should have been reported from the syntax phase.
            syntax::node::ast::PathSegment::Missing(_) => return Err(skip_diagnostic()),
        })
    }
    /// Resolves the first segment of a generic path.
    /// If `allow_generic_args` is true the generic args will be ignored.
    fn first_generic(&mut self, allow_generic_args: bool) -> Maybe<ResolvedGenericItem<'db>> {
        if let Some(base_module) =
            self.try_handle_super_segments(|resolved_items, db, segment, module_id| {
                resolved_items.mark_generic(db, segment, ResolvedGenericItem::Module(module_id));
            })
        {
            return Ok(ResolvedGenericItem::Module(base_module?));
        }
        let db = self.resolver.db;
        Ok(match self.segments.peek().unwrap() {
            syntax::node::ast::PathSegment::WithGenericArgs(generic_segment) => {
                let generics_stable_ptr = generic_segment.generic_args(db).stable_ptr(db);
                if !allow_generic_args {
                    return Err(self
                        .diagnostics
                        .report(generics_stable_ptr, UnexpectedGenericArgs));
                }
                let identifier = generic_segment.ident(db);
                match self.determine_base(&identifier)? {
                    ResolvedBase::Module(module_id) => ResolvedGenericItem::Module(module_id),
                    ResolvedBase::Crate(_) => {
                        // Crates do not have generics.
                        return Err(self
                            .diagnostics
                            .report(generics_stable_ptr, UnexpectedGenericArgs));
                    }
                    ResolvedBase::StatementEnvironment(generic_item) => generic_item,
                    ResolvedBase::FoundThroughGlobalUse {
                        item_info: inner_module_item, ..
                    } => {
                        self.resolver.insert_used_use(inner_module_item.item_id);
                        let generic_item =
                            ResolvedGenericItem::from_module_item(db, inner_module_item.item_id)?;
                        self.resolver.resolved_items.mark_generic(
                            db,
                            &self.segments.next().unwrap(),
                            generic_item,
                        )
                    }
                    ResolvedBase::Ambiguous(module_items) => {
                        return Err(self
                            .diagnostics
                            .report(identifier.stable_ptr(db), AmbiguousPath(module_items)));
                    }
                    ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                        return Err(self.diagnostics.report(
                            identifier.stable_ptr(db),
                            ItemNotVisible(module_item_id, containing_modules),
                        ));
                    }
                }
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(db);
                match self.determine_base(&identifier)? {
                    // This item lies inside a module.
                    ResolvedBase::Module(module_id) => ResolvedGenericItem::Module(module_id),
                    ResolvedBase::Crate(crate_id) => self.resolver.resolved_items.mark_generic(
                        db,
                        &self.segments.next().unwrap(),
                        ResolvedGenericItem::Module(ModuleId::CrateRoot(crate_id)),
                    ),
                    ResolvedBase::StatementEnvironment(generic_item) => self
                        .resolver
                        .resolved_items
                        .mark_generic(db, &self.segments.next().unwrap(), generic_item),
                    ResolvedBase::FoundThroughGlobalUse {
                        item_info: inner_module_item, ..
                    } => {
                        self.resolver.insert_used_use(inner_module_item.item_id);
                        let generic_item =
                            ResolvedGenericItem::from_module_item(db, inner_module_item.item_id)?;
                        self.resolver.resolved_items.mark_generic(
                            db,
                            &self.segments.next().unwrap(),
                            generic_item,
                        )
                    }
                    ResolvedBase::Ambiguous(module_items) => {
                        return Err(self
                            .diagnostics
                            .report(identifier.stable_ptr(db), AmbiguousPath(module_items)));
                    }
                    ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                        return Err(self.diagnostics.report(
                            identifier.stable_ptr(db),
                            ItemNotVisible(module_item_id, containing_modules),
                        ));
                    }
                }
            }
            // A diagnostic for the missing segment should have been reported from the syntax phase.
            syntax::node::ast::PathSegment::Missing(_) => return Err(skip_diagnostic()),
        })
    }

    /// Handles `super::` initial segments, by removing them, and returning the valid module if
    /// exists. If there's none - returns None.
    /// If there are, but that's an invalid path, adds to diagnostics and returns `Some(Err)`.
    fn try_handle_super_segments(
        &mut self,
        mut mark: impl FnMut(
            &mut ResolvedItems<'db>,
            &'db dyn Database,
            &syntax::node::ast::PathSegment<'db>,
            ModuleId<'db>,
        ),
    ) -> Option<Maybe<ModuleId<'db>>> {
        let db = self.resolver.db;
        let mut curr = None;
        for segment in self.segments.peeking_take_while(|segment| match segment {
            ast::PathSegment::WithGenericArgs(_) | ast::PathSegment::Missing(_) => false,
            ast::PathSegment::Simple(simple) => simple.identifier(db).long(db) == SUPER_KW,
        }) {
            let module_id = match curr {
                Some(module_id) => module_id,
                None => {
                    if let Some(module_id) =
                        self.resolver.try_get_active_module_id(&self.macro_info)
                    {
                        module_id
                    } else {
                        return Some(Err(self
                            .diagnostics
                            .report(segment.stable_ptr(db), SuperUsedInMacroCallTopLevel)));
                    }
                }
            };
            match module_id {
                ModuleId::CrateRoot(_) => {
                    return Some(Err(self
                        .diagnostics
                        .report(segment.stable_ptr(db), SuperUsedInRootModule)));
                }
                ModuleId::Submodule(submodule_id) => {
                    let parent = submodule_id.parent_module(db);
                    mark(&mut self.resolver.resolved_items, db, &segment, parent);
                    curr = Some(parent);
                }
                ModuleId::MacroCall { .. } => {
                    return Some(Err(self
                        .diagnostics
                        .report(segment.stable_ptr(db), SuperUsedInMacroCallTopLevel)));
                }
            }
        }
        curr.map(Ok)
    }

    /// Resolves the inner item of a module, given the current segment of the path.
    fn module_inner_item(
        &mut self,
        module_id: &ModuleId<'db>,
        ident: SmolStrId<'db>,
        identifier: &TerminalIdentifier<'db>,
    ) -> Maybe<ModuleItemInfo<'db>> {
        let db = self.resolver.db;
        if let Some(info) =
            self.resolver.resolve_item_in_module_or_expanded_macro(*module_id, ident)
        {
            return Ok(info);
        }
        match self.resolver.resolve_path_using_use_star(*module_id, ident) {
            UseStarResult::UniquePathFound(item_info) => Ok(item_info),
            UseStarResult::AmbiguousPath(module_items) => {
                Err(self.diagnostics.report(identifier.stable_ptr(db), AmbiguousPath(module_items)))
            }
            UseStarResult::PathNotFound => {
                let item_type = if self.segments.len() == 0 {
                    self.expected_item_type
                } else {
                    NotFoundItemType::Identifier
                };
                Err(self.diagnostics.report(identifier.stable_ptr(db), PathNotFound(item_type)))
            }
            UseStarResult::ItemNotVisible(module_item_id, containing_modules) => {
                Err(self.diagnostics.report(
                    identifier.stable_ptr(db),
                    ItemNotVisible(module_item_id, containing_modules),
                ))
            }
        }
    }

    /// Given the current resolved item, resolves the next segment.
    fn next_concrete(
        &mut self,
        containing_item: &ResolvedConcreteItem<'db>,
        segment: &ast::PathSegment<'db>,
    ) -> Maybe<ResolvedConcreteItem<'db>> {
        let db = self.resolver.db;
        let identifier = &segment.identifier_ast(db);
        let generic_args_syntax = segment.generic_args(db);

        let ident = identifier.text(db);

        if identifier.text(db).long(db) == SELF_TYPE_KW {
            return Err(self.diagnostics.report(identifier.stable_ptr(db), SelfMustBeFirst));
        }

        match containing_item {
            ResolvedConcreteItem::Module(module_id) => {
                // Prefix `super` segments should be removed earlier. Middle `super` segments are
                // not allowed.
                if ident.long(db) == SUPER_KW {
                    return Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath));
                }
                let inner_item_info = self.module_inner_item(module_id, ident, identifier)?;

                self.specialize_generic_inner_item(*module_id, segment, identifier, inner_item_info)
            }
            ResolvedConcreteItem::Type(ty) => {
                if let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) = ty.long(db) {
                    let enum_id = concrete_enum_id.enum_id(db);
                    let variants = db.enum_variants(enum_id).map_err(|_| {
                        self.diagnostics.report(identifier.stable_ptr(db), UnknownEnum)
                    })?;
                    let variant_id = variants.get(&ident).ok_or_else(|| {
                        self.diagnostics.report(
                            identifier.stable_ptr(db),
                            NoSuchVariant { enum_id, variant_name: ident },
                        )
                    })?;
                    let variant = db.variant_semantic(enum_id, *variant_id)?;
                    let concrete_variant = db.concrete_enum_variant(*concrete_enum_id, &variant)?;
                    Ok(ResolvedConcreteItem::Variant(concrete_variant))
                } else {
                    Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath))
                }
            }
            ResolvedConcreteItem::SelfTrait(concrete_trait_id) => {
                let impl_id = ImplLongId::SelfImpl(*concrete_trait_id).intern(db);
                let Some(trait_item_id) =
                    db.trait_item_by_name(concrete_trait_id.trait_id(db), ident)?
                else {
                    return Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath));
                };
                if let Ok(Some(trait_item_info)) =
                    db.trait_item_info_by_name(concrete_trait_id.trait_id(db), ident)
                {
                    self.resolver.validate_feature_constraints(
                        self.diagnostics,
                        identifier,
                        &trait_item_info,
                    );
                }
                Ok(match trait_item_id {
                    TraitItemId::Function(trait_function_id) => {
                        ResolvedConcreteItem::Function(self.resolver.specialize_function(
                            self.diagnostics,
                            identifier.stable_ptr(db).untyped(),
                            GenericFunctionId::Impl(ImplGenericFunctionId {
                                impl_id,
                                function: trait_function_id,
                            }),
                            &generic_args_syntax.unwrap_or_default(),
                        )?)
                    }
                    TraitItemId::Type(trait_type_id) => ResolvedConcreteItem::Type(
                        TypeLongId::ImplType(ImplTypeId::new(impl_id, trait_type_id, db))
                            .intern(db),
                    ),
                    TraitItemId::Constant(trait_constant_id) => ResolvedConcreteItem::Constant(
                        ConstValue::ImplConstant(ImplConstantId::new(
                            impl_id,
                            trait_constant_id,
                            db,
                        ))
                        .intern(db),
                    ),
                    TraitItemId::Impl(trait_impl_id) => ResolvedConcreteItem::Impl(
                        ImplLongId::ImplImpl(ImplImplId::new(impl_id, trait_impl_id, db))
                            .intern(db),
                    ),
                })
            }
            ResolvedConcreteItem::Trait(concrete_trait_id) => {
                let Some(trait_item_id) =
                    db.trait_item_by_name(concrete_trait_id.trait_id(db), ident)?
                else {
                    return Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath));
                };

                if let Ok(Some(trait_item_info)) =
                    db.trait_item_info_by_name(concrete_trait_id.trait_id(db), ident)
                {
                    self.resolver.validate_feature_constraints(
                        self.diagnostics,
                        identifier,
                        &trait_item_info,
                    );
                }

                match trait_item_id {
                    TraitItemId::Function(trait_function_id) => {
                        let concrete_trait_function = ConcreteTraitGenericFunctionLongId::new(
                            db,
                            *concrete_trait_id,
                            trait_function_id,
                        )
                        .intern(db);
                        let identifier_stable_ptr = identifier.stable_ptr(db).untyped();
                        let impl_lookup_context =
                            self.resolver.impl_lookup_context_ex(&self.macro_info);
                        let generic_function = GenericFunctionId::Impl(
                            self.resolver.inference().infer_trait_generic_function(
                                concrete_trait_function,
                                impl_lookup_context,
                                Some(identifier_stable_ptr),
                            ),
                        );

                        Ok(ResolvedConcreteItem::Function(self.resolver.specialize_function(
                            self.diagnostics,
                            identifier_stable_ptr,
                            generic_function,
                            &generic_args_syntax.unwrap_or_default(),
                        )?))
                    }
                    TraitItemId::Type(trait_type_id) => {
                        let concrete_trait_type = ConcreteTraitTypeId::new_from_data(
                            db,
                            *concrete_trait_id,
                            trait_type_id,
                        );

                        let impl_lookup_context =
                            self.resolver.impl_lookup_context_ex(&self.macro_info);
                        let identifier_stable_ptr = identifier.stable_ptr(db).untyped();
                        let ty = self.resolver.inference().infer_trait_type(
                            concrete_trait_type,
                            impl_lookup_context,
                            Some(identifier_stable_ptr),
                        );
                        Ok(ResolvedConcreteItem::Type(
                            self.resolver.inference().rewrite(ty).no_err(),
                        ))
                    }
                    TraitItemId::Constant(trait_constant_id) => {
                        let concrete_trait_constant = ConcreteTraitConstantLongId::new(
                            db,
                            *concrete_trait_id,
                            trait_constant_id,
                        )
                        .intern(db);

                        let impl_lookup_context =
                            self.resolver.impl_lookup_context_ex(&self.macro_info);
                        let identifier_stable_ptr = identifier.stable_ptr(db).untyped();
                        let imp_constant_id = self.resolver.inference().infer_trait_constant(
                            concrete_trait_constant,
                            impl_lookup_context,
                            Some(identifier_stable_ptr),
                        );
                        // Make sure the inference is solved for successful impl lookup
                        // Ignore the result of the `solve()` call - the error, if any, will be
                        // reported later.
                        self.resolver.inference().solve().ok();

                        Ok(ResolvedConcreteItem::Constant(
                            ConstValue::ImplConstant(imp_constant_id).intern(db),
                        ))
                    }
                    TraitItemId::Impl(trait_impl_id) => {
                        let concrete_trait_impl = ConcreteTraitImplLongId::new_from_data(
                            db,
                            *concrete_trait_id,
                            trait_impl_id,
                        )
                        .intern(db);

                        let impl_lookup_context =
                            self.resolver.impl_lookup_context_ex(&self.macro_info);
                        let identifier_stable_ptr = identifier.stable_ptr(db).untyped();
                        let impl_impl_id = self.resolver.inference().infer_trait_impl(
                            concrete_trait_impl,
                            impl_lookup_context,
                            Some(identifier_stable_ptr),
                        );
                        // Make sure the inference is solved for successful impl lookup
                        // Ignore the result of the `solve()` call - the error, if any, will be
                        // reported later.
                        self.resolver.inference().solve().ok();

                        Ok(ResolvedConcreteItem::Impl(
                            ImplLongId::ImplImpl(impl_impl_id).intern(db),
                        ))
                    }
                }
            }
            ResolvedConcreteItem::Impl(impl_id) => {
                let concrete_trait_id = db.impl_concrete_trait(*impl_id)?;
                let trait_id = concrete_trait_id.trait_id(db);
                let Some(trait_item_id) = db.trait_item_by_name(trait_id, ident)? else {
                    return Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath));
                };
                if let Ok(Some(trait_item_info)) =
                    db.trait_item_info_by_name(concrete_trait_id.trait_id(db), ident)
                {
                    self.resolver.validate_feature_constraints(
                        self.diagnostics,
                        identifier,
                        &trait_item_info,
                    );
                }
                if let ImplLongId::Concrete(concrete_impl) = impl_id.long(db) {
                    let impl_def_id: ImplDefId<'_> = concrete_impl.impl_def_id(db);

                    if let Ok(Some(impl_item_info)) = db.impl_item_info_by_name(impl_def_id, ident)
                    {
                        self.resolver.validate_feature_constraints(
                            self.diagnostics,
                            identifier,
                            &impl_item_info,
                        );
                    }
                }

                match trait_item_id {
                    TraitItemId::Function(trait_function_id) => {
                        let generic_function_id = GenericFunctionId::Impl(ImplGenericFunctionId {
                            impl_id: *impl_id,
                            function: trait_function_id,
                        });

                        Ok(ResolvedConcreteItem::Function(self.resolver.specialize_function(
                            self.diagnostics,
                            identifier.stable_ptr(db).untyped(),
                            generic_function_id,
                            &generic_args_syntax.unwrap_or_default(),
                        )?))
                    }
                    TraitItemId::Type(trait_type_id) => {
                        let impl_type_id = ImplTypeId::new(*impl_id, trait_type_id, db);
                        let ty = self
                            .resolver
                            .inference()
                            .reduce_impl_ty(impl_type_id)
                            .unwrap_or_else(|_| TypeLongId::ImplType(impl_type_id).intern(db));
                        Ok(ResolvedConcreteItem::Type(ty))
                    }
                    TraitItemId::Constant(trait_constant_id) => {
                        let impl_constant_id = ImplConstantId::new(*impl_id, trait_constant_id, db);

                        let constant = self
                            .resolver
                            .inference()
                            .reduce_impl_constant(impl_constant_id)
                            .unwrap_or_else(|_| {
                                ConstValue::ImplConstant(impl_constant_id).intern(db)
                            });

                        Ok(ResolvedConcreteItem::Constant(constant))
                    }
                    TraitItemId::Impl(trait_impl_id) => {
                        let impl_impl_id = ImplImplId::new(*impl_id, trait_impl_id, db);
                        let imp = self
                            .resolver
                            .inference()
                            .reduce_impl_impl(impl_impl_id)
                            .unwrap_or_else(|_| ImplLongId::ImplImpl(impl_impl_id).intern(db));

                        Ok(ResolvedConcreteItem::Impl(imp))
                    }
                }
            }
            ResolvedConcreteItem::Function(function_id) if ident.long(db) == "Coupon" => {
                if !are_coupons_enabled(db, self.resolver.active_module_id(&self.macro_info)) {
                    self.diagnostics.report(identifier.stable_ptr(db), CouponsDisabled);
                }
                if matches!(
                    function_id.get_concrete(db).generic_function,
                    GenericFunctionId::Extern(_)
                ) {
                    return Err(self
                        .diagnostics
                        .report(identifier.stable_ptr(db), CouponForExternFunctionNotAllowed));
                }
                Ok(ResolvedConcreteItem::Type(TypeLongId::Coupon(*function_id).intern(db)))
            }
            _ => Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath)),
        }
    }

    /// Given the current resolved item, resolves the next segment.
    fn next_generic(
        &mut self,
        containing_item: &ResolvedGenericItem<'db>,
        segment: &ast::PathSegment<'db>,
    ) -> Maybe<ResolvedGenericItem<'db>> {
        let db = self.resolver.db;
        let identifier = segment.identifier_ast(db);
        let ident = identifier.text(db);
        match containing_item {
            ResolvedGenericItem::Module(module_id) => {
                let inner_item_info = self.module_inner_item(module_id, ident, &identifier)?;

                self.validate_module_item_usability(*module_id, &identifier, &inner_item_info);
                self.resolver.insert_used_use(inner_item_info.item_id);
                ResolvedGenericItem::from_module_item(db, inner_item_info.item_id)
            }
            ResolvedGenericItem::GenericType(GenericTypeId::Enum(enum_id)) => {
                let variants = db.enum_variants(*enum_id)?;
                let variant_id = variants.get(&ident).ok_or_else(|| {
                    self.diagnostics.report(
                        identifier.stable_ptr(db),
                        NoSuchVariant { enum_id: *enum_id, variant_name: ident },
                    )
                })?;
                let variant = db.variant_semantic(*enum_id, *variant_id)?;
                Ok(ResolvedGenericItem::Variant(variant))
            }
            _ => Err(self.diagnostics.report(identifier.stable_ptr(db), InvalidPath)),
        }
    }

    /// Determines the base module or crate for the path resolving. Looks only in non-local scope
    /// (i.e. current module, or crates).
    fn determine_base(
        &mut self,
        identifier: &ast::TerminalIdentifier<'db>,
    ) -> Maybe<ResolvedBase<'db>> {
        let db = self.resolver.db;
        let ident = identifier.text(db);
        if let ResolutionContext::Statement(ref mut env) = self.resolution_context
            && let Some(item) = get_statement_item_by_name(env, ident)
        {
            return Ok(ResolvedBase::StatementEnvironment(item));
        }

        let Some(module_id) = self.resolver.try_get_active_module_id(&self.macro_info) else {
            let item_type = if self.segments.len() == 1 {
                self.expected_item_type
            } else {
                NotFoundItemType::Identifier
            };
            return Err(self
                .diagnostics
                .report(identifier.stable_ptr(db), PathNotFound(item_type)));
        };
        // If an item with this name is found inside the current module, use the current module.
        if let Some(info) = self.resolver.resolve_item_in_module_or_expanded_macro(module_id, ident)
            && !matches!(self.resolution_context, ResolutionContext::ModuleItem(id) if id == info.item_id)
        {
            return Ok(ResolvedBase::Module(module_id));
        }

        // If the first element is `crate`, use the crate's root module as the base module.
        if ident.long(db) == CRATE_KW {
            return Ok(ResolvedBase::Crate(self.resolver.active_owning_crate_id(&self.macro_info)));
        }
        // If the first segment is a name of a crate, use the crate's root module as the base
        // module.
        if let Some(dep) = self
            .resolver
            .active_settings(&self.macro_info)
            .dependencies
            .get(ident.long(db).as_str())
        {
            let dep_crate_id =
                CrateLongId::Real { name: ident, discriminator: dep.discriminator.clone() }
                    .intern(db);
            let configs = db.crate_configs();
            if !configs.contains_key(&dep_crate_id) {
                let get_long_id = |crate_id: CrateId<'db>| crate_id.long(db);
                panic!(
                    "Invalid crate dependency: {:?}\nconfigured crates: {:#?}",
                    get_long_id(dep_crate_id),
                    configs.keys().cloned().map(get_long_id).collect_vec()
                );
            }

            return Ok(ResolvedBase::Crate(dep_crate_id));
        }
        // If the first segment is `core` - and it was not overridden by a dependency - using it.
        if ident.long(db) == CORELIB_CRATE_NAME {
            return Ok(ResolvedBase::Crate(CrateId::core(db)));
        }
        // TODO(orizi): Remove when `starknet` becomes a proper crate.
        if ident.long(db) == STARKNET_CRATE_NAME {
            // Making sure we don't look for it in `*` modules, to prevent cycles.
            return Ok(ResolvedBase::Module(self.resolver.prelude_submodule_ex(&self.macro_info)));
        }
        // Finding whether the item is an imported module.
        match self.resolver.resolve_path_using_use_star(module_id, ident) {
            UseStarResult::UniquePathFound(inner_module_item) => {
                return Ok(ResolvedBase::FoundThroughGlobalUse {
                    item_info: inner_module_item,
                    containing_module: module_id,
                });
            }
            UseStarResult::AmbiguousPath(module_items) => {
                return Ok(ResolvedBase::Ambiguous(module_items));
            }
            UseStarResult::PathNotFound => {}
            UseStarResult::ItemNotVisible(module_item_id, containing_modules) => {
                let prelude = self.resolver.prelude_submodule_ex(&self.macro_info);
                if let Ok(Some(_)) = db.module_item_by_name(prelude, ident) {
                    return Ok(ResolvedBase::Module(prelude));
                }
                return Ok(ResolvedBase::ItemNotVisible(module_item_id, containing_modules));
            }
        }
        Ok(ResolvedBase::Module(self.resolver.prelude_submodule_ex(&self.macro_info)))
    }
    /// Validates that an item is usable from the current module or adds a diagnostic.
    /// This includes visibility checks and feature checks.
    fn validate_module_item_usability(
        &mut self,
        containing_module_id: ModuleId<'db>,
        identifier: &ast::TerminalIdentifier<'db>,
        item_info: &ModuleItemInfo<'db>,
    ) {
        if !self.resolver.is_item_visible_ex(
            containing_module_id,
            item_info,
            self.resolver.active_module_id(&self.macro_info),
            &self.macro_info,
        ) {
            self.diagnostics.report(
                identifier.stable_ptr(self.resolver.db),
                ItemNotVisible(item_info.item_id, vec![]),
            );
        }

        self.resolver.validate_feature_constraints(self.diagnostics, identifier, item_info);
    }
}
