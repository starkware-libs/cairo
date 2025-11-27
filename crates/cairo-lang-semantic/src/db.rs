use std::sync::Arc;

use cairo_lang_defs::db::{DefsGroup, DefsGroupEx, defs_group_input};
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ImplAliasId, InlineMacroExprPluginId, InlineMacroExprPluginLongId, LanguageElementId,
    LookupItemId, MacroPluginId, MacroPluginLongId, ModuleId, ModuleItemId, UseId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, CrateInput, FileId, FileLongId, SmolStrId, Tracked};
use cairo_lang_syntax::attribute::consts::{DEPRECATED_ATTR, UNUSED_IMPORTS, UNUSED_VARIABLES};
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{Intern, require};
use itertools::{Itertools, chain};
use salsa::{Database, Setter};

use crate::SemanticDiagnostic;
use crate::cache::{SemanticCacheLoadingData, load_cached_crate_modules_semantic};
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnosticsBuilder};
use crate::ids::{AnalyzerPluginId, AnalyzerPluginLongId};
use crate::items::constant::ConstantSemantic;
use crate::items::enm::EnumSemantic;
use crate::items::extern_function::ExternFunctionSemantic;
use crate::items::extern_type::ExternTypeSemantic;
use crate::items::free_function::FreeFunctionSemantic;
use crate::items::imp::{ImplId, ImplSemantic};
use crate::items::impl_alias::ImplAliasSemantic;
use crate::items::macro_call::{MacroCallSemantic, module_macro_modules};
use crate::items::macro_declaration::MacroDeclarationSemantic;
use crate::items::module::{ModuleSemantic, ModuleSemanticData};
use crate::items::module_type_alias::ModuleTypeAliasSemantic;
use crate::items::structure::StructSemantic;
use crate::items::trt::TraitSemantic;
use crate::items::us::{SemanticUseEx, UseSemantic};
use crate::items::visibility::Visibility;
use crate::plugin::{AnalyzerPlugin, InternedPluginSuite, PluginSuite};
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, ResolverData};

#[salsa::input]
pub struct SemanticGroupInput {
    #[returns(ref)]
    pub default_analyzer_plugins: Option<Vec<AnalyzerPluginLongId>>,
    #[returns(ref)]
    pub analyzer_plugin_overrides: Option<OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>>>,
}

#[salsa::tracked(returns(ref))]
pub fn semantic_group_input(db: &dyn Database) -> SemanticGroupInput {
    SemanticGroupInput::new(db, None, None)
}

fn default_analyzer_plugins_input(db: &dyn Database) -> &[AnalyzerPluginLongId] {
    semantic_group_input(db).default_analyzer_plugins(db).as_ref().unwrap()
}

fn analyzer_plugin_overrides_input(
    db: &dyn Database,
) -> &OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>> {
    semantic_group_input(db).analyzer_plugin_overrides(db).as_ref().unwrap()
}

// Salsa database interface.
// All queries starting with priv_ are for internal use only by this crate.
// They appear in the public API because of salsa limitations.
// We differentiate between the declaration and the definition of each item:
// Declarations and definitions must not depend on other definitions, only other declarations.
// This prevents cycles where there shouldn't be any.
pub trait SemanticGroup: Database {
    // Lookups.
    // ========
    fn lookup_resolved_generic_item_by_ptr<'db>(
        &'db self,
        id: LookupItemId<'db>,
        ptr: ast::TerminalIdentifierPtr<'db>,
    ) -> Option<ResolvedGenericItem<'db>> {
        lookup_resolved_generic_item_by_ptr(self.as_dyn_database(), id, ptr)
    }

    fn lookup_resolved_concrete_item_by_ptr<'db>(
        &'db self,
        id: LookupItemId<'db>,
        ptr: ast::TerminalIdentifierPtr<'db>,
    ) -> Option<ResolvedConcreteItem<'db>> {
        lookup_resolved_concrete_item_by_ptr(self.as_dyn_database(), id, ptr)
    }

    // Diagnostics.
    // ============
    /// Aggregates module level semantic diagnostics.
    fn module_semantic_diagnostics<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
        module_semantic_diagnostics_tracked(self.as_dyn_database(), (), module_id)
    }

    /// Aggregates file level semantic diagnostics.
    fn file_semantic_diagnostics<'db>(
        &'db self,
        file_id: FileId<'db>,
    ) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
        file_semantic_diagnostics(self.as_dyn_database(), file_id)
    }

    // Analyzer plugins.
    // ========
    fn default_analyzer_plugins_input(&self) -> &[AnalyzerPluginLongId] {
        default_analyzer_plugins_input(self.as_dyn_database())
    }

    /// Interned version of `default_analyzer_plugins`.
    fn default_analyzer_plugins<'db>(&'db self) -> Arc<Vec<AnalyzerPluginId<'db>>> {
        default_analyzer_plugins(self.as_dyn_database())
    }

    fn analyzer_plugin_overrides_input(
        &self,
    ) -> &OrderedHashMap<CrateInput, Arc<[AnalyzerPluginLongId]>> {
        analyzer_plugin_overrides_input(self.as_dyn_database())
    }

    /// Interned version of `analyzer_plugin_overrides_input`.
    fn analyzer_plugin_overrides<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<CrateId<'db>, Arc<Vec<AnalyzerPluginId<'db>>>>> {
        analyzer_plugin_overrides(self.as_dyn_database())
    }

    /// Returns [`AnalyzerPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Returns
    /// [`SemanticGroupEx::set_override_crate_analyzer_plugins`] if it has been set,
    /// or the ([`SemanticGroup::default_analyzer_plugins`]) otherwise.
    fn crate_analyzer_plugins<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Arc<Vec<AnalyzerPluginId<'db>>> {
        crate_analyzer_plugins(self.as_dyn_database(), crate_id)
    }

    /// Returns the set of `allow` that were declared as by a plugin.
    /// An allow that is not in this set will be handled as an unknown allow.
    fn declared_allows<'db>(&self, crate_id: CrateId<'db>) -> Arc<OrderedHashSet<String>> {
        declared_allows(self.as_dyn_database(), crate_id)
    }

    /// Returns the [ModuleSemanticData] of all modules in the crate's cache, and the loading data
    /// of the [SemanticGroup] in the crate.
    fn cached_crate_semantic_data<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Option<ModuleSemanticDataCacheAndLoadingData<'db>> {
        cached_crate_semantic_data(self.as_dyn_database(), crate_id)
    }
}

impl<T: Database + ?Sized> SemanticGroup for T {}

/// Initializes the [`SemanticGroup`] database to a proper state.
pub fn init_semantic_group(db: &mut dyn Database) {
    semantic_group_input(db).set_analyzer_plugin_overrides(db).to(Some(OrderedHashMap::default()));
}

#[salsa::tracked]
fn default_analyzer_plugins(db: &dyn Database) -> Arc<Vec<AnalyzerPluginId<'_>>> {
    let inp = db.default_analyzer_plugins_input();
    Arc::new(inp.iter().map(|plugin| plugin.clone().intern(db)).collect_vec())
}

#[salsa::tracked]
fn analyzer_plugin_overrides(
    db: &dyn Database,
) -> Arc<OrderedHashMap<CrateId<'_>, Arc<Vec<AnalyzerPluginId<'_>>>>> {
    let inp = db.analyzer_plugin_overrides_input();
    Arc::new(
        inp.iter()
            .map(|(crate_input, plugins)| {
                (
                    crate_input.clone().into_crate_long_id(db).intern(db),
                    Arc::new(plugins.iter().map(|plugin| plugin.clone().intern(db)).collect_vec()),
                )
            })
            .collect(),
    )
}

#[salsa::tracked]
fn module_semantic_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    module_id: ModuleId<'db>,
) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
    module_semantic_diagnostics(db, module_id)
}

fn module_semantic_diagnostics<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for (_module_id, plugin_diag) in
        module_id.module_data(db)?.plugin_diagnostics(db).iter().cloned()
    {
        diagnostics.add(SemanticDiagnostic::new(
            match plugin_diag.inner_span {
                None => StableLocation::new(plugin_diag.stable_ptr),
                Some(inner_span) => {
                    StableLocation::with_inner_span(plugin_diag.stable_ptr, inner_span)
                }
            },
            SemanticDiagnosticKind::PluginDiagnostic(plugin_diag),
        ));
    }
    let data = db.priv_module_semantic_data(module_id)?;
    diagnostics.extend(data.diagnostics.clone());
    // TODO(Gil): Aggregate diagnostics for subitems with semantic model (i.e. impl function, trait
    // functions and generic params) directly and not via the parent item.
    for item in module_id.module_data(db)?.items(db).iter() {
        match item {
            ModuleItemId::Constant(const_id) => {
                diagnostics.extend(db.constant_semantic_diagnostics(*const_id));
            }
            // Add signature diagnostics.
            ModuleItemId::Use(use_id) => {
                diagnostics.extend(db.use_semantic_diagnostics(*use_id));
            }
            ModuleItemId::FreeFunction(free_function) => {
                diagnostics.extend(db.free_function_declaration_diagnostics(*free_function));
                diagnostics.extend(db.free_function_body_diagnostics(*free_function));
            }
            ModuleItemId::Struct(struct_id) => {
                diagnostics.extend(db.struct_declaration_diagnostics(*struct_id));
                diagnostics.extend(db.struct_definition_diagnostics(*struct_id));
            }
            ModuleItemId::Enum(enum_id) => {
                diagnostics.extend(db.enum_definition_diagnostics(*enum_id));
                diagnostics.extend(db.enum_declaration_diagnostics(*enum_id));
            }
            ModuleItemId::Trait(trait_id) => {
                diagnostics.extend(db.trait_semantic_declaration_diagnostics(*trait_id));
                diagnostics.extend(db.trait_semantic_definition_diagnostics(*trait_id));
            }
            ModuleItemId::Impl(impl_def_id) => {
                diagnostics.extend(db.impl_semantic_declaration_diagnostics(*impl_def_id));
                diagnostics.extend(db.impl_semantic_definition_diagnostics(*impl_def_id));
            }
            ModuleItemId::Submodule(submodule_id) => {
                // Note that the parent module does not report the diagnostics of its submodules.
                if let Ok(file_id) = db.module_main_file(ModuleId::Submodule(*submodule_id))
                    && db.file_content(file_id).is_none()
                {
                    // Note that the error location is in the parent module, not the
                    // submodule.

                    let path = match file_id.long(db) {
                        FileLongId::OnDisk(path) => path.display().to_string(),
                        FileLongId::Virtual(_) | FileLongId::External(_) => {
                            panic!("Expected OnDisk file.")
                        }
                    };

                    let stable_location =
                        StableLocation::new(submodule_id.stable_ptr(db).untyped());
                    diagnostics.add(SemanticDiagnostic::new(
                        stable_location,
                        SemanticDiagnosticKind::ModuleFileNotFound(path),
                    ));
                }
            }
            ModuleItemId::ExternType(extern_type) => {
                diagnostics.extend(db.extern_type_declaration_diagnostics(*extern_type));
            }
            ModuleItemId::ExternFunction(extern_function) => {
                diagnostics.extend(db.extern_function_declaration_diagnostics(*extern_function));
            }
            ModuleItemId::TypeAlias(type_alias) => {
                diagnostics.extend(db.module_type_alias_semantic_diagnostics(*type_alias));
            }
            ModuleItemId::ImplAlias(type_alias) => {
                diagnostics.extend(db.impl_alias_semantic_diagnostics(*type_alias));
            }
            ModuleItemId::MacroDeclaration(macro_declaration) => {
                diagnostics.extend(db.macro_declaration_diagnostics(*macro_declaration));
            }
        }
    }
    for global_use in module_id.module_data(db)?.global_uses(db).keys() {
        diagnostics.extend(db.global_use_semantic_diagnostics(*global_use));
    }
    for macro_call in db.module_macro_calls_ids(module_id)?.iter() {
        diagnostics.extend(db.macro_call_diagnostics(*macro_call));
        if let Ok(macro_module_id) = db.macro_call_module_id(*macro_call)
            && let Ok(semantic_diags) = db.module_semantic_diagnostics(macro_module_id)
        {
            diagnostics.extend(semantic_diags);
        }
    }
    add_unused_item_diagnostics(db, module_id, data, &mut diagnostics);
    add_duplicated_names_from_macro_expansions_diagnostics(db, module_id, &mut diagnostics);
    for analyzer_plugin_id in db.crate_analyzer_plugins(module_id.owning_crate(db)).iter() {
        let analyzer_plugin = analyzer_plugin_id.long(db);

        for diag in analyzer_plugin.diagnostics(db, module_id) {
            diagnostics.add(SemanticDiagnostic::new(
                StableLocation::new(diag.stable_ptr),
                SemanticDiagnosticKind::PluginDiagnostic(diag),
            ));
        }
    }

    Ok(diagnostics.build())
}

/// Adds diagnostics for duplicated names from macro expansions.
fn add_duplicated_names_from_macro_expansions_diagnostics<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
) {
    if matches!(module_id, ModuleId::MacroCall { .. }) {
        // Macro calls are handled by the caller.
        return;
    }
    let mut names = UnorderedHashSet::<SmolStrId<'_>>::default();
    for defined_module in chain!([&module_id], module_macro_modules(db, false, module_id)) {
        let Ok(data) = db.priv_module_semantic_data(*defined_module) else {
            continue;
        };
        for (name, info) in data.items.iter() {
            if !names.insert(*name)
                && let Ok(stable_ptr) =
                    db.module_item_name_stable_ptr(*defined_module, info.item_id)
            {
                diagnostics
                    .report(stable_ptr, SemanticDiagnosticKind::NameDefinedMultipleTimes(*name));
            }
        }
    }
}

fn crate_analyzer_plugins<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Arc<Vec<AnalyzerPluginId<'db>>> {
    db.analyzer_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_analyzer_plugins())
}

#[salsa::tracked]
fn declared_allows(db: &dyn Database, crate_id: CrateId<'_>) -> Arc<OrderedHashSet<String>> {
    let base_lints = [DEPRECATED_ATTR, UNUSED_IMPORTS, UNUSED_VARIABLES];

    let crate_analyzer_plugins = db.crate_analyzer_plugins(crate_id);

    Arc::new(OrderedHashSet::from_iter(chain!(
        base_lints.map(|attr| attr.into()),
        crate_analyzer_plugins.iter().flat_map(|plugin| plugin.long(db).declared_allows())
    )))
}

/// Adds diagnostics for unused items in a module.
///
/// Returns `None` if skipped attempt to add diagnostics.
fn add_unused_item_diagnostics<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    data: &ModuleSemanticData<'db>,
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
) {
    let Ok(all_used_uses) = db.module_all_used_uses(module_id) else {
        return;
    };
    for info in data.items.values() {
        if info.visibility == Visibility::Public {
            continue;
        }
        if let ModuleItemId::Use(use_id) = info.item_id {
            add_unused_import_diagnostics(db, all_used_uses, use_id, diagnostics);
        };
    }
}

/// Adds diagnostics for unused imports.
fn add_unused_import_diagnostics<'db>(
    db: &'db dyn Database,
    all_used_uses: &OrderedHashSet<UseId<'db>>,
    use_id: UseId<'db>,
    diagnostics: &mut DiagnosticsBuilder<'db, SemanticDiagnostic<'db>>,
) {
    let _iife = (|| {
        let item = db.use_resolved_item(use_id).ok()?;
        // TODO(orizi): Properly handle usages of impls, and than add warnings on their usages as
        // well.
        require(!matches!(
            item,
            ResolvedGenericItem::Impl(_) | ResolvedGenericItem::GenericImplAlias(_)
        ))?;
        require(!all_used_uses.contains(&use_id))?;
        let resolver_data = db.use_resolver_data(use_id).ok()?;

        require(
            !resolver_data
                .feature_config
                .allowed_lints
                .contains(&SmolStrId::from(db, UNUSED_IMPORTS)),
        )?;
        Some(diagnostics.add(SemanticDiagnostic::new(
            StableLocation::new(use_id.untyped_stable_ptr(db)),
            SemanticDiagnosticKind::UnusedImport(use_id),
        )))
    })();
}

#[salsa::tracked]
fn file_semantic_diagnostics<'db>(
    db: &'db dyn Database,
    file_id: FileId<'db>,
) -> Maybe<Diagnostics<'db, SemanticDiagnostic<'db>>> {
    let mut diagnostics = DiagnosticsBuilder::default();
    for module_id in db.file_modules(file_id)?.iter().copied() {
        if let Ok(module_diagnostics) = db.module_semantic_diagnostics(module_id) {
            diagnostics.extend(module_diagnostics)
        }
    }
    Ok(diagnostics.build())
}

#[salsa::tracked]
pub fn lookup_resolved_generic_item_by_ptr<'db>(
    db: &'db dyn Database,
    id: LookupItemId<'db>,
    ptr: ast::TerminalIdentifierPtr<'db>,
) -> Option<ResolvedGenericItem<'db>> {
    get_resolver_data_options(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.generic.get(&ptr).cloned())
}

#[salsa::tracked]
pub fn lookup_resolved_concrete_item_by_ptr<'db>(
    db: &'db dyn Database,
    id: LookupItemId<'db>,
    ptr: ast::TerminalIdentifierPtr<'db>,
) -> Option<ResolvedConcreteItem<'db>> {
    get_resolver_data_options(id, db)
        .into_iter()
        .find_map(|resolver_data| resolver_data.resolved_items.concrete.get(&ptr).cloned())
}

pub fn get_resolver_data_options<'db>(
    id: LookupItemId<'db>,
    db: &'db dyn Database,
) -> Vec<Arc<ResolverData<'db>>> {
    match id {
        LookupItemId::ModuleItem(module_item) => match module_item {
            ModuleItemId::Constant(id) => vec![db.constant_resolver_data(id)],
            ModuleItemId::Submodule(_) => vec![],
            ModuleItemId::Use(id) => vec![db.use_resolver_data(id)],
            ModuleItemId::FreeFunction(id) => vec![
                db.free_function_declaration_resolver_data(id),
                db.free_function_body_resolver_data(id),
            ],
            ModuleItemId::Struct(id) => vec![
                db.struct_declaration_resolver_data(id),
                db.struct_definition_resolver_data(id),
            ],
            ModuleItemId::Enum(id) => {
                vec![db.enum_definition_resolver_data(id), db.enum_declaration_resolver_data(id)]
            }
            ModuleItemId::TypeAlias(id) => vec![db.module_type_alias_resolver_data(id)],
            ModuleItemId::ImplAlias(id) => vec![db.impl_alias_resolver_data(id)],
            ModuleItemId::Trait(id) => vec![db.trait_resolver_data(id)],
            ModuleItemId::Impl(id) => vec![db.impl_def_resolver_data(id)],
            ModuleItemId::ExternType(_) => vec![],
            ModuleItemId::ExternFunction(id) => {
                vec![db.extern_function_declaration_resolver_data(id)]
            }
            ModuleItemId::MacroDeclaration(id) => vec![db.macro_declaration_resolver_data(id)],
        },
        LookupItemId::TraitItem(id) => match id {
            cairo_lang_defs::ids::TraitItemId::Function(id) => {
                let mut res = vec![db.trait_function_resolver_data(id)];
                if let Ok(Some(resolver_data)) = db.trait_function_body_resolver_data(id) {
                    res.push(Ok(resolver_data));
                }
                res
            }
            cairo_lang_defs::ids::TraitItemId::Type(id) => vec![db.trait_type_resolver_data(id)],
            cairo_lang_defs::ids::TraitItemId::Constant(id) => {
                vec![db.trait_constant_resolver_data(id)]
            }
            cairo_lang_defs::ids::TraitItemId::Impl(id) => vec![db.trait_impl_resolver_data(id)],
        },
        LookupItemId::ImplItem(id) => match id {
            cairo_lang_defs::ids::ImplItemId::Function(id) => {
                vec![db.impl_function_resolver_data(id), db.impl_function_body_resolver_data(id)]
            }
            cairo_lang_defs::ids::ImplItemId::Type(id) => vec![db.impl_type_def_resolver_data(id)],
            cairo_lang_defs::ids::ImplItemId::Constant(id) => {
                vec![db.impl_constant_def_resolver_data(id)]
            }
            cairo_lang_defs::ids::ImplItemId::Impl(id) => vec![db.impl_impl_def_resolver_data(id)],
        },
    }
    .into_iter()
    .flatten()
    .collect()
}

pub trait SemanticGroupEx: Database {
    /// Overrides the default analyzer plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `SemanticGroup::analyzer_plugin_overrides`.
    fn set_override_crate_analyzer_plugins(
        &mut self,
        crate_id: CrateId<'_>,
        plugins: Arc<[AnalyzerPluginId<'_>]>,
    ) {
        let mut overrides = self.analyzer_plugin_overrides_input().clone();
        let plugins = plugins.iter().map(|plugin| plugin.long(self).clone()).collect_vec();
        overrides.insert(self.crate_input(crate_id).clone(), Arc::from(plugins));
        let db_ref = self.as_dyn_database();
        semantic_group_input(db_ref).set_analyzer_plugin_overrides(self).to(Some(overrides));
    }
}

impl<T: Database + ?Sized> SemanticGroupEx for T {}

/// An extension trait for [`SemanticGroup`] to manage plugin setters.
pub trait PluginSuiteInput: Database {
    /// Interns each plugin from the [`PluginSuite`] into the database.
    fn intern_plugin_suite<'r>(&'r mut self, suite: PluginSuite) -> InternedPluginSuite<'r> {
        let PluginSuite { plugins, inline_macro_plugins, analyzer_plugins } = suite;

        let macro_plugins = plugins
            .into_iter()
            .map(|plugin| MacroPluginId::new(self, MacroPluginLongId(plugin)))
            .collect::<Arc<[_]>>();

        let inline_macro_plugins = Arc::new(
            inline_macro_plugins
                .into_iter()
                .map(|(name, plugin)| {
                    (name, InlineMacroExprPluginId::new(self, InlineMacroExprPluginLongId(plugin)))
                })
                .collect::<OrderedHashMap<_, _>>(),
        );

        let analyzer_plugins = analyzer_plugins
            .into_iter()
            .map(|plugin| AnalyzerPluginId::new(self, AnalyzerPluginLongId(plugin)))
            .collect::<Arc<[_]>>();

        InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins }
    }

    /// Sets macro, inline macro and analyzer plugins specified in the [`PluginSuite`] as default
    /// for all crates.
    ///
    /// *Note*: Sets the following Salsa inputs: [`DefsGroup::default_macro_plugins`],
    /// [`DefsGroup::default_inline_macro_plugins`], and
    /// [`SemanticGroup::default_analyzer_plugins`].
    fn set_default_plugins_from_suite(&mut self, suite: PluginSuite) {
        let PluginSuite { plugins, inline_macro_plugins, analyzer_plugins } = suite;
        // let interned = self.intern_plugin_suite(suite);

        let macro_plugins = plugins.into_iter().map(MacroPluginLongId).collect_vec();

        let inline_macro_plugins = inline_macro_plugins
            .into_iter()
            .map(|(name, plugin)| (name, InlineMacroExprPluginLongId(plugin)))
            .collect::<OrderedHashMap<_, _>>();

        let analyzer_plugins =
            analyzer_plugins.into_iter().map(AnalyzerPluginLongId).collect::<Vec<_>>();

        defs_group_input(self.as_dyn_database())
            .set_default_macro_plugins(self)
            .to(Some(macro_plugins));
        defs_group_input(self.as_dyn_database())
            .set_default_inline_macro_plugins(self)
            .to(Some(inline_macro_plugins));
        semantic_group_input(self.as_dyn_database())
            .set_default_analyzer_plugins(self)
            .to(Some(analyzer_plugins));
    }

    /// Sets macro, inline macro and analyzer plugins present in the [`PluginSuite`] for a crate
    /// pointed to by the [`CrateId`], overriding the defaults for that crate.
    ///
    /// *Note*: Sets the following Salsa inputs: [`DefsGroup::macro_plugin_overrides`],
    /// [`DefsGroup::inline_macro_plugin_overrides`], and
    /// [`SemanticGroup::analyzer_plugin_overrides`].
    fn set_override_crate_plugins_from_suite(
        &mut self,
        crate_id: CrateId<'_>,
        suite: InternedPluginSuite<'_>,
    ) {
        let InternedPluginSuite { macro_plugins, inline_macro_plugins, analyzer_plugins } = suite;

        self.set_override_crate_macro_plugins(crate_id, Arc::new(macro_plugins.to_vec()));
        self.set_override_crate_inline_macro_plugins(crate_id, inline_macro_plugins);
        self.set_override_crate_analyzer_plugins(crate_id, analyzer_plugins);
    }
}

impl<T: Database + ?Sized> PluginSuiteInput for T {}

/// Returns all ancestors (parents) of the given module, including the module itself, in order from
/// closest to farthest.
pub fn module_ancestors<'db>(
    db: &'db dyn Database,
    mut module_id: ModuleId<'db>,
) -> Vec<ModuleId<'db>> {
    let mut ancestors = Vec::new();
    ancestors.push(module_id); // Include the module itself first
    while let ModuleId::Submodule(submodule_id) = module_id {
        let parent = submodule_id.parent_module(db);
        ancestors.push(parent);
        module_id = parent;
    }
    ancestors
}

/// Returns all ancestors (parents) and all macro-generated modules within them.
pub fn module_fully_accessible_modules<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
) -> OrderedHashSet<ModuleId<'db>> {
    let mut result: Vec<ModuleId<'db>> = module_ancestors(db, module_id);
    let mut index = 0;
    while let Some(curr) = result.get(index).copied() {
        index += 1;
        if let Ok(macro_call_ids) = db.module_macro_calls_ids(curr) {
            for macro_call_id in macro_call_ids.iter() {
                if let Ok(generated_module_id) = db.macro_call_module_id(*macro_call_id) {
                    result.push(generated_module_id);
                }
            }
        }
    }
    result.into_iter().collect()
}

/// Cache for the semantic data of a crate.
#[derive(PartialEq, Eq, Clone, salsa::Update)]
pub struct ModuleSemanticDataCacheAndLoadingData<'db> {
    /// Semantic data of the modules in the crate.
    pub modules_semantic_data: Arc<OrderedHashMap<ModuleId<'db>, ModuleSemanticData<'db>>>,
    /// Resolved implementations of the impl aliases in the crate.
    pub impl_aliases_resolved_impls: Arc<OrderedHashMap<ImplAliasId<'db>, ImplId<'db>>>,
    /// Loading data of the semantic cache.
    pub loading_data: Arc<SemanticCacheLoadingData<'db>>,
}

#[salsa::tracked]
fn cached_crate_semantic_data<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> Option<ModuleSemanticDataCacheAndLoadingData<'db>> {
    load_cached_crate_modules_semantic(db, crate_id)
}
