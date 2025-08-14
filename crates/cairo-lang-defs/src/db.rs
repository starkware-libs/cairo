use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::{DiagnosticNote, Maybe, PluginFileDiagnosticNotes, ToMaybe};
use cairo_lang_filesystem::db::{ExternalFiles, TryExtAsVirtual};
use cairo_lang_filesystem::ids::{
    CrateId, CrateInput, Directory, FileId, FileKind, FileLongId, VirtualFile,
};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::consts::{
    ALLOW_ATTR, ALLOW_ATTR_ATTR, DEPRECATED_ATTR, FEATURE_ATTR, FMT_SKIP_ATTR,
    IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR, INTERNAL_ATTR, MUST_USE_ATTR, PHANTOM_ATTR,
    STARKNET_INTERFACE_ATTR, UNSTABLE_ATTR,
};
use cairo_lang_syntax::attribute::structured::AttributeStructurize;
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{Itertools, chain};
use salsa::Database;

use crate::cache::{DefCacheLoadingData, load_cached_crate_modules};
use crate::ids::*;
use crate::plugin::{DynGeneratedFileAuxData, MacroPlugin, MacroPluginMetadata, PluginDiagnostic};
use crate::plugin_utils::try_extract_unnamed_arg;

/// Salsa database interface.
/// See [`super::ids`] for further details.
#[cairo_lang_proc_macros::query_group]
pub trait DefsGroup: ParserGroup {
    #[salsa::interned]
    fn intern_constant<'db>(&'db self, id: ConstantLongId<'db>) -> ConstantId<'db>;
    #[salsa::interned]
    fn intern_submodule<'db>(&'db self, id: SubmoduleLongId<'db>) -> SubmoduleId<'db>;
    #[salsa::interned]
    fn intern_use<'db>(&'db self, id: UseLongId<'db>) -> UseId<'db>;
    #[salsa::interned]
    fn intern_global_use<'db>(&'db self, id: GlobalUseLongId<'db>) -> GlobalUseId<'db>;
    #[salsa::interned]
    fn intern_free_function<'db>(&'db self, id: FreeFunctionLongId<'db>) -> FreeFunctionId<'db>;
    #[salsa::interned]
    fn intern_impl_type_def<'db>(&'db self, id: ImplTypeDefLongId<'db>) -> ImplTypeDefId<'db>;
    #[salsa::interned]
    fn intern_impl_constant_def<'db>(
        &'db self,
        id: ImplConstantDefLongId<'db>,
    ) -> ImplConstantDefId<'db>;
    #[salsa::interned]
    fn intern_impl_impl_def<'db>(&'db self, id: ImplImplDefLongId<'db>) -> ImplImplDefId<'db>;
    #[salsa::interned]
    fn intern_impl_function<'db>(&'db self, id: ImplFunctionLongId<'db>) -> ImplFunctionId<'db>;
    #[salsa::interned]
    fn intern_struct<'db>(&'db self, id: StructLongId<'db>) -> StructId<'db>;
    #[salsa::interned]
    fn intern_enum<'db>(&'db self, id: EnumLongId<'db>) -> EnumId<'db>;
    #[salsa::interned]
    fn intern_module_type_alias<'db>(
        &'db self,
        id: ModuleTypeAliasLongId<'db>,
    ) -> ModuleTypeAliasId<'db>;
    #[salsa::interned]
    fn intern_impl_alias<'db>(&'db self, id: ImplAliasLongId<'db>) -> ImplAliasId<'db>;
    #[salsa::interned]
    fn intern_member<'db>(&'db self, id: MemberLongId<'db>) -> MemberId<'db>;
    #[salsa::interned]
    fn intern_variant<'db>(&'db self, id: VariantLongId<'db>) -> VariantId<'db>;
    #[salsa::interned]
    fn intern_trait<'db>(&'db self, id: TraitLongId<'db>) -> TraitId<'db>;
    #[salsa::interned]
    fn intern_trait_type<'db>(&'db self, id: TraitTypeLongId<'db>) -> TraitTypeId<'db>;
    #[salsa::interned]
    fn intern_trait_constant<'db>(&'db self, id: TraitConstantLongId<'db>) -> TraitConstantId<'db>;
    #[salsa::interned]
    fn intern_trait_impl<'db>(&'db self, id: TraitImplLongId<'db>) -> TraitImplId<'db>;
    #[salsa::interned]
    fn intern_trait_function<'db>(&'db self, id: TraitFunctionLongId<'db>) -> TraitFunctionId<'db>;
    #[salsa::interned]
    fn intern_impl_def<'db>(&'db self, id: ImplDefLongId<'db>) -> ImplDefId<'db>;
    #[salsa::interned]
    fn intern_extern_type<'db>(&'db self, id: ExternTypeLongId<'db>) -> ExternTypeId<'db>;
    #[salsa::interned]
    fn intern_extern_function<'db>(
        &'db self,
        id: ExternFunctionLongId<'db>,
    ) -> ExternFunctionId<'db>;
    #[salsa::interned]
    fn intern_macro_declaration<'db>(
        &'db self,
        id: MacroDeclarationLongId<'db>,
    ) -> MacroDeclarationId<'db>;
    #[salsa::interned]
    fn intern_macro_call<'db>(&'db self, id: MacroCallLongId<'db>) -> MacroCallId<'db>;
    #[salsa::interned]
    fn intern_param<'db>(&'db self, id: ParamLongId<'db>) -> ParamId<'db>;
    #[salsa::interned]
    fn intern_generic_param<'db>(&'db self, id: GenericParamLongId<'db>) -> GenericParamId<'db>;
    #[salsa::interned]
    fn intern_local_var<'db>(&'db self, id: LocalVarLongId<'db>) -> LocalVarId<'db>;
    #[salsa::interned]
    fn intern_statement_const<'db>(
        &'db self,
        id: StatementConstLongId<'db>,
    ) -> StatementConstId<'db>;
    #[salsa::interned]
    fn intern_statement_use<'db>(&'db self, id: StatementUseLongId<'db>) -> StatementUseId<'db>;
    #[salsa::interned]
    fn intern_plugin_generated_file<'db>(
        &'db self,
        id: PluginGeneratedFileLongId<'db>,
    ) -> PluginGeneratedFileId<'db>;

    // Plugins.
    // ========

    #[salsa::input]
    fn default_macro_plugins_input(&self) -> Arc<[MacroPluginLongId]>;

    /// Interned version of `default_macro_plugins_input`.
    fn default_macro_plugins<'db>(&'db self) -> Arc<Vec<MacroPluginId<'db>>>;

    #[salsa::input]
    fn macro_plugin_overrides_input(
        &self,
    ) -> Arc<OrderedHashMap<CrateInput, Arc<[MacroPluginLongId]>>>;

    /// Interned version of `macro_plugin_overrides_input`.
    fn macro_plugin_overrides<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<CrateId<'db>, Arc<Vec<MacroPluginId<'db>>>>>;

    #[salsa::interned]
    fn intern_macro_plugin(&self, plugin: MacroPluginLongId) -> MacroPluginId<'_>;

    /// Returns [`MacroPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Provides an override if it has been set with
    /// [`DefsGroupEx::set_override_crate_macro_plugins`] or the default
    /// ([`DefsGroup::default_macro_plugins`]) otherwise.
    fn crate_macro_plugins<'db>(&'db self, crate_id: CrateId<'db>) -> Arc<Vec<MacroPluginId<'db>>>;

    #[salsa::input]
    fn default_inline_macro_plugins_input(
        &self,
    ) -> Arc<OrderedHashMap<String, InlineMacroExprPluginLongId>>;

    /// Interned version of `default_inline_macro_plugins_input`.
    fn default_inline_macro_plugins<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>>;

    #[salsa::input]
    fn inline_macro_plugin_overrides_input(
        &self,
    ) -> Arc<OrderedHashMap<CrateInput, Arc<OrderedHashMap<String, InlineMacroExprPluginLongId>>>>;

    /// Interned version of `inline_macro_plugin_overrides_input`.
    fn inline_macro_plugin_overrides<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<CrateId<'db>, Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>>>>;

    #[salsa::interned]
    fn intern_inline_macro_plugin(
        &self,
        plugin: InlineMacroExprPluginLongId,
    ) -> InlineMacroExprPluginId<'_>;

    /// Returns [`InlineMacroExprPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Provides an override if it has been set with
    /// [`DefsGroupEx::set_override_crate_inline_macro_plugins`] or the default
    /// ([`DefsGroup::default_inline_macro_plugins`]) otherwise.
    fn crate_inline_macro_plugins<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>>;

    /// Returns the set of attributes allowed anywhere.
    /// An attribute on any item that is not in this set will be handled as an unknown attribute.
    fn allowed_attributes<'db>(&self, crate_id: CrateId<'db>) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of attributes allowed on statements.
    /// An attribute on a statement that is not in this set will be handled as an unknown attribute.
    fn allowed_statement_attributes(&self) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of `derive` that were declared as by a plugin.
    /// A derive that is not in this set will be handled as an unknown derive.
    fn declared_derives<'db>(&self, crate_id: CrateId<'db>) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of attributes that were declared as phantom type attributes by a plugin,
    /// i.e. a type marked with this attribute is considered a phantom type.
    fn declared_phantom_type_attributes<'db>(
        &self,
        crate_id: CrateId<'db>,
    ) -> Arc<OrderedHashSet<String>>;

    /// Checks whether the submodule is defined as inline.
    fn is_submodule_inline<'db>(&self, submodule_id: SubmoduleId<'db>) -> bool;

    // Module to syntax.
    /// Gets the main file of the module.
    /// A module might have more virtual files generated by plugins.
    fn module_main_file<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<FileId<'db>>;
    /// Gets all the files of a module - main files and generated virtual files.
    fn module_files<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<Arc<Vec<FileId<'db>>>>;
    /// Gets a file from a module and a FileIndex (i.e. ModuleFileId).
    fn module_file<'db>(&'db self, module_id: ModuleFileId<'db>) -> Maybe<FileId<'db>>;
    /// Gets the directory of a module.
    fn module_dir<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<Directory<'db>>;

    // File to module.
    fn crate_modules<'db>(&'db self, crate_id: CrateId<'db>) -> Arc<Vec<ModuleId<'db>>>;
    fn priv_file_to_module_mapping<'db>(
        &'db self,
    ) -> Arc<OrderedHashMap<FileId<'db>, Vec<ModuleId<'db>>>>;
    fn file_modules<'db>(&'db self, file_id: FileId<'db>) -> Maybe<Arc<Vec<ModuleId<'db>>>>;

    /// Returns the [ModuleData] of all modules in the crate's cache, and the loading data of the
    /// [DefsGroup] in the crate.
    fn cached_crate_modules<'db>(
        &'db self,
        crate_id: CrateId<'db>,
    ) -> Option<ModuleDataCacheAndLoadingData<'db>>;
    // Module level resolving.
    fn priv_module_data<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<ModuleData<'db>>;
    // Returns the information about sub-files generated by the file in the module.
    fn priv_module_sub_files<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        file_id: FileId<'db>,
    ) -> Maybe<Arc<PrivModuleSubFiles<'db>>>;
    fn module_submodules<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<SubmoduleId<'db>, ast::ItemModule<'db>>>>;
    fn module_submodules_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<SubmoduleId<'db>>>>;
    fn module_constants<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ConstantId<'db>, ast::ItemConstant<'db>>>>;
    fn module_constants_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<ConstantId<'db>>>>;
    fn module_constant_by_id<'db>(
        &'db self,
        constant_id: ConstantId<'db>,
    ) -> Maybe<ast::ItemConstant<'db>>;
    fn module_free_functions<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<FreeFunctionId<'db>, ast::FunctionWithBody<'db>>>>;
    fn module_free_functions_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<FreeFunctionId<'db>>>>;
    fn module_free_function_by_id<'db>(
        &'db self,
        free_function_id: FreeFunctionId<'db>,
    ) -> Maybe<ast::FunctionWithBody<'db>>;
    fn module_items<'db>(&'db self, module_id: ModuleId<'db>)
    -> Maybe<Arc<Vec<ModuleItemId<'db>>>>;
    fn module_global_uses<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>>>>;
    /// Returns the stable ptr of the name of a module item.
    fn module_item_name_stable_ptr<'db>(
        &'db self,
        module_id: ModuleId<'db>,
        item_id: ModuleItemId<'db>,
    ) -> Maybe<SyntaxStablePtrId<'db>>;
    fn module_uses<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<UseId<'db>, ast::UsePathLeaf<'db>>>>;
    fn module_uses_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<Arc<Vec<UseId<'db>>>>;
    fn module_use_by_id<'db>(&'db self, use_id: UseId<'db>) -> Maybe<ast::UsePathLeaf<'db>>;
    fn module_global_use_by_id<'db>(
        &'db self,
        global_use_id: GlobalUseId<'db>,
    ) -> Maybe<ast::UsePathStar<'db>>;
    fn module_structs<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StructId<'db>, ast::ItemStruct<'db>>>>;
    fn module_structs_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<StructId<'db>>>>;
    fn module_struct_by_id<'db>(&'db self, struct_id: StructId<'db>)
    -> Maybe<ast::ItemStruct<'db>>;
    fn module_enums<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<EnumId<'db>, ast::ItemEnum<'db>>>>;
    fn module_enums_ids<'db>(&'db self, module_id: ModuleId<'db>) -> Maybe<Arc<Vec<EnumId<'db>>>>;
    fn module_enum_by_id<'db>(&'db self, enum_id: EnumId<'db>) -> Maybe<ast::ItemEnum<'db>>;
    fn module_type_aliases<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ModuleTypeAliasId<'db>, ast::ItemTypeAlias<'db>>>>;
    fn module_type_aliases_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<ModuleTypeAliasId<'db>>>>;
    fn module_type_alias_by_id<'db>(
        &'db self,
        module_type_alias_id: ModuleTypeAliasId<'db>,
    ) -> Maybe<ast::ItemTypeAlias<'db>>;
    fn module_impl_aliases<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplAliasId<'db>, ast::ItemImplAlias<'db>>>>;
    fn module_impl_aliases_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<ImplAliasId<'db>>>>;
    fn module_impl_alias_by_id<'db>(
        &'db self,
        impl_alias_id: ImplAliasId<'db>,
    ) -> Maybe<ast::ItemImplAlias<'db>>;
    fn module_traits<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<TraitId<'db>, ast::ItemTrait<'db>>>>;
    fn module_traits_ids<'db>(&'db self, module_id: ModuleId<'db>)
    -> Maybe<Arc<Vec<TraitId<'db>>>>;
    fn module_trait_by_id<'db>(&'db self, trait_id: TraitId<'db>) -> Maybe<ast::ItemTrait<'db>>;
    fn module_impls<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ImplDefId<'db>, ast::ItemImpl<'db>>>>;
    fn module_impls_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<ImplDefId<'db>>>>;
    fn module_impl_by_id<'db>(&'db self, impl_id: ImplDefId<'db>) -> Maybe<ast::ItemImpl<'db>>;
    fn module_extern_types<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>>>>;
    fn module_extern_types_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<ExternTypeId<'db>>>>;
    fn module_extern_type_by_id<'db>(
        &'db self,
        extern_type_id: ExternTypeId<'db>,
    ) -> Maybe<ast::ItemExternType<'db>>;
    fn module_extern_functions<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>>>>;
    fn module_extern_functions_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<ExternFunctionId<'db>>>>;
    fn module_extern_function_by_id<'db>(
        &'db self,
        extern_function_id: ExternFunctionId<'db>,
    ) -> Maybe<ast::ItemExternFunction<'db>>;
    /// Returns the macro declarations in the module.
    fn module_macro_declarations<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>>>>;
    /// Returns the IDs of the macro declarations in the module.
    fn module_macro_declarations_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<MacroDeclarationId<'db>>>>;
    /// Returns the macro declaration by its ID.
    fn module_macro_declaration_by_id<'db>(
        &'db self,
        macro_declaration_id: MacroDeclarationId<'db>,
    ) -> Maybe<ast::ItemMacroDeclaration<'db>>;
    /// Returns the macro calls in the module.
    fn module_macro_calls<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>>>>;
    /// Returns the IDs of the macro calls in the module.
    fn module_macro_calls_ids<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<MacroCallId<'db>>>>;
    /// Returns the macro call by its ID.
    fn module_macro_call_by_id<'db>(
        &'db self,
        macro_call_id: MacroCallId<'db>,
    ) -> Maybe<ast::ItemInlineMacro<'db>>;
    fn module_ancestors<'db>(&'db self, module_id: ModuleId<'db>) -> OrderedHashSet<ModuleId<'db>>;
    fn module_generated_file_aux_data<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<[Option<DynGeneratedFileAuxData>]>>;
    fn module_plugin_diagnostics<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<Vec<(ModuleFileId<'db>, PluginDiagnostic<'db>)>>>;
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    fn module_plugin_diagnostics_notes<'db>(
        &'db self,
        module_id: ModuleId<'db>,
    ) -> Maybe<Arc<PluginFileDiagnosticNotes<'db>>>;
}

/// Initializes the [`DefsGroup`] database to a proper state.
pub fn init_defs_group(db: &mut dyn DefsGroup) {
    db.set_macro_plugin_overrides_input(Arc::new(OrderedHashMap::default()));
    db.set_inline_macro_plugin_overrides_input(Arc::new(OrderedHashMap::default()));
}

pub fn default_macro_plugins<'db>(db: &'db dyn DefsGroup) -> Arc<Vec<MacroPluginId<'db>>> {
    Arc::new(
        db.default_macro_plugins_input()
            .iter()
            .map(|plugin| db.intern_macro_plugin(plugin.clone()))
            .collect(),
    )
}

pub fn macro_plugin_overrides<'db>(
    db: &'db dyn DefsGroup,
) -> Arc<OrderedHashMap<CrateId<'db>, Arc<Vec<MacroPluginId<'db>>>>> {
    let inp = db.macro_plugin_overrides_input();
    Arc::new(
        inp.iter()
            .map(|(crate_id, plugins)| {
                (
                    crate_id.clone().into_crate_long_id(db).intern(db),
                    Arc::new(plugins.iter().map(|plugin| plugin.clone().intern(db)).collect()),
                )
            })
            .collect(),
    )
}

pub fn inline_macro_plugin_overrides<'db>(
    db: &'db dyn DefsGroup,
) -> Arc<OrderedHashMap<CrateId<'db>, Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>>>> {
    let inp = db.inline_macro_plugin_overrides_input();
    Arc::new(
        inp.iter()
            .map(|(crate_id, plugins)| {
                (
                    crate_id.clone().into_crate_long_id(db).intern(db),
                    Arc::new(
                        plugins
                            .iter()
                            .map(|(name, plugin)| (name.clone(), plugin.clone().intern(db)))
                            .collect(),
                    ),
                )
            })
            .collect(),
    )
}

pub fn default_inline_macro_plugins<'db>(
    db: &'db dyn DefsGroup,
) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>> {
    let inp: Arc<OrderedHashMap<String, InlineMacroExprPluginLongId>> =
        db.default_inline_macro_plugins_input();
    Arc::new(inp.iter().map(|(name, plugin)| (name.clone(), plugin.clone().intern(db))).collect())
}

fn crate_macro_plugins<'db>(
    db: &'db dyn DefsGroup,
    crate_id: CrateId<'db>,
) -> Arc<Vec<MacroPluginId<'db>>> {
    macro_plugin_overrides(db).get(&crate_id).cloned().unwrap_or_else(|| db.default_macro_plugins())
}

fn crate_inline_macro_plugins<'db>(
    db: &'db dyn DefsGroup,
    crate_id: CrateId<'db>,
) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>> {
    db.inline_macro_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_inline_macro_plugins())
}

fn allowed_attributes<'db>(
    db: &dyn DefsGroup,
    crate_id: CrateId<'db>,
) -> Arc<OrderedHashSet<String>> {
    let base_attrs = [
        INLINE_ATTR,
        MUST_USE_ATTR,
        UNSTABLE_ATTR,
        DEPRECATED_ATTR,
        INTERNAL_ATTR,
        ALLOW_ATTR,
        ALLOW_ATTR_ATTR,
        FEATURE_ATTR,
        PHANTOM_ATTR,
        IMPLICIT_PRECEDENCE_ATTR,
        FMT_SKIP_ATTR,
        // TODO(orizi): Remove this once `starknet` is removed from corelib.
        STARKNET_INTERFACE_ATTR,
    ];

    let crate_plugins = db.crate_macro_plugins(crate_id);

    Arc::new(OrderedHashSet::from_iter(chain!(
        base_attrs.map(|attr| attr.into()),
        crate_plugins
            .iter()
            .flat_map(|plugin| db.lookup_intern_macro_plugin(*plugin).declared_attributes())
    )))
}

fn allowed_statement_attributes(_db: &dyn DefsGroup) -> Arc<OrderedHashSet<String>> {
    let all_attributes = [FMT_SKIP_ATTR, ALLOW_ATTR, FEATURE_ATTR];
    Arc::new(OrderedHashSet::from_iter(all_attributes.map(|attr| attr.into())))
}

fn declared_derives<'db>(
    db: &dyn DefsGroup,
    crate_id: CrateId<'db>,
) -> Arc<OrderedHashSet<String>> {
    Arc::new(OrderedHashSet::from_iter(
        db.crate_macro_plugins(crate_id)
            .iter()
            .flat_map(|plugin| db.lookup_intern_macro_plugin(*plugin).declared_derives()),
    ))
}

fn declared_phantom_type_attributes<'db>(
    db: &dyn DefsGroup,
    crate_id: CrateId<'db>,
) -> Arc<OrderedHashSet<String>> {
    let crate_plugins = db.crate_macro_plugins(crate_id);

    Arc::new(OrderedHashSet::from_iter(chain!(
        [PHANTOM_ATTR.into()],
        crate_plugins
            .iter()
            .flat_map(|plugin| db.lookup_intern_macro_plugin(*plugin).phantom_type_attributes())
    )))
}

fn is_submodule_inline<'db>(db: &dyn DefsGroup, submodule_id: SubmoduleId<'db>) -> bool {
    match submodule_id.stable_ptr(db).lookup(db).body(db) {
        MaybeModuleBody::Some(_) => true,
        MaybeModuleBody::None(_) => false,
    }
}

fn module_main_file<'db>(db: &'db dyn DefsGroup, module_id: ModuleId<'db>) -> Maybe<FileId<'db>> {
    Ok(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe()?.root.file(db, "lib.cairo")
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            if db.is_submodule_inline(submodule_id) {
                // This is an inline module, we return the file where the inline module was
                // defined. It can be either the file of the parent module
                // or a plugin-generated virtual file.
                db.module_file(submodule_id.module_file_id(db))?
            } else {
                let name = format!("{}.cairo", submodule_id.name(db));
                db.module_dir(parent)?.file(db, &name)
            }
        }
        ModuleId::MacroCall { generated_file_id, .. } => {
            // This is a macro-generated module, so the main file is the generated file.
            generated_file_id
        }
    })
}

fn module_files<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<FileId<'db>>>> {
    Ok(Arc::new(db.priv_module_data(module_id)?.files.to_vec()))
}

fn module_file<'db>(
    db: &'db dyn DefsGroup,
    module_file_id: ModuleFileId<'db>,
) -> Maybe<FileId<'db>> {
    Ok(db.module_files(module_file_id.0)?[module_file_id.1.0])
}

fn module_dir<'db>(db: &'db dyn DefsGroup, module_id: ModuleId<'db>) -> Maybe<Directory<'db>> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe().map(|config| config.root.clone())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            let name = submodule_id.name(db);
            Ok(db.module_dir(parent)?.subdir(name))
        }
        ModuleId::MacroCall { id: macro_call_id, .. } => {
            // This is a macro call, we return the directory for the file that contained the macro
            // call, as it is considered the location of the macro itself.
            db.module_dir(macro_call_id.module_file_id(db).0)
        }
    }
}

/// Appends all the modules under the given module, including nested modules.
fn collect_modules_under<'db>(
    db: &'db dyn DefsGroup,
    modules: &mut Vec<ModuleId<'db>>,
    module_id: ModuleId<'db>,
) {
    modules.push(module_id);
    if let Ok(submodule_ids) = db.module_submodules_ids(module_id) {
        for submodule_module_id in submodule_ids.iter().copied() {
            collect_modules_under(db, modules, ModuleId::Submodule(submodule_module_id));
        }
    }
}

/// Returns all the modules in the crate, including recursively.
fn crate_modules<'db>(db: &'db dyn DefsGroup, crate_id: CrateId<'db>) -> Arc<Vec<ModuleId<'db>>> {
    let mut modules = Vec::new();
    collect_modules_under(db, &mut modules, ModuleId::CrateRoot(crate_id));
    Arc::new(modules)
}

fn priv_file_to_module_mapping<'db>(
    db: &'db dyn DefsGroup,
) -> Arc<OrderedHashMap<FileId<'db>, Vec<ModuleId<'db>>>> {
    let mut mapping = OrderedHashMap::<FileId<'db>, Vec<ModuleId<'db>>>::default();
    for crate_id in db.crates() {
        for module_id in db.crate_modules(*crate_id).iter().copied() {
            if let Ok(files) = db.module_files(module_id) {
                for file_id in files.iter().copied() {
                    match mapping.get_mut(&file_id) {
                        Some(file_modules) => {
                            file_modules.push(module_id);
                        }
                        None => {
                            mapping.insert(file_id, vec![module_id]);
                        }
                    }
                }
            }
        }
    }
    mapping.into()
}
fn file_modules<'db>(
    db: &'db dyn DefsGroup,
    file_id: FileId<'db>,
) -> Maybe<Arc<Vec<ModuleId<'db>>>> {
    Ok(Arc::new(db.priv_file_to_module_mapping().get(&file_id).to_maybe()?.clone()))
}

#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct ModuleData<'db> {
    /// The list of IDs of all items in the module. Each ID here is guaranteed to be a key in one
    /// of the specific-item-kind maps.
    pub(crate) items: Arc<Vec<ModuleItemId<'db>>>,

    // Specific-item-kind maps
    pub(crate) constants: Arc<OrderedHashMap<ConstantId<'db>, ast::ItemConstant<'db>>>,
    pub(crate) submodules: Arc<OrderedHashMap<SubmoduleId<'db>, ast::ItemModule<'db>>>,
    pub(crate) uses: Arc<OrderedHashMap<UseId<'db>, ast::UsePathLeaf<'db>>>,
    pub(crate) free_functions: Arc<OrderedHashMap<FreeFunctionId<'db>, ast::FunctionWithBody<'db>>>,
    pub(crate) structs: Arc<OrderedHashMap<StructId<'db>, ast::ItemStruct<'db>>>,
    pub(crate) enums: Arc<OrderedHashMap<EnumId<'db>, ast::ItemEnum<'db>>>,
    pub(crate) type_aliases: Arc<OrderedHashMap<ModuleTypeAliasId<'db>, ast::ItemTypeAlias<'db>>>,
    pub(crate) impl_aliases: Arc<OrderedHashMap<ImplAliasId<'db>, ast::ItemImplAlias<'db>>>,
    pub(crate) traits: Arc<OrderedHashMap<TraitId<'db>, ast::ItemTrait<'db>>>,
    pub(crate) impls: Arc<OrderedHashMap<ImplDefId<'db>, ast::ItemImpl<'db>>>,
    pub(crate) extern_types: Arc<OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>>>,
    pub(crate) extern_functions:
        Arc<OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>>>,
    pub(crate) macro_declarations:
        Arc<OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>>>,
    pub(crate) global_uses: Arc<OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>>>,
    /// Calls to inline macros in the module (only those that were not handled by plugins).
    pub(crate) macro_calls: Arc<OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>>>,
    pub(crate) files: Arc<Vec<FileId<'db>>>,
    /// Generation info for each file. Virtual files have Some. Other files have None.
    pub(crate) generated_file_aux_data: Arc<Vec<Option<DynGeneratedFileAuxData>>>,
    pub(crate) plugin_diagnostics: Arc<Vec<(ModuleFileId<'db>, PluginDiagnostic<'db>)>>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    pub(crate) diagnostics_notes: PluginFileDiagnosticNotes<'db>,
}

/// Information about generated files from running on a module file.
#[derive(Clone, Debug, Eq, PartialEq, salsa::Update)]
pub struct PrivModuleSubFiles<'db> {
    /// The files generated by plugins running on items.
    files: OrderedHashMap<FileId<'db>, VirtualFile<'db>>,
    /// The aux data per such file.
    aux_data: Vec<Option<DynGeneratedFileAuxData>>,
    /// The items not filtered out by plugins.
    items: Vec<ast::ModuleItem<'db>>,
    /// The diagnostics generated by the plugins.
    plugin_diagnostics: Vec<PluginDiagnostic<'db>>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    diagnostics_notes: PluginFileDiagnosticNotes<'db>,
}

fn priv_module_data<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<ModuleData<'db>> {
    let crate_id = module_id.owning_crate(db);

    if let Some((map, _)) = db.cached_crate_modules(crate_id) {
        if let Some(module_data) = map.get(&module_id) {
            return Ok(module_data.clone());
        } else {
            panic!("module not found in cached modules_data {:?}", module_id.name(db));
        }
    };

    let module_file = db.module_main_file(module_id)?;
    let main_file_aux_data = if let ModuleId::Submodule(submodule_id) = module_id {
        let parent_module_data = db.priv_module_data(submodule_id.parent_module(db))?;
        let item_module_ast = &parent_module_data.submodules[&submodule_id];
        if matches!(item_module_ast.body(db), MaybeModuleBody::Some(_)) {
            // TODO(spapini): Diagnostics in this module that get mapped to parent module
            // should lie in that modules ModuleData, or somehow collected by its
            // diagnostics collector function.

            // If this is an inline module, copy its generation file info from the parent
            // module, from the file where this submodule was defined.
            parent_module_data.generated_file_aux_data[submodule_id.file_index(db).0].clone()
        } else {
            None
        }
    } else {
        None
    };
    let mut file_queue = VecDeque::new();
    file_queue.push_back(module_file);
    let mut constants = OrderedHashMap::default();
    let mut submodules = OrderedHashMap::default();
    let mut uses = OrderedHashMap::default();
    let mut free_functions = OrderedHashMap::default();
    let mut structs = OrderedHashMap::default();
    let mut enums = OrderedHashMap::default();
    let mut type_aliases = OrderedHashMap::default();
    let mut impl_aliases = OrderedHashMap::default();
    let mut traits = OrderedHashMap::default();
    let mut impls = OrderedHashMap::default();
    let mut extern_types = OrderedHashMap::default();
    let mut extern_functions = OrderedHashMap::default();
    let mut macro_declarations = OrderedHashMap::default();
    let mut macro_calls = OrderedHashMap::default();
    let mut global_uses = OrderedHashMap::default();
    let mut aux_data = Vec::new();
    let mut files = Vec::new();
    let mut plugin_diagnostics = Vec::new();
    let mut diagnostics_notes = OrderedHashMap::default();

    let mut items = vec![];
    aux_data.push(main_file_aux_data);
    while let Some(file_id) = file_queue.pop_front() {
        let file_index = FileIndex(files.len());
        let module_file_id = ModuleFileId(module_id, file_index);
        files.push(file_id);

        let priv_module_data = db.priv_module_sub_files(module_id, file_id)?;
        diagnostics_notes.extend(priv_module_data.diagnostics_notes.clone().into_iter());
        file_queue.extend(priv_module_data.files.keys().copied());
        for diag in &priv_module_data.plugin_diagnostics {
            plugin_diagnostics.push((module_file_id, diag.clone()));
        }
        aux_data.extend(priv_module_data.aux_data.iter().cloned());
        for item_ast in &priv_module_data.items {
            match item_ast.clone() {
                ast::ModuleItem::Constant(constant) => {
                    let item_id =
                        ConstantLongId(module_file_id, constant.stable_ptr(db)).intern(db);
                    constants.insert(item_id, constant);
                    items.push(ModuleItemId::Constant(item_id));
                }
                ast::ModuleItem::Module(module) => {
                    let item_id = SubmoduleLongId(module_file_id, module.stable_ptr(db)).intern(db);
                    submodules.insert(item_id, module);
                    items.push(ModuleItemId::Submodule(item_id));
                }
                ast::ModuleItem::Use(us) => {
                    for leaf in get_all_path_leaves(db, &us) {
                        let id = UseLongId(module_file_id, leaf.stable_ptr(db)).intern(db);
                        uses.insert(id, leaf);
                        items.push(ModuleItemId::Use(id));
                    }
                    for star in get_all_path_stars(db, &us) {
                        let id = GlobalUseLongId(module_file_id, star.stable_ptr(db)).intern(db);
                        global_uses.insert(id, star);
                    }
                }
                ast::ModuleItem::FreeFunction(function) => {
                    let item_id =
                        FreeFunctionLongId(module_file_id, function.stable_ptr(db)).intern(db);
                    free_functions.insert(item_id, function);
                    items.push(ModuleItemId::FreeFunction(item_id));
                }
                ast::ModuleItem::ExternFunction(extern_function) => {
                    let item_id =
                        ExternFunctionLongId(module_file_id, extern_function.stable_ptr(db))
                            .intern(db);
                    extern_functions.insert(item_id, extern_function);
                    items.push(ModuleItemId::ExternFunction(item_id));
                }
                ast::ModuleItem::ExternType(extern_type) => {
                    let item_id =
                        ExternTypeLongId(module_file_id, extern_type.stable_ptr(db)).intern(db);
                    extern_types.insert(item_id, extern_type);
                    items.push(ModuleItemId::ExternType(item_id));
                }
                ast::ModuleItem::Trait(trt) => {
                    let item_id = TraitLongId(module_file_id, trt.stable_ptr(db)).intern(db);
                    traits.insert(item_id, trt);
                    items.push(ModuleItemId::Trait(item_id));
                }
                ast::ModuleItem::Impl(imp) => {
                    let item_id = ImplDefLongId(module_file_id, imp.stable_ptr(db)).intern(db);
                    impls.insert(item_id, imp);
                    items.push(ModuleItemId::Impl(item_id));
                }
                ast::ModuleItem::Struct(structure) => {
                    let item_id = StructLongId(module_file_id, structure.stable_ptr(db)).intern(db);
                    structs.insert(item_id, structure);
                    items.push(ModuleItemId::Struct(item_id));
                }
                ast::ModuleItem::Enum(enm) => {
                    let item_id = EnumLongId(module_file_id, enm.stable_ptr(db)).intern(db);
                    enums.insert(item_id, enm);
                    items.push(ModuleItemId::Enum(item_id));
                }
                ast::ModuleItem::TypeAlias(type_alias) => {
                    let item_id =
                        ModuleTypeAliasLongId(module_file_id, type_alias.stable_ptr(db)).intern(db);
                    type_aliases.insert(item_id, type_alias);
                    items.push(ModuleItemId::TypeAlias(item_id));
                }
                ast::ModuleItem::ImplAlias(impl_alias) => {
                    let item_id =
                        ImplAliasLongId(module_file_id, impl_alias.stable_ptr(db)).intern(db);
                    impl_aliases.insert(item_id, impl_alias);
                    items.push(ModuleItemId::ImplAlias(item_id));
                }
                ast::ModuleItem::MacroDeclaration(macro_declaration) => {
                    let item_id =
                        MacroDeclarationLongId(module_file_id, macro_declaration.stable_ptr(db))
                            .intern(db);
                    macro_declarations.insert(item_id, macro_declaration);
                    items.push(ModuleItemId::MacroDeclaration(item_id));
                }
                ast::ModuleItem::InlineMacro(inline_macro_ast) => {
                    let item_id =
                        MacroCallLongId(module_file_id, inline_macro_ast.stable_ptr(db)).intern(db);
                    macro_calls.insert(item_id, inline_macro_ast.clone());
                }
                ast::ModuleItem::HeaderDoc(_) => {}
                ast::ModuleItem::Missing(_) => {}
            }
        }
    }
    let res = ModuleData {
        items: items.into(),
        constants: constants.into(),
        submodules: submodules.into(),
        uses: uses.into(),
        free_functions: free_functions.into(),
        structs: structs.into(),
        enums: enums.into(),
        type_aliases: type_aliases.into(),
        impl_aliases: impl_aliases.into(),
        traits: traits.into(),
        impls: impls.into(),
        extern_types: extern_types.into(),
        extern_functions: extern_functions.into(),
        macro_declarations: macro_declarations.into(),
        macro_calls: macro_calls.into(),
        global_uses: global_uses.into(),
        files: files.into(),
        generated_file_aux_data: aux_data.into(),
        plugin_diagnostics: plugin_diagnostics.into(),
        diagnostics_notes,
    };
    Ok(res)
}

pub type ModuleDataCacheAndLoadingData<'db> =
    (Arc<OrderedHashMap<ModuleId<'db>, ModuleData<'db>>>, Arc<DefCacheLoadingData<'db>>);

fn cached_crate_modules<'db>(
    db: &'db dyn DefsGroup,
    crate_id: CrateId<'db>,
) -> Option<ModuleDataCacheAndLoadingData<'db>> {
    load_cached_crate_modules(db, crate_id)
}

pub fn init_external_files(db: &mut dyn DefsGroup) {
    let try_ext_as_virtual_impl: TryExtAsVirtual =
        Arc::new(|db: &dyn Database, external_id: salsa::Id| {
            // TODO(eytan-starkware): Once everything is &dyn Database, remove the unsafe cast.
            let defs_db: &dyn DefsGroup = unsafe { std::mem::transmute(db) };
            try_ext_as_virtual_impl(defs_db, external_id)
        });
    let new_ext_as_virtual = ExternalFiles::create_ext_as_virtual(try_ext_as_virtual_impl.clone());
    ExternalFiles::replace_existing(db, new_ext_as_virtual, try_ext_as_virtual_impl);
}

/// Returns the `VirtualFile` matching the given external id.
pub fn try_ext_as_virtual_impl<'db>(
    db: &'db dyn DefsGroup,
    external_id: salsa::Id,
) -> Option<VirtualFile<'db>> {
    let long_id = PluginGeneratedFileId::from_intern_id(external_id).long(db);
    let file_id = FileLongId::External(external_id).intern(db);
    let data = db.priv_module_sub_files(long_id.module_id, long_id.stable_ptr.file_id(db)).unwrap();
    data.files.get(&file_id).cloned()
}

fn priv_module_sub_files<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
    file_id: FileId<'db>,
) -> Maybe<Arc<PrivModuleSubFiles<'db>>> {
    let module_main_file = db.module_main_file(module_id)?;
    let file_syntax = db.file_module_syntax(file_id)?;
    let item_asts = if module_main_file == file_id {
        if let ModuleId::Submodule(submodule_id) = module_id {
            let data = db.priv_module_data(submodule_id.parent_module(db))?;
            if let MaybeModuleBody::Some(body) = data.submodules[&submodule_id].body(db) {
                Some(body.items(db))
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
    .unwrap_or_else(|| file_syntax.items(db));

    let crate_id = module_id.owning_crate(db);

    let allowed_attributes = db.allowed_attributes(crate_id);
    // TODO(orizi): Actually extract the allowed features per module.
    let allowed_features = Default::default();

    let cfg_set = db
        .crate_config(crate_id)
        .and_then(|cfg| cfg.settings.cfg_set.clone().map(Arc::new))
        .unwrap_or(db.cfg_set());
    let edition = db
        .crate_config(module_id.owning_crate(db))
        .map(|cfg| cfg.settings.edition)
        .unwrap_or_default();
    let metadata = MacroPluginMetadata {
        cfg_set: &cfg_set,
        declared_derives: &db.declared_derives(crate_id),
        allowed_features: &allowed_features,
        edition,
    };

    let mut files = OrderedHashMap::<_, _>::default();
    let mut aux_data = Vec::new();
    let mut items = Vec::new();
    let mut plugin_diagnostics = Vec::new();
    let mut diagnostics_notes = OrderedHashMap::default();
    for item_ast in item_asts.elements(db) {
        let mut remove_original_item = false;
        // Iterate the plugins by their order. The first one to change something (either
        // generate new code, remove the original code, or both), breaks the loop. If more
        // plugins might have act on the item, they can do it on the generated code.
        for plugin_id in db.crate_macro_plugins(crate_id).iter() {
            let plugin = db.lookup_intern_macro_plugin(*plugin_id);

            let result = plugin.generate_code(db, item_ast.clone(), &metadata);
            plugin_diagnostics.extend(result.diagnostics);
            if result.remove_original_item {
                remove_original_item = true;
            }

            if let Some(generated) = result.code {
                let generated_file_id = FileLongId::External(
                    PluginGeneratedFileLongId {
                        module_id,
                        stable_ptr: item_ast.stable_ptr(db).untyped(),
                        name: generated.name.clone(),
                    }
                    .intern(db)
                    .as_intern_id(),
                )
                .intern(db);
                if let Some(text) = generated.diagnostics_note {
                    diagnostics_notes
                        .insert(generated_file_id, DiagnosticNote { text, location: None });
                }
                files.insert(
                    generated_file_id,
                    VirtualFile {
                        parent: Some(file_id),
                        name: generated.name,
                        content: generated.content.into(),
                        code_mappings: generated.code_mappings.into(),
                        kind: FileKind::Module,
                        original_item_removed: remove_original_item,
                    },
                );
                aux_data.push(generated.aux_data);
            }
            if remove_original_item {
                break;
            }
        }
        if remove_original_item {
            // Don't add the original item to the module data.
            continue;
        }
        validate_attributes(db, &allowed_attributes, &item_ast, &mut plugin_diagnostics);
        items.push(item_ast);
    }
    let res = PrivModuleSubFiles { files, aux_data, items, plugin_diagnostics, diagnostics_notes };
    Ok(res.into())
}

/// Collects attributes allowed by `allow_attr` attribute.
fn collect_extra_allowed_attributes<'db>(
    db: &'db dyn SyntaxGroup,
    item: &impl QueryAttrs<'db>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> OrderedHashSet<String> {
    let mut extra_allowed_attributes = OrderedHashSet::default();
    for attr in item.query_attr(db, ALLOW_ATTR_ATTR) {
        let args = attr.clone().structurize(db).args;
        if args.is_empty() {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Expected arguments.".to_string(),
            ));
            continue;
        }
        for arg in args {
            if let Some(ast::Expr::Path(path)) = try_extract_unnamed_arg(db, &arg.arg) {
                if let Some([ast::PathSegment::Simple(segment)]) =
                    path.segments(db).elements(db).collect_array()
                {
                    extra_allowed_attributes.insert(segment.ident(db).text(db).into());
                    continue;
                }
            }
            plugin_diagnostics.push(PluginDiagnostic::error(
                arg.arg.stable_ptr(db),
                "Expected simple identifier.".to_string(),
            ));
        }
    }
    extra_allowed_attributes
}

/// Validates that all attributes on the given item are in the allowed set or adds diagnostics.
pub fn validate_attributes_flat<'db>(
    db: &'db dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    extra_allowed_attributes: &OrderedHashSet<String>,
    item: &impl QueryAttrs<'db>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    let local_extra_attributes = collect_extra_allowed_attributes(db, item, plugin_diagnostics);
    for attr in item.attributes_elements(db) {
        let attr_text = attr.attr(db).as_syntax_node().get_text_without_trivia(db);
        if !(allowed_attributes.contains(attr_text)
            || extra_allowed_attributes.contains(attr_text)
            || local_extra_attributes.contains(attr_text))
        {
            plugin_diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                "Unsupported attribute.".to_string(),
            ));
        }
    }
}

/// Validates that all attributes on all items in the given element list are in the allowed set or
/// adds diagnostics.
fn validate_attributes_element_list<'db, Item: QueryAttrs<'db> + TypedSyntaxNode<'db>>(
    db: &'db dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    extra_allowed_attributes: &OrderedHashSet<String>,
    items: impl Iterator<Item = Item>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    for item in items {
        validate_attributes_flat(
            db,
            allowed_attributes,
            extra_allowed_attributes,
            &item,
            plugin_diagnostics,
        );
    }
}

/// Validates that all attributes on an item and on items contained within it are in the allowed set
/// or adds diagnostics.
fn validate_attributes<'db>(
    db: &'db dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    item_ast: &ast::ModuleItem<'db>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) {
    let extra_allowed_attributes =
        collect_extra_allowed_attributes(db, item_ast, plugin_diagnostics);
    validate_attributes_flat(
        db,
        allowed_attributes,
        &extra_allowed_attributes,
        item_ast,
        plugin_diagnostics,
    );

    match item_ast {
        ast::ModuleItem::Trait(item) => {
            if let ast::MaybeTraitBody::Some(body) = item.body(db) {
                validate_attributes_element_list(
                    db,
                    allowed_attributes,
                    &extra_allowed_attributes,
                    body.items(db).elements(db),
                    plugin_diagnostics,
                );
            }
        }
        ast::ModuleItem::Impl(item) => {
            if let ast::MaybeImplBody::Some(body) = item.body(db) {
                validate_attributes_element_list(
                    db,
                    allowed_attributes,
                    &extra_allowed_attributes,
                    body.items(db).elements(db),
                    plugin_diagnostics,
                );
            }
        }
        ast::ModuleItem::Struct(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                &extra_allowed_attributes,
                item.members(db).elements(db),
                plugin_diagnostics,
            );
        }
        ast::ModuleItem::Enum(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                &extra_allowed_attributes,
                item.variants(db).elements(db),
                plugin_diagnostics,
            );
        }
        _ => {}
    }
}

/// Returns all the path leaves under a given use item.
pub fn get_all_path_leaves<'db>(
    db: &'db dyn SyntaxGroup,
    use_item: &ast::ItemUse<'db>,
) -> Vec<ast::UsePathLeaf<'db>> {
    let mut res = vec![];
    let mut stack = vec![use_item.use_path(db)];
    while let Some(use_path) = stack.pop() {
        match use_path {
            ast::UsePath::Leaf(use_path) => res.push(use_path),
            ast::UsePath::Single(use_path) => stack.push(use_path.use_path(db)),
            ast::UsePath::Multi(use_path) => {
                stack.extend(use_path.use_paths(db).elements(db).rev())
            }
            ast::UsePath::Star(_) => {}
        }
    }
    res
}

/// Returns all the path stars under a given use item.
pub fn get_all_path_stars<'db>(
    db: &'db dyn SyntaxGroup,
    use_item: &ast::ItemUse<'db>,
) -> Vec<ast::UsePathStar<'db>> {
    let mut res = vec![];
    let mut stack = vec![use_item.use_path(db)];
    while let Some(use_path) = stack.pop() {
        match use_path {
            ast::UsePath::Leaf(_) => {}
            ast::UsePath::Single(use_path) => stack.push(use_path.use_path(db)),
            ast::UsePath::Multi(use_path) => {
                stack.extend(use_path.use_paths(db).elements(db).rev())
            }
            ast::UsePath::Star(use_path) => res.push(use_path),
        }
    }
    res
}

/// Returns all the constant definitions of the given module.
pub fn module_constants<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<ConstantId<'db>, ast::ItemConstant<'db>>>> {
    Ok(db.priv_module_data(module_id)?.constants)
}
pub fn module_constants_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ConstantId<'db>>>> {
    Ok(Arc::new(db.module_constants(module_id)?.keys().copied().collect_vec()))
}
pub fn module_constant_by_id<'db>(
    db: &'db dyn DefsGroup,
    constant_id: ConstantId<'db>,
) -> Maybe<ast::ItemConstant<'db>> {
    let module_constants = db.module_constants(constant_id.module_file_id(db).0)?;
    Ok(module_constants[&constant_id].clone())
}

/// Returns all the *direct* submodules of the given module - including those generated by macro
/// plugins. To get all the submodules including nested modules, use [`collect_modules_under`].
fn module_submodules<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<SubmoduleId<'db>, ast::ItemModule<'db>>>> {
    Ok(db.priv_module_data(module_id)?.submodules)
}
fn module_submodules_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<SubmoduleId<'db>>>> {
    Ok(Arc::new(db.module_submodules(module_id)?.keys().copied().collect_vec()))
}
pub fn module_submodule_by_id<'db>(
    db: &'db dyn DefsGroup,
    submodule_id: SubmoduleId<'db>,
) -> Maybe<ast::ItemModule<'db>> {
    let module_submodules = db.module_submodules(submodule_id.module_file_id(db).0)?;
    Ok(module_submodules[&submodule_id].clone())
}

/// Returns all the free functions of the given module.
pub fn module_free_functions<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<FreeFunctionId<'db>, ast::FunctionWithBody<'db>>>> {
    Ok(db.priv_module_data(module_id)?.free_functions)
}
pub fn module_free_functions_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<FreeFunctionId<'db>>>> {
    Ok(Arc::new(db.module_free_functions(module_id)?.keys().copied().collect_vec()))
}
pub fn module_free_function_by_id<'db>(
    db: &'db dyn DefsGroup,
    free_function_id: FreeFunctionId<'db>,
) -> Maybe<ast::FunctionWithBody<'db>> {
    let module_free_functions = db.module_free_functions(free_function_id.module_file_id(db).0)?;
    Ok(module_free_functions[&free_function_id].clone())
}

/// Returns all the uses of the given module.
pub fn module_uses<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<UseId<'db>, ast::UsePathLeaf<'db>>>> {
    Ok(db.priv_module_data(module_id)?.uses)
}
pub fn module_uses_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<UseId<'db>>>> {
    Ok(Arc::new(db.module_uses(module_id)?.keys().copied().collect_vec()))
}
pub fn module_use_by_id<'db>(
    db: &'db dyn DefsGroup,
    use_id: UseId<'db>,
) -> Maybe<ast::UsePathLeaf<'db>> {
    let module_uses = db.module_uses(use_id.module_file_id(db).0)?;
    Ok(module_uses[&use_id].clone())
}

/// Returns the `use *` of the given module, by its ID.
pub fn module_global_use_by_id<'db>(
    db: &'db dyn DefsGroup,
    global_use_id: GlobalUseId<'db>,
) -> Maybe<ast::UsePathStar<'db>> {
    let module_global_uses = db.module_global_uses(global_use_id.module_file_id(db).0)?;
    Ok(module_global_uses[&global_use_id].clone())
}

/// Returns all the structs of the given module.
pub fn module_structs<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<StructId<'db>, ast::ItemStruct<'db>>>> {
    Ok(db.priv_module_data(module_id)?.structs)
}
pub fn module_structs_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<StructId<'db>>>> {
    Ok(Arc::new(db.module_structs(module_id)?.keys().copied().collect_vec()))
}
pub fn module_struct_by_id<'db>(
    db: &'db dyn DefsGroup,
    struct_id: StructId<'db>,
) -> Maybe<ast::ItemStruct<'db>> {
    let module_structs = db.module_structs(struct_id.module_file_id(db).0)?;
    Ok(module_structs[&struct_id].clone())
}

/// Returns all the enums of the given module.
pub fn module_enums<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<EnumId<'db>, ast::ItemEnum<'db>>>> {
    Ok(db.priv_module_data(module_id)?.enums)
}
pub fn module_enums_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<EnumId<'db>>>> {
    Ok(Arc::new(db.module_enums(module_id)?.keys().copied().collect_vec()))
}
pub fn module_enum_by_id<'db>(
    db: &'db dyn DefsGroup,
    enum_id: EnumId<'db>,
) -> Maybe<ast::ItemEnum<'db>> {
    let module_enums = db.module_enums(enum_id.module_file_id(db).0)?;
    Ok(module_enums[&enum_id].clone())
}

/// Returns all the type aliases of the given module.
pub fn module_type_aliases<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<ModuleTypeAliasId<'db>, ast::ItemTypeAlias<'db>>>> {
    Ok(db.priv_module_data(module_id)?.type_aliases)
}
pub fn module_type_aliases_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ModuleTypeAliasId<'db>>>> {
    Ok(Arc::new(db.module_type_aliases(module_id)?.keys().copied().collect_vec()))
}
pub fn module_type_alias_by_id<'db>(
    db: &'db dyn DefsGroup,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<ast::ItemTypeAlias<'db>> {
    let module_type_aliases = db.module_type_aliases(module_type_alias_id.module_file_id(db).0)?;
    Ok(module_type_aliases[&module_type_alias_id].clone())
}

/// Returns all the impl aliases of the given module.
pub fn module_impl_aliases<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<ImplAliasId<'db>, ast::ItemImplAlias<'db>>>> {
    Ok(db.priv_module_data(module_id)?.impl_aliases)
}
pub fn module_impl_aliases_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ImplAliasId<'db>>>> {
    Ok(Arc::new(db.module_impl_aliases(module_id)?.keys().copied().collect_vec()))
}
pub fn module_impl_alias_by_id<'db>(
    db: &'db dyn DefsGroup,
    impl_alias_id: ImplAliasId<'db>,
) -> Maybe<ast::ItemImplAlias<'db>> {
    let module_impl_aliases = db.module_impl_aliases(impl_alias_id.module_file_id(db).0)?;
    Ok(module_impl_aliases[&impl_alias_id].clone())
}

/// Returns all the traits of the given module.
pub fn module_traits<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<TraitId<'db>, ast::ItemTrait<'db>>>> {
    Ok(db.priv_module_data(module_id)?.traits)
}
pub fn module_traits_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<TraitId<'db>>>> {
    Ok(Arc::new(db.module_traits(module_id)?.keys().copied().collect_vec()))
}
pub fn module_trait_by_id<'db>(
    db: &'db dyn DefsGroup,
    trait_id: TraitId<'db>,
) -> Maybe<ast::ItemTrait<'db>> {
    let module_traits = db.module_traits(trait_id.module_file_id(db).0)?;
    Ok(module_traits[&trait_id].clone())
}

/// Returns all the impls of the given module.
pub fn module_impls<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<ImplDefId<'db>, ast::ItemImpl<'db>>>> {
    Ok(db.priv_module_data(module_id)?.impls)
}
pub fn module_impls_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ImplDefId<'db>>>> {
    Ok(Arc::new(db.module_impls(module_id)?.keys().copied().collect_vec()))
}
pub fn module_impl_by_id<'db>(
    db: &'db dyn DefsGroup,
    impl_def_id: ImplDefId<'db>,
) -> Maybe<ast::ItemImpl<'db>> {
    let module_impls = db.module_impls(impl_def_id.module_file_id(db).0)?;
    Ok(module_impls[&impl_def_id].clone())
}

/// Returns all the extern_types of the given module.
pub fn module_extern_types<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<ExternTypeId<'db>, ast::ItemExternType<'db>>>> {
    Ok(db.priv_module_data(module_id)?.extern_types)
}
pub fn module_extern_types_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ExternTypeId<'db>>>> {
    Ok(Arc::new(db.module_extern_types(module_id)?.keys().copied().collect_vec()))
}
pub fn module_extern_type_by_id<'db>(
    db: &'db dyn DefsGroup,
    extern_type_id: ExternTypeId<'db>,
) -> Maybe<ast::ItemExternType<'db>> {
    let module_extern_types = db.module_extern_types(extern_type_id.module_file_id(db).0)?;
    Ok(module_extern_types[&extern_type_id].clone())
}

/// Returns all the macro declarations of the given module.
pub fn module_macro_declarations<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<MacroDeclarationId<'db>, ast::ItemMacroDeclaration<'db>>>> {
    Ok(db.priv_module_data(module_id)?.macro_declarations)
}
/// Returns all the ids of the macro declarations of the given module.
pub fn module_macro_declarations_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<MacroDeclarationId<'db>>>> {
    Ok(Arc::new(db.module_macro_declarations(module_id)?.keys().copied().collect_vec()))
}
/// Returns the macro declaration of the given id.
pub fn module_macro_declaration_by_id<'db>(
    db: &'db dyn DefsGroup,
    macro_declaration_id: MacroDeclarationId<'db>,
) -> Maybe<ast::ItemMacroDeclaration<'db>> {
    let module_macro_declarations =
        db.module_macro_declarations(macro_declaration_id.module_file_id(db).0)?;
    Ok(module_macro_declarations[&macro_declaration_id].clone())
}

/// Query implementation of [DefsGroup::module_macro_calls].
pub fn module_macro_calls<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<MacroCallId<'db>, ast::ItemInlineMacro<'db>>>> {
    Ok(db.priv_module_data(module_id)?.macro_calls)
}
/// Query implementation of [DefsGroup::module_macro_calls_ids].
pub fn module_macro_calls_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<MacroCallId<'db>>>> {
    Ok(Arc::new(db.module_macro_calls(module_id)?.keys().copied().collect_vec()))
}
/// Query implementation of [DefsGroup::module_macro_call_by_id].
fn module_macro_call_by_id<'db>(
    db: &'db dyn DefsGroup,
    macro_call_id: MacroCallId<'db>,
) -> Maybe<ast::ItemInlineMacro<'db>> {
    let module_macro_calls = db.module_macro_calls(macro_call_id.module_file_id(db).0)?;
    Ok(module_macro_calls[&macro_call_id].clone())
}

/// Returns all the extern_functions of the given module.
pub fn module_extern_functions<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<ExternFunctionId<'db>, ast::ItemExternFunction<'db>>>> {
    Ok(db.priv_module_data(module_id)?.extern_functions)
}
pub fn module_extern_functions_ids<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ExternFunctionId<'db>>>> {
    Ok(Arc::new(db.module_extern_functions(module_id)?.keys().copied().collect_vec()))
}
pub fn module_extern_function_by_id<'db>(
    db: &'db dyn DefsGroup,
    extern_function_id: ExternFunctionId<'db>,
) -> Maybe<ast::ItemExternFunction<'db>> {
    let module_extern_functions =
        db.module_extern_functions(extern_function_id.module_file_id(db).0)?;
    Ok(module_extern_functions[&extern_function_id].clone())
}

pub fn module_ancestors<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> OrderedHashSet<ModuleId<'db>> {
    let mut current = module_id;
    let mut ancestors = OrderedHashSet::default();
    while let ModuleId::Submodule(submodule_id) = current {
        let parent = submodule_id.parent_module(db);
        ancestors.insert(parent);
        current = parent
    }
    ancestors
}

/// Returns the generated_file_infos of the given module.
pub fn module_generated_file_aux_data<'db>(
    db: &dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<[Option<DynGeneratedFileAuxData>]>> {
    Ok(Arc::from(db.priv_module_data(module_id)?.generated_file_aux_data.as_slice()))
}

/// Returns all the plugin diagnostics of the given module.
pub fn module_plugin_diagnostics<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<(ModuleFileId<'db>, PluginDiagnostic<'db>)>>> {
    Ok(Arc::new(db.priv_module_data(module_id)?.plugin_diagnostics.to_vec()))
}

/// Diagnostic notes for diagnostics originating in the plugin generated files identified by
/// [`FileId`].
pub fn module_plugin_diagnostics_notes<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<PluginFileDiagnosticNotes<'db>>> {
    Ok(db.priv_module_data(module_id)?.diagnostics_notes.into())
}

fn module_items<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<Vec<ModuleItemId<'db>>>> {
    Ok(Arc::new(db.priv_module_data(module_id)?.items.to_vec()))
}

fn module_global_uses<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
) -> Maybe<Arc<OrderedHashMap<GlobalUseId<'db>, ast::UsePathStar<'db>>>> {
    Ok(db.priv_module_data(module_id)?.global_uses)
}

fn module_item_name_stable_ptr<'db>(
    db: &'db dyn DefsGroup,
    module_id: ModuleId<'db>,
    item_id: ModuleItemId<'db>,
) -> Maybe<SyntaxStablePtrId<'db>> {
    let data = db.priv_module_data(module_id)?;
    Ok(match &item_id {
        ModuleItemId::Constant(id) => data.constants[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Submodule(id) => data.submodules[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Use(id) => data.uses[id].name_stable_ptr(db),
        ModuleItemId::FreeFunction(id) => {
            data.free_functions[id].declaration(db).name(db).stable_ptr(db).untyped()
        }
        ModuleItemId::Struct(id) => data.structs[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Enum(id) => data.enums[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::TypeAlias(id) => data.type_aliases[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::ImplAlias(id) => data.impl_aliases[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Trait(id) => data.traits[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::Impl(id) => data.impls[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::ExternType(id) => data.extern_types[id].name(db).stable_ptr(db).untyped(),
        ModuleItemId::ExternFunction(id) => {
            data.extern_functions[id].declaration(db).name(db).stable_ptr(db).untyped()
        }
        ModuleItemId::MacroDeclaration(id) => {
            data.macro_declarations[id].name(db).stable_ptr(db).untyped()
        }
    })
}

pub trait DefsGroupEx: DefsGroup {
    /// Overrides the default macro plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `DefsGroup::macro_plugin_overrides`.
    fn set_override_crate_macro_plugins<'db>(
        &mut self,
        crate_id: CrateId<'db>,
        plugins: Arc<Vec<MacroPluginId<'db>>>,
    ) {
        let crate_input = self.crate_input(crate_id);
        let mut overrides = self.macro_plugin_overrides_input().as_ref().clone();
        let plugins =
            plugins.iter().map(|plugin| self.lookup_intern_macro_plugin(*plugin)).collect();
        overrides.insert(crate_input, plugins);
        self.set_macro_plugin_overrides_input(Arc::new(overrides));
    }

    /// Overrides the default inline macro plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `DefsGroup::inline_macro_plugin_overrides`.
    fn set_override_crate_inline_macro_plugins<'db>(
        &mut self,
        crate_id: CrateId<'db>,
        plugins: Arc<OrderedHashMap<String, InlineMacroExprPluginId<'db>>>,
    ) {
        let crate_input = self.crate_input(crate_id);
        let mut overrides = self.inline_macro_plugin_overrides_input().as_ref().clone();
        let plugins = Arc::new(
            plugins
                .iter()
                .map(|(name, plugin)| {
                    (name.clone(), self.lookup_intern_inline_macro_plugin(*plugin))
                })
                .collect(),
        );
        overrides.insert(crate_input, plugins);
        self.set_inline_macro_plugin_overrides_input(Arc::new(overrides));
    }
}

impl<T: DefsGroup + ?Sized> DefsGroupEx for T {}
