use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::{DiagnosticNote, Maybe, PluginFileDiagnosticNotes, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, Directory, FileId, FileKind, FileLongId, VirtualFile};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::consts::{
    ALLOW_ATTR, ALLOW_ATTR_ATTR, DEPRECATED_ATTR, FEATURE_ATTR, FMT_SKIP_ATTR,
    IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR, INTERNAL_ATTR, MUST_USE_ATTR, PHANTOM_ATTR,
    STARKNET_INTERFACE_ATTR, UNSTABLE_ATTR,
};
use cairo_lang_syntax::attribute::structured::AttributeStructurize;
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::element_list::ElementList;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};
use itertools::{Itertools, chain};
use salsa::InternKey;

use crate::ids::*;
use crate::plugin::{DynGeneratedFileAuxData, MacroPlugin, MacroPluginMetadata, PluginDiagnostic};
use crate::plugin_utils::try_extract_unnamed_arg;

/// Salsa database interface.
/// See [`super::ids`] for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup:
    FilesGroup + SyntaxGroup + Upcast<dyn SyntaxGroup> + ParserGroup + Upcast<dyn FilesGroup>
{
    #[salsa::interned]
    fn intern_constant(&self, id: ConstantLongId) -> ConstantId;
    #[salsa::interned]
    fn intern_submodule(&self, id: SubmoduleLongId) -> SubmoduleId;
    #[salsa::interned]
    fn intern_use(&self, id: UseLongId) -> UseId;
    #[salsa::interned]
    fn intern_global_use(&self, id: GlobalUseLongId) -> GlobalUseId;
    #[salsa::interned]
    fn intern_free_function(&self, id: FreeFunctionLongId) -> FreeFunctionId;
    #[salsa::interned]
    fn intern_impl_type_def(&self, id: ImplTypeDefLongId) -> ImplTypeDefId;
    #[salsa::interned]
    fn intern_impl_constant_def(&self, id: ImplConstantDefLongId) -> ImplConstantDefId;
    #[salsa::interned]
    fn intern_impl_impl_def(&self, id: ImplImplDefLongId) -> ImplImplDefId;
    #[salsa::interned]
    fn intern_impl_function(&self, id: ImplFunctionLongId) -> ImplFunctionId;
    #[salsa::interned]
    fn intern_struct(&self, id: StructLongId) -> StructId;
    #[salsa::interned]
    fn intern_enum(&self, id: EnumLongId) -> EnumId;
    #[salsa::interned]
    fn intern_module_type_alias(&self, id: ModuleTypeAliasLongId) -> ModuleTypeAliasId;
    #[salsa::interned]
    fn intern_impl_alias(&self, id: ImplAliasLongId) -> ImplAliasId;
    #[salsa::interned]
    fn intern_member(&self, id: MemberLongId) -> MemberId;
    #[salsa::interned]
    fn intern_variant(&self, id: VariantLongId) -> VariantId;
    #[salsa::interned]
    fn intern_trait(&self, id: TraitLongId) -> TraitId;
    #[salsa::interned]
    fn intern_trait_type(&self, id: TraitTypeLongId) -> TraitTypeId;
    #[salsa::interned]
    fn intern_trait_constant(&self, id: TraitConstantLongId) -> TraitConstantId;
    #[salsa::interned]
    fn intern_trait_impl(&self, id: TraitImplLongId) -> TraitImplId;
    #[salsa::interned]
    fn intern_trait_function(&self, id: TraitFunctionLongId) -> TraitFunctionId;
    #[salsa::interned]
    fn intern_impl_def(&self, id: ImplDefLongId) -> ImplDefId;
    #[salsa::interned]
    fn intern_extern_type(&self, id: ExternTypeLongId) -> ExternTypeId;
    #[salsa::interned]
    fn intern_extern_function(&self, id: ExternFunctionLongId) -> ExternFunctionId;
    #[salsa::interned]
    fn intern_param(&self, id: ParamLongId) -> ParamId;
    #[salsa::interned]
    fn intern_generic_param(&self, id: GenericParamLongId) -> GenericParamId;
    #[salsa::interned]
    fn intern_local_var(&self, id: LocalVarLongId) -> LocalVarId;
    #[salsa::interned]
    fn intern_statement_const(&self, id: StatementConstLongId) -> StatementConstId;
    #[salsa::interned]
    fn intern_statement_use(&self, id: StatementUseLongId) -> StatementUseId;
    #[salsa::interned]
    fn intern_plugin_generated_file(&self, id: PluginGeneratedFileLongId) -> PluginGeneratedFileId;

    // Plugins.
    // ========

    #[salsa::input]
    fn default_macro_plugins(&self) -> Arc<[MacroPluginId]>;

    #[salsa::input]
    fn macro_plugin_overrides(&self) -> Arc<OrderedHashMap<CrateId, Arc<[MacroPluginId]>>>;

    #[salsa::interned]
    fn intern_macro_plugin(&self, plugin: MacroPluginLongId) -> MacroPluginId;

    /// Returns [`MacroPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Provides an override if it has been set with
    /// [`DefsGroupEx::set_override_crate_macro_plugins`] or the default
    /// ([`DefsGroup::default_macro_plugins`]) otherwise.
    fn crate_macro_plugins(&self, crate_id: CrateId) -> Arc<[MacroPluginId]>;

    #[salsa::input]
    fn default_inline_macro_plugins(&self) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId>>;

    #[salsa::input]
    fn inline_macro_plugin_overrides(
        &self,
    ) -> Arc<OrderedHashMap<CrateId, Arc<OrderedHashMap<String, InlineMacroExprPluginId>>>>;

    #[salsa::interned]
    fn intern_inline_macro_plugin(
        &self,
        plugin: InlineMacroExprPluginLongId,
    ) -> InlineMacroExprPluginId;

    /// Returns [`InlineMacroExprPluginId`]s of the plugins set for the crate with [`CrateId`].
    /// Provides an override if it has been set with
    /// [`DefsGroupEx::set_override_crate_inline_macro_plugins`] or the default
    /// ([`DefsGroup::default_inline_macro_plugins`]) otherwise.
    fn crate_inline_macro_plugins(
        &self,
        crate_id: CrateId,
    ) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId>>;

    /// Returns the set of attributes allowed anywhere.
    /// An attribute on any item that is not in this set will be handled as an unknown attribute.
    fn allowed_attributes(&self, crate_id: CrateId) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of attributes allowed on statements.
    /// An attribute on a statement that is not in this set will be handled as an unknown attribute.
    fn allowed_statement_attributes(&self) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of `derive` that were declared as by a plugin.
    /// A derive that is not in this set will be handled as an unknown derive.
    fn declared_derives(&self, crate_id: CrateId) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of attributes that were declared as phantom type attributes by a plugin,
    /// i.e. a type marked with this attribute is considered a phantom type.
    fn declared_phantom_type_attributes(&self, crate_id: CrateId) -> Arc<OrderedHashSet<String>>;

    /// Checks whether the submodule is defined as inline.
    fn is_submodule_inline(&self, submodule_id: SubmoduleId) -> Maybe<bool>;

    // Module to syntax.
    /// Gets the main file of the module.
    /// A module might have more virtual files generated by plugins.
    fn module_main_file(&self, module_id: ModuleId) -> Maybe<FileId>;
    /// Gets all the files of a module - main files and generated virtual files.
    fn module_files(&self, module_id: ModuleId) -> Maybe<Arc<[FileId]>>;
    /// Gets a file from a module and a FileIndex (i.e. ModuleFileId).
    fn module_file(&self, module_id: ModuleFileId) -> Maybe<FileId>;
    /// Gets the directory of a module.
    fn module_dir(&self, module_id: ModuleId) -> Maybe<Directory>;

    // File to module.
    fn crate_modules(&self, crate_id: CrateId) -> Arc<[ModuleId]>;
    fn priv_file_to_module_mapping(&self) -> Arc<OrderedHashMap<FileId, Vec<ModuleId>>>;
    fn file_modules(&self, file_id: FileId) -> Maybe<Arc<[ModuleId]>>;

    // Module level resolving.
    fn priv_module_data(&self, module_id: ModuleId) -> Maybe<ModuleData>;
    // Returns the information about sub-files generated by the file in the module.
    fn priv_module_sub_files(
        &self,
        module_id: ModuleId,
        file_id: FileId,
    ) -> Maybe<Arc<PrivModuleSubFiles>>;
    fn module_submodules(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<SubmoduleId, ast::ItemModule>>>;
    fn module_submodules_ids(&self, module_id: ModuleId) -> Maybe<Arc<[SubmoduleId]>>;
    fn module_constants(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<ConstantId, ast::ItemConstant>>>;
    fn module_constants_ids(&self, module_id: ModuleId) -> Maybe<Arc<[ConstantId]>>;
    fn module_constant_by_id(&self, constant_id: ConstantId) -> Maybe<Option<ast::ItemConstant>>;
    fn module_free_functions(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<FreeFunctionId, ast::FunctionWithBody>>>;
    fn module_free_functions_ids(&self, module_id: ModuleId) -> Maybe<Arc<[FreeFunctionId]>>;
    fn module_free_function_by_id(
        &self,
        free_function_id: FreeFunctionId,
    ) -> Maybe<Option<ast::FunctionWithBody>>;
    fn module_items(&self, module_id: ModuleId) -> Maybe<Arc<[ModuleItemId]>>;
    fn module_global_uses(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<GlobalUseId, ast::UsePathStar>>>;
    /// Returns the stable ptr of the name of a module item.
    fn module_item_name_stable_ptr(
        &self,
        module_id: ModuleId,
        item_id: ModuleItemId,
    ) -> Maybe<SyntaxStablePtrId>;
    fn module_uses(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<UseId, ast::UsePathLeaf>>>;
    fn module_uses_ids(&self, module_id: ModuleId) -> Maybe<Arc<[UseId]>>;
    fn module_use_by_id(&self, use_id: UseId) -> Maybe<Option<ast::UsePathLeaf>>;
    fn module_global_use_by_id(
        &self,
        global_use_id: GlobalUseId,
    ) -> Maybe<Option<ast::UsePathStar>>;
    fn module_structs(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<StructId, ast::ItemStruct>>>;
    fn module_structs_ids(&self, module_id: ModuleId) -> Maybe<Arc<[StructId]>>;
    fn module_struct_by_id(&self, struct_id: StructId) -> Maybe<Option<ast::ItemStruct>>;
    fn module_enums(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<EnumId, ast::ItemEnum>>>;
    fn module_enums_ids(&self, module_id: ModuleId) -> Maybe<Arc<[EnumId]>>;
    fn module_enum_by_id(&self, enum_id: EnumId) -> Maybe<Option<ast::ItemEnum>>;
    fn module_type_aliases(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<ModuleTypeAliasId, ast::ItemTypeAlias>>>;
    fn module_type_aliases_ids(&self, module_id: ModuleId) -> Maybe<Arc<[ModuleTypeAliasId]>>;
    fn module_type_alias_by_id(
        &self,
        module_type_alias_id: ModuleTypeAliasId,
    ) -> Maybe<Option<ast::ItemTypeAlias>>;
    fn module_impl_aliases(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<ImplAliasId, ast::ItemImplAlias>>>;
    fn module_impl_aliases_ids(&self, module_id: ModuleId) -> Maybe<Arc<[ImplAliasId]>>;
    fn module_impl_alias_by_id(
        &self,
        impl_alias_id: ImplAliasId,
    ) -> Maybe<Option<ast::ItemImplAlias>>;
    fn module_traits(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<TraitId, ast::ItemTrait>>>;
    fn module_traits_ids(&self, module_id: ModuleId) -> Maybe<Arc<[TraitId]>>;
    fn module_trait_by_id(&self, trait_id: TraitId) -> Maybe<Option<ast::ItemTrait>>;
    fn module_impls(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<ImplDefId, ast::ItemImpl>>>;
    fn module_impls_ids(&self, module_id: ModuleId) -> Maybe<Arc<[ImplDefId]>>;
    fn module_impl_by_id(&self, impl_id: ImplDefId) -> Maybe<Option<ast::ItemImpl>>;
    fn module_extern_types(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<ExternTypeId, ast::ItemExternType>>>;
    fn module_extern_types_ids(&self, module_id: ModuleId) -> Maybe<Arc<[ExternTypeId]>>;
    fn module_extern_type_by_id(
        &self,
        extern_type_id: ExternTypeId,
    ) -> Maybe<Option<ast::ItemExternType>>;
    fn module_extern_functions(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>>>;
    fn module_extern_functions_ids(&self, module_id: ModuleId) -> Maybe<Arc<[ExternFunctionId]>>;
    fn module_extern_function_by_id(
        &self,
        extern_function_id: ExternFunctionId,
    ) -> Maybe<Option<ast::ItemExternFunction>>;
    fn module_ancestors(&self, module_id: ModuleId) -> OrderedHashSet<ModuleId>;
    fn module_generated_file_aux_data(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<[Option<DynGeneratedFileAuxData>]>>;
    fn module_plugin_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<[(ModuleFileId, PluginDiagnostic)]>>;
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    fn module_plugin_diagnostics_notes(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<PluginFileDiagnosticNotes>>;
}

/// Initializes the [`DefsGroup`] database to a proper state.
pub fn init_defs_group(db: &mut dyn DefsGroup) {
    db.set_macro_plugin_overrides(Arc::new(OrderedHashMap::default()));
    db.set_inline_macro_plugin_overrides(Arc::new(OrderedHashMap::default()));
}

fn crate_macro_plugins(db: &dyn DefsGroup, crate_id: CrateId) -> Arc<[MacroPluginId]> {
    db.macro_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_macro_plugins())
}

fn crate_inline_macro_plugins(
    db: &dyn DefsGroup,
    crate_id: CrateId,
) -> Arc<OrderedHashMap<String, InlineMacroExprPluginId>> {
    db.inline_macro_plugin_overrides()
        .get(&crate_id)
        .cloned()
        .unwrap_or_else(|| db.default_inline_macro_plugins())
}

fn allowed_attributes(db: &dyn DefsGroup, crate_id: CrateId) -> Arc<OrderedHashSet<String>> {
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

fn declared_derives(db: &dyn DefsGroup, crate_id: CrateId) -> Arc<OrderedHashSet<String>> {
    Arc::new(OrderedHashSet::from_iter(
        db.crate_macro_plugins(crate_id)
            .iter()
            .flat_map(|plugin| db.lookup_intern_macro_plugin(*plugin).declared_derives()),
    ))
}

fn declared_phantom_type_attributes(
    db: &dyn DefsGroup,
    crate_id: CrateId,
) -> Arc<OrderedHashSet<String>> {
    let crate_plugins = db.crate_macro_plugins(crate_id);

    Arc::new(OrderedHashSet::from_iter(chain!(
        [PHANTOM_ATTR.into()],
        crate_plugins
            .iter()
            .flat_map(|plugin| db.lookup_intern_macro_plugin(*plugin).phantom_type_attributes())
    )))
}

fn is_submodule_inline(db: &dyn DefsGroup, submodule_id: SubmoduleId) -> Maybe<bool> {
    let parent = submodule_id.parent_module(db);
    let item_module_ast = &db.priv_module_data(parent)?.submodules[&submodule_id];
    match item_module_ast.body(db.upcast()) {
        MaybeModuleBody::Some(_) => Ok(true),
        MaybeModuleBody::None(_) => Ok(false),
    }
}

fn module_main_file(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<FileId> {
    Ok(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe()?.root.file(db.upcast(), "lib.cairo".into())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            if db.is_submodule_inline(submodule_id)? {
                // This is an inline module, we return the file where the inline module was
                // defined. It can be either the file of the parent module
                // or a plugin-generated virtual file.
                db.module_file(submodule_id.module_file_id(db))?
            } else {
                let name = submodule_id.name(db);
                db.module_dir(parent)?.file(db.upcast(), format!("{name}.cairo").into())
            }
        }
    })
}

fn module_files(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[FileId]>> {
    Ok(db.priv_module_data(module_id)?.files.into())
}

fn module_file(db: &dyn DefsGroup, module_file_id: ModuleFileId) -> Maybe<FileId> {
    Ok(db.module_files(module_file_id.0)?[module_file_id.1.0])
}

fn module_dir(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Directory> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe().map(|config| config.root)
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            let name = submodule_id.name(db);
            Ok(db.module_dir(parent)?.subdir(name))
        }
    }
}

/// Appends all the modules under the given module, including nested modules.
fn collect_modules_under(db: &dyn DefsGroup, modules: &mut Vec<ModuleId>, module_id: ModuleId) {
    modules.push(module_id);
    if let Ok(submodule_ids) = db.module_submodules_ids(module_id) {
        for submodule_module_id in submodule_ids.iter().copied() {
            collect_modules_under(db, modules, ModuleId::Submodule(submodule_module_id));
        }
    }
}

/// Returns all the modules in the crate, including recursively.
fn crate_modules(db: &dyn DefsGroup, crate_id: CrateId) -> Arc<[ModuleId]> {
    let mut modules = Vec::new();
    collect_modules_under(db, &mut modules, ModuleId::CrateRoot(crate_id));
    modules.into()
}

fn priv_file_to_module_mapping(db: &dyn DefsGroup) -> Arc<OrderedHashMap<FileId, Vec<ModuleId>>> {
    let mut mapping = OrderedHashMap::<FileId, Vec<ModuleId>>::default();
    for crate_id in db.crates() {
        for module_id in db.crate_modules(crate_id).iter().copied() {
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
fn file_modules(db: &dyn DefsGroup, file_id: FileId) -> Maybe<Arc<[ModuleId]>> {
    Ok(db.priv_file_to_module_mapping().get(&file_id).to_maybe()?.clone().into())
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleData {
    /// The list of IDs of all items in the module. Each ID here is guaranteed to be a key in one
    /// of the specific-item-kind maps.
    items: Arc<[ModuleItemId]>,

    // Specific-item-kind maps
    constants: Arc<OrderedHashMap<ConstantId, ast::ItemConstant>>,
    submodules: Arc<OrderedHashMap<SubmoduleId, ast::ItemModule>>,
    uses: Arc<OrderedHashMap<UseId, ast::UsePathLeaf>>,
    free_functions: Arc<OrderedHashMap<FreeFunctionId, ast::FunctionWithBody>>,
    structs: Arc<OrderedHashMap<StructId, ast::ItemStruct>>,
    enums: Arc<OrderedHashMap<EnumId, ast::ItemEnum>>,
    type_aliases: Arc<OrderedHashMap<ModuleTypeAliasId, ast::ItemTypeAlias>>,
    impl_aliases: Arc<OrderedHashMap<ImplAliasId, ast::ItemImplAlias>>,
    traits: Arc<OrderedHashMap<TraitId, ast::ItemTrait>>,
    impls: Arc<OrderedHashMap<ImplDefId, ast::ItemImpl>>,
    extern_types: Arc<OrderedHashMap<ExternTypeId, ast::ItemExternType>>,
    extern_functions: Arc<OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>>,
    global_uses: Arc<OrderedHashMap<GlobalUseId, ast::UsePathStar>>,

    files: Vec<FileId>,
    /// Generation info for each file. Virtual files have Some. Other files have None.
    generated_file_aux_data: Vec<Option<DynGeneratedFileAuxData>>,
    plugin_diagnostics: Vec<(ModuleFileId, PluginDiagnostic)>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    diagnostics_notes: PluginFileDiagnosticNotes,
}

/// Information about generated files from running on a module file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PrivModuleSubFiles {
    /// The files generated by plugins running on items.
    files: OrderedHashMap<FileId, VirtualFile>,
    /// The aux data per such file.
    aux_data: Vec<Option<DynGeneratedFileAuxData>>,
    /// The items not filtered out by plugins.
    items: Vec<ast::ModuleItem>,
    /// The diagnostics generated by the plugins.
    plugin_diagnostics: Vec<PluginDiagnostic>,
    /// Diagnostic notes for diagnostics originating in the plugin generated files identified by
    /// [`FileId`].
    /// Diagnostic notes are added with `note: ` prefix at the end of diagnostic display.
    diagnostics_notes: PluginFileDiagnosticNotes,
}

fn priv_module_data(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<ModuleData> {
    let syntax_db = db.upcast();
    let module_file = db.module_main_file(module_id)?;
    let main_file_aux_data = if let ModuleId::Submodule(submodule_id) = module_id {
        let parent_module_data = db.priv_module_data(submodule_id.parent_module(db))?;
        let item_module_ast = &parent_module_data.submodules[&submodule_id];
        if matches!(item_module_ast.body(syntax_db), MaybeModuleBody::Some(_)) {
            // TODO(spapini): Diagnostics in this module that get mapped to parent module
            // should lie in that modules ModuleData, or somehow collected by its
            // diagnostics collector function.

            // If this is an inline module, copy its generation file info from the parent
            // module, from the file where this submodule was defined.
            parent_module_data
                .generated_file_aux_data
                .into_iter()
                .nth(submodule_id.file_index(db).0)
                .unwrap()
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
                    let item_id = ConstantLongId(module_file_id, constant.stable_ptr()).intern(db);
                    constants.insert(item_id, constant);
                    items.push(ModuleItemId::Constant(item_id));
                }
                ast::ModuleItem::Module(module) => {
                    let item_id = SubmoduleLongId(module_file_id, module.stable_ptr()).intern(db);
                    submodules.insert(item_id, module);
                    items.push(ModuleItemId::Submodule(item_id));
                }
                ast::ModuleItem::Use(us) => {
                    for leaf in get_all_path_leaves(db.upcast(), &us) {
                        let id = UseLongId(module_file_id, leaf.stable_ptr()).intern(db);
                        uses.insert(id, leaf);
                        items.push(ModuleItemId::Use(id));
                    }
                    for star in get_all_path_stars(db.upcast(), &us) {
                        let id = GlobalUseLongId(module_file_id, star.stable_ptr()).intern(db);
                        global_uses.insert(id, star);
                    }
                }
                ast::ModuleItem::FreeFunction(function) => {
                    let item_id =
                        FreeFunctionLongId(module_file_id, function.stable_ptr()).intern(db);
                    free_functions.insert(item_id, function);
                    items.push(ModuleItemId::FreeFunction(item_id));
                }
                ast::ModuleItem::ExternFunction(extern_function) => {
                    let item_id =
                        ExternFunctionLongId(module_file_id, extern_function.stable_ptr())
                            .intern(db);
                    extern_functions.insert(item_id, extern_function);
                    items.push(ModuleItemId::ExternFunction(item_id));
                }
                ast::ModuleItem::ExternType(extern_type) => {
                    let item_id =
                        ExternTypeLongId(module_file_id, extern_type.stable_ptr()).intern(db);
                    extern_types.insert(item_id, extern_type);
                    items.push(ModuleItemId::ExternType(item_id));
                }
                ast::ModuleItem::Trait(trt) => {
                    let item_id = TraitLongId(module_file_id, trt.stable_ptr()).intern(db);
                    traits.insert(item_id, trt);
                    items.push(ModuleItemId::Trait(item_id));
                }
                ast::ModuleItem::Impl(imp) => {
                    let item_id = ImplDefLongId(module_file_id, imp.stable_ptr()).intern(db);
                    impls.insert(item_id, imp);
                    items.push(ModuleItemId::Impl(item_id));
                }
                ast::ModuleItem::Struct(structure) => {
                    let item_id = StructLongId(module_file_id, structure.stable_ptr()).intern(db);
                    structs.insert(item_id, structure);
                    items.push(ModuleItemId::Struct(item_id));
                }
                ast::ModuleItem::Enum(enm) => {
                    let item_id = EnumLongId(module_file_id, enm.stable_ptr()).intern(db);
                    enums.insert(item_id, enm);
                    items.push(ModuleItemId::Enum(item_id));
                }
                ast::ModuleItem::TypeAlias(type_alias) => {
                    let item_id =
                        ModuleTypeAliasLongId(module_file_id, type_alias.stable_ptr()).intern(db);
                    type_aliases.insert(item_id, type_alias);
                    items.push(ModuleItemId::TypeAlias(item_id));
                }
                ast::ModuleItem::ImplAlias(impl_alias) => {
                    let item_id =
                        ImplAliasLongId(module_file_id, impl_alias.stable_ptr()).intern(db);
                    impl_aliases.insert(item_id, impl_alias);
                    items.push(ModuleItemId::ImplAlias(item_id));
                }
                ast::ModuleItem::InlineMacro(inline_macro_ast) => plugin_diagnostics.push((
                    module_file_id,
                    PluginDiagnostic::error(
                        &inline_macro_ast,
                        format!(
                            "Unknown inline item macro: '{}'.",
                            inline_macro_ast.name(db.upcast()).text(db.upcast())
                        ),
                    ),
                )),
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
        global_uses: global_uses.into(),
        files,
        generated_file_aux_data: aux_data,
        plugin_diagnostics,
        diagnostics_notes,
    };
    Ok(res)
}

/// Returns the `VirtualFile` matching the given external id.
pub fn try_ext_as_virtual_impl(
    db: &dyn DefsGroup,
    external_id: salsa::InternId,
) -> Option<VirtualFile> {
    let long_id = PluginGeneratedFileId::from_intern_id(external_id).lookup_intern(db);
    let file_id = FileLongId::External(external_id).intern(db);
    let data = db
        .priv_module_sub_files(long_id.module_id, long_id.stable_ptr.file_id(db.upcast()))
        .unwrap();
    data.files.get(&file_id).cloned()
}

fn priv_module_sub_files(
    db: &dyn DefsGroup,
    module_id: ModuleId,
    file_id: FileId,
) -> Maybe<Arc<PrivModuleSubFiles>> {
    let syntax_db = db.upcast();
    let module_main_file = db.module_main_file(module_id)?;
    let file_syntax = db.file_module_syntax(file_id)?;
    let item_asts = if module_main_file == file_id {
        if let ModuleId::Submodule(submodule_id) = module_id {
            let data = db.priv_module_data(submodule_id.parent_module(db))?;
            if let MaybeModuleBody::Some(body) = data.submodules[&submodule_id].body(db.upcast()) {
                Some(body.items(syntax_db))
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
    .unwrap_or_else(|| file_syntax.items(syntax_db));

    let crate_id = module_id.owning_crate(db);

    let allowed_attributes = db.allowed_attributes(crate_id);
    // TODO(orizi): Actually extract the allowed features per module.
    let allowed_features = Default::default();

    let cfg_set = db
        .crate_config(crate_id)
        .and_then(|cfg| cfg.settings.cfg_set.map(Arc::new))
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
    for item_ast in item_asts.elements(syntax_db) {
        let mut remove_original_item = false;
        // Iterate the plugins by their order. The first one to change something (either
        // generate new code, remove the original code, or both), breaks the loop. If more
        // plugins might have act on the item, they can do it on the generated code.
        for plugin_id in db.crate_macro_plugins(crate_id).iter() {
            let plugin = db.lookup_intern_macro_plugin(*plugin_id);

            let result = plugin.generate_code(db.upcast(), item_ast.clone(), &metadata);
            plugin_diagnostics.extend(result.diagnostics);
            if result.remove_original_item {
                remove_original_item = true;
            }

            if let Some(generated) = result.code {
                let generated_file_id = FileLongId::External(
                    PluginGeneratedFileLongId {
                        module_id,
                        stable_ptr: item_ast.stable_ptr().untyped(),
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
        validate_attributes(syntax_db, &allowed_attributes, &item_ast, &mut plugin_diagnostics);
        items.push(item_ast);
    }
    let res = PrivModuleSubFiles { files, aux_data, items, plugin_diagnostics, diagnostics_notes };
    Ok(res.into())
}

/// Collects attributes allowed by `allow_attr` attribute.
fn collect_extra_allowed_attributes(
    db: &dyn SyntaxGroup,
    item: &impl QueryAttrs,
    plugin_diagnostics: &mut Vec<PluginDiagnostic>,
) -> OrderedHashSet<String> {
    let mut extra_allowed_attributes = OrderedHashSet::default();
    for attr in item.attributes_elements(db) {
        if attr.attr(db).as_syntax_node().get_text_without_trivia(db) == ALLOW_ATTR_ATTR {
            let args = attr.clone().structurize(db).args;
            if args.is_empty() {
                plugin_diagnostics.push(PluginDiagnostic::error(
                    attr.stable_ptr(),
                    "Expected arguments.".to_string(),
                ));
                continue;
            }
            for arg in args {
                if let Some(ast::Expr::Path(path)) = try_extract_unnamed_arg(db, &arg.arg) {
                    if let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] {
                        extra_allowed_attributes.insert(segment.ident(db).text(db).into());
                        continue;
                    }
                }
                plugin_diagnostics.push(PluginDiagnostic::error(
                    &arg.arg,
                    "Expected simple identifier.".to_string(),
                ));
            }
        }
    }
    extra_allowed_attributes
}

/// Validates that all attributes on the given item are in the allowed set or adds diagnostics.
pub fn validate_attributes_flat(
    db: &dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    extra_allowed_attributes: &OrderedHashSet<String>,
    item: &impl QueryAttrs,
    plugin_diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let local_extra_attributes = collect_extra_allowed_attributes(db, item, plugin_diagnostics);
    for attr in item.attributes_elements(db) {
        let attr_text = attr.attr(db).as_syntax_node().get_text_without_trivia(db);
        if !(allowed_attributes.contains(&attr_text)
            || extra_allowed_attributes.contains(&attr_text)
            || local_extra_attributes.contains(&attr_text))
        {
            plugin_diagnostics
                .push(PluginDiagnostic::error(&attr, "Unsupported attribute.".to_string()));
        }
    }
}

/// Validates that all attributes on all items in the given element list are in the allowed set or
/// adds diagnostics.
fn validate_attributes_element_list<Item: QueryAttrs + TypedSyntaxNode, const STEP: usize>(
    db: &dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    extra_allowed_attributes: &OrderedHashSet<String>,
    items: &ElementList<Item, STEP>,
    plugin_diagnostics: &mut Vec<PluginDiagnostic>,
) {
    for item in items.elements(db) {
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
fn validate_attributes(
    db: &dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    item_ast: &ast::ModuleItem,
    plugin_diagnostics: &mut Vec<PluginDiagnostic>,
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
                    &body.items(db),
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
                    &body.items(db),
                    plugin_diagnostics,
                );
            }
        }
        ast::ModuleItem::Struct(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                &extra_allowed_attributes,
                &item.members(db),
                plugin_diagnostics,
            );
        }
        ast::ModuleItem::Enum(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                &extra_allowed_attributes,
                &item.variants(db),
                plugin_diagnostics,
            );
        }
        _ => {}
    }
}

/// Returns all the path leaves under a given use item.
pub fn get_all_path_leaves(db: &dyn SyntaxGroup, use_item: &ast::ItemUse) -> Vec<ast::UsePathLeaf> {
    let mut res = vec![];
    let mut stack = vec![use_item.use_path(db)];
    while let Some(use_path) = stack.pop() {
        match use_path {
            ast::UsePath::Leaf(use_path) => res.push(use_path),
            ast::UsePath::Single(use_path) => stack.push(use_path.use_path(db)),
            ast::UsePath::Multi(use_path) => {
                stack.extend(use_path.use_paths(db).elements(db).into_iter().rev())
            }
            ast::UsePath::Star(_) => {}
        }
    }
    res
}

/// Returns all the path stars under a given use item.
pub fn get_all_path_stars(db: &dyn SyntaxGroup, use_item: &ast::ItemUse) -> Vec<ast::UsePathStar> {
    let mut res = vec![];
    let mut stack = vec![use_item.use_path(db)];
    while let Some(use_path) = stack.pop() {
        match use_path {
            ast::UsePath::Leaf(_) => {}
            ast::UsePath::Single(use_path) => stack.push(use_path.use_path(db)),
            ast::UsePath::Multi(use_path) => {
                stack.extend(use_path.use_paths(db).elements(db).into_iter().rev())
            }
            ast::UsePath::Star(use_path) => res.push(use_path),
        }
    }
    res
}

/// Returns all the constant definitions of the given module.
pub fn module_constants(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<ConstantId, ast::ItemConstant>>> {
    Ok(db.priv_module_data(module_id)?.constants)
}
pub fn module_constants_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[ConstantId]>> {
    Ok(db.module_constants(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_constant_by_id(
    db: &dyn DefsGroup,
    constant_id: ConstantId,
) -> Maybe<Option<ast::ItemConstant>> {
    let module_constants = db.module_constants(constant_id.module_file_id(db.upcast()).0)?;
    Ok(module_constants.get(&constant_id).cloned())
}

/// Returns all the *direct* submodules of the given module - including those generated by macro
/// plugins. To get all the submodules including nested modules, use [`collect_modules_under`].
fn module_submodules(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<SubmoduleId, ast::ItemModule>>> {
    Ok(db.priv_module_data(module_id)?.submodules)
}
fn module_submodules_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[SubmoduleId]>> {
    Ok(db.module_submodules(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_submodule_by_id(
    db: &dyn DefsGroup,
    submodule_id: SubmoduleId,
) -> Maybe<Option<ast::ItemModule>> {
    let module_submodules = db.module_submodules(submodule_id.module_file_id(db.upcast()).0)?;
    Ok(module_submodules.get(&submodule_id).cloned())
}

/// Returns all the free functions of the given module.
pub fn module_free_functions(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<FreeFunctionId, ast::FunctionWithBody>>> {
    Ok(db.priv_module_data(module_id)?.free_functions)
}
pub fn module_free_functions_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[FreeFunctionId]>> {
    Ok(db.module_free_functions(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_free_function_by_id(
    db: &dyn DefsGroup,
    free_function_id: FreeFunctionId,
) -> Maybe<Option<ast::FunctionWithBody>> {
    let module_free_functions =
        db.module_free_functions(free_function_id.module_file_id(db.upcast()).0)?;
    Ok(module_free_functions.get(&free_function_id).cloned())
}

/// Returns all the uses of the given module.
pub fn module_uses(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<UseId, ast::UsePathLeaf>>> {
    Ok(db.priv_module_data(module_id)?.uses)
}
pub fn module_uses_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[UseId]>> {
    Ok(db.module_uses(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_use_by_id(db: &dyn DefsGroup, use_id: UseId) -> Maybe<Option<ast::UsePathLeaf>> {
    let module_uses = db.module_uses(use_id.module_file_id(db.upcast()).0)?;
    Ok(module_uses.get(&use_id).cloned())
}

/// Returns the `use *` of the given module, by its ID.
pub fn module_global_use_by_id(
    db: &dyn DefsGroup,
    global_use_id: GlobalUseId,
) -> Maybe<Option<ast::UsePathStar>> {
    let module_global_uses = db.module_global_uses(global_use_id.module_file_id(db.upcast()).0)?;
    Ok(module_global_uses.get(&global_use_id).cloned())
}

/// Returns all the structs of the given module.
pub fn module_structs(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<StructId, ast::ItemStruct>>> {
    Ok(db.priv_module_data(module_id)?.structs)
}
pub fn module_structs_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[StructId]>> {
    Ok(db.module_structs(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_struct_by_id(
    db: &dyn DefsGroup,
    struct_id: StructId,
) -> Maybe<Option<ast::ItemStruct>> {
    let module_structs = db.module_structs(struct_id.module_file_id(db.upcast()).0)?;
    Ok(module_structs.get(&struct_id).cloned())
}

/// Returns all the enums of the given module.
pub fn module_enums(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<EnumId, ast::ItemEnum>>> {
    Ok(db.priv_module_data(module_id)?.enums)
}
pub fn module_enums_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[EnumId]>> {
    Ok(db.module_enums(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_enum_by_id(db: &dyn DefsGroup, enum_id: EnumId) -> Maybe<Option<ast::ItemEnum>> {
    let module_enums = db.module_enums(enum_id.module_file_id(db.upcast()).0)?;
    Ok(module_enums.get(&enum_id).cloned())
}

/// Returns all the type aliases of the given module.
pub fn module_type_aliases(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<ModuleTypeAliasId, ast::ItemTypeAlias>>> {
    Ok(db.priv_module_data(module_id)?.type_aliases)
}
pub fn module_type_aliases_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[ModuleTypeAliasId]>> {
    Ok(db.module_type_aliases(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_type_alias_by_id(
    db: &dyn DefsGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<Option<ast::ItemTypeAlias>> {
    let module_type_aliases =
        db.module_type_aliases(module_type_alias_id.module_file_id(db.upcast()).0)?;
    Ok(module_type_aliases.get(&module_type_alias_id).cloned())
}

/// Returns all the impl aliases of the given module.
pub fn module_impl_aliases(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<ImplAliasId, ast::ItemImplAlias>>> {
    Ok(db.priv_module_data(module_id)?.impl_aliases)
}
pub fn module_impl_aliases_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[ImplAliasId]>> {
    Ok(db.module_impl_aliases(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_impl_alias_by_id(
    db: &dyn DefsGroup,
    impl_alias_id: ImplAliasId,
) -> Maybe<Option<ast::ItemImplAlias>> {
    let module_impl_aliases =
        db.module_impl_aliases(impl_alias_id.module_file_id(db.upcast()).0)?;
    Ok(module_impl_aliases.get(&impl_alias_id).cloned())
}

/// Returns all the traits of the given module.
pub fn module_traits(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<TraitId, ast::ItemTrait>>> {
    Ok(db.priv_module_data(module_id)?.traits)
}
pub fn module_traits_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[TraitId]>> {
    Ok(db.module_traits(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_trait_by_id(db: &dyn DefsGroup, trait_id: TraitId) -> Maybe<Option<ast::ItemTrait>> {
    let module_traits = db.module_traits(trait_id.module_file_id(db.upcast()).0)?;
    Ok(module_traits.get(&trait_id).cloned())
}

/// Returns all the impls of the given module.
pub fn module_impls(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<ImplDefId, ast::ItemImpl>>> {
    Ok(db.priv_module_data(module_id)?.impls)
}
pub fn module_impls_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[ImplDefId]>> {
    Ok(db.module_impls(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_impl_by_id(
    db: &dyn DefsGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Option<ast::ItemImpl>> {
    let module_impls = db.module_impls(impl_def_id.module_file_id(db.upcast()).0)?;
    Ok(module_impls.get(&impl_def_id).cloned())
}

/// Returns all the extern_types of the given module.
pub fn module_extern_types(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<ExternTypeId, ast::ItemExternType>>> {
    Ok(db.priv_module_data(module_id)?.extern_types)
}
pub fn module_extern_types_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[ExternTypeId]>> {
    Ok(db.module_extern_types(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_extern_type_by_id(
    db: &dyn DefsGroup,
    extern_type_id: ExternTypeId,
) -> Maybe<Option<ast::ItemExternType>> {
    let module_extern_types =
        db.module_extern_types(extern_type_id.module_file_id(db.upcast()).0)?;
    Ok(module_extern_types.get(&extern_type_id).cloned())
}

/// Returns all the extern_functions of the given module.
pub fn module_extern_functions(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>>> {
    Ok(db.priv_module_data(module_id)?.extern_functions)
}
pub fn module_extern_functions_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[ExternFunctionId]>> {
    Ok(db.module_extern_functions(module_id)?.keys().copied().collect_vec().into())
}
pub fn module_extern_function_by_id(
    db: &dyn DefsGroup,
    extern_function_id: ExternFunctionId,
) -> Maybe<Option<ast::ItemExternFunction>> {
    let module_extern_functions =
        db.module_extern_functions(extern_function_id.module_file_id(db.upcast()).0)?;
    Ok(module_extern_functions.get(&extern_function_id).cloned())
}

pub fn module_ancestors(db: &dyn DefsGroup, module_id: ModuleId) -> OrderedHashSet<ModuleId> {
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
pub fn module_generated_file_aux_data(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[Option<DynGeneratedFileAuxData>]>> {
    Ok(db.priv_module_data(module_id)?.generated_file_aux_data.into())
}

/// Returns all the plugin diagnostics of the given module.
pub fn module_plugin_diagnostics(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[(ModuleFileId, PluginDiagnostic)]>> {
    Ok(db.priv_module_data(module_id)?.plugin_diagnostics.into())
}

/// Diagnostic notes for diagnostics originating in the plugin generated files identified by
/// [`FileId`].
pub fn module_plugin_diagnostics_notes(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<PluginFileDiagnosticNotes>> {
    Ok(db.priv_module_data(module_id)?.diagnostics_notes.into())
}

fn module_items(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[ModuleItemId]>> {
    Ok(db.priv_module_data(module_id)?.items)
}

fn module_global_uses(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<OrderedHashMap<GlobalUseId, ast::UsePathStar>>> {
    Ok(db.priv_module_data(module_id)?.global_uses)
}

fn module_item_name_stable_ptr(
    db: &dyn DefsGroup,
    module_id: ModuleId,
    item_id: ModuleItemId,
) -> Maybe<SyntaxStablePtrId> {
    let data = db.priv_module_data(module_id)?;
    let db = db.upcast();
    Ok(match &item_id {
        ModuleItemId::Constant(id) => data.constants[id].name(db).stable_ptr().untyped(),
        ModuleItemId::Submodule(id) => data.submodules[id].name(db).stable_ptr().untyped(),
        ModuleItemId::Use(id) => data.uses[id].name_stable_ptr(db),
        ModuleItemId::FreeFunction(id) => {
            data.free_functions[id].declaration(db).name(db).stable_ptr().untyped()
        }
        ModuleItemId::Struct(id) => data.structs[id].name(db).stable_ptr().untyped(),
        ModuleItemId::Enum(id) => data.enums[id].name(db).stable_ptr().untyped(),
        ModuleItemId::TypeAlias(id) => data.type_aliases[id].name(db).stable_ptr().untyped(),
        ModuleItemId::ImplAlias(id) => data.impl_aliases[id].name(db).stable_ptr().untyped(),
        ModuleItemId::Trait(id) => data.traits[id].name(db).stable_ptr().untyped(),
        ModuleItemId::Impl(id) => data.impls[id].name(db).stable_ptr().untyped(),
        ModuleItemId::ExternType(id) => data.extern_types[id].name(db).stable_ptr().untyped(),
        ModuleItemId::ExternFunction(id) => {
            data.extern_functions[id].declaration(db).name(db).stable_ptr().untyped()
        }
    })
}

pub trait DefsGroupEx: DefsGroup {
    /// Overrides the default macro plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `DefsGroup::macro_plugin_overrides`.
    fn set_override_crate_macro_plugins(
        &mut self,
        crate_id: CrateId,
        plugins: Arc<[MacroPluginId]>,
    ) {
        let mut overrides = self.macro_plugin_overrides().as_ref().clone();
        overrides.insert(crate_id, plugins);
        self.set_macro_plugin_overrides(Arc::new(overrides));
    }

    /// Overrides the default inline macro plugins available for [`CrateId`] with `plugins`.
    ///
    /// *Note*: Sets the following Salsa input: `DefsGroup::inline_macro_plugin_overrides`.
    fn set_override_crate_inline_macro_plugins(
        &mut self,
        crate_id: CrateId,
        plugins: Arc<OrderedHashMap<String, InlineMacroExprPluginId>>,
    ) {
        let mut overrides = self.inline_macro_plugin_overrides().as_ref().clone();
        overrides.insert(crate_id, plugins);
        self.set_inline_macro_plugin_overrides(Arc::new(overrides));
    }
}

impl<T: DefsGroup + ?Sized> DefsGroupEx for T {}
