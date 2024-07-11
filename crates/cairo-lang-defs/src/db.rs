use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::{Maybe, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, Directory, FileId, FileKind, FileLongId, VirtualFile};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::attribute::consts::{
    DEPRECATED_ATTR, FEATURE_ATTR, FMT_SKIP_ATTR, IMPLICIT_PRECEDENCE_ATTR, INLINE_ATTR,
    INTERNAL_ATTR, MUST_USE_ATTR, PHANTOM_ATTR, STARKNET_INTERFACE_ATTR, UNSTABLE_ATTR,
};
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::element_list::ElementList;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};
use itertools::Itertools;

use crate::ids::*;
use crate::plugin::{
    DynGeneratedFileAuxData, InlineMacroExprPlugin, MacroPlugin, MacroPluginMetadata,
    PluginDiagnostic,
};

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

    // Plugins.
    // ========
    #[salsa::input]
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>>;
    #[salsa::input]
    fn inline_macro_plugins(&self) -> Arc<OrderedHashMap<String, Arc<dyn InlineMacroExprPlugin>>>;

    /// Returns the set of attributes allowed anywhere.
    /// An attribute on any item that is not in this set will be handled as an unknown attribute.
    fn allowed_attributes(&self) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of attributes allowed on statements.
    /// An attribute on a statement that is not in this set will be handled as an unknown attribute.
    fn allowed_statement_attributes(&self) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of `derive` that were declared as by a plugin.
    /// A derive that is not in this set will be handled as an unknown derive.
    fn declared_derives(&self) -> Arc<OrderedHashSet<String>>;

    /// Returns the set of attributes that were declared as phantom type attributes by a plugin,
    /// i.e. a type marked with this attribute is considered a phantom type.
    fn declared_phantom_type_attributes(&self) -> Arc<OrderedHashSet<String>>;

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
    fn module_generated_file_infos(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<[Option<GeneratedFileInfo>]>>;
    fn module_plugin_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Arc<[(ModuleFileId, PluginDiagnostic)]>>;
}

fn allowed_attributes(db: &dyn DefsGroup) -> Arc<OrderedHashSet<String>> {
    let mut all_attributes = OrderedHashSet::from_iter([
        INLINE_ATTR.into(),
        MUST_USE_ATTR.into(),
        UNSTABLE_ATTR.into(),
        DEPRECATED_ATTR.into(),
        INTERNAL_ATTR.into(),
        FEATURE_ATTR.into(),
        PHANTOM_ATTR.into(),
        IMPLICIT_PRECEDENCE_ATTR.into(),
        FMT_SKIP_ATTR.into(),
        // TODO(orizi): Remove this once `starknet` is removed from corelib.
        STARKNET_INTERFACE_ATTR.into(),
    ]);
    for plugin in db.macro_plugins() {
        all_attributes.extend(plugin.declared_attributes());
    }
    Arc::new(all_attributes)
}

fn allowed_statement_attributes(_db: &dyn DefsGroup) -> Arc<OrderedHashSet<String>> {
    let all_attributes = OrderedHashSet::from_iter([FMT_SKIP_ATTR.into(), FEATURE_ATTR.into()]);
    Arc::new(all_attributes)
}

fn declared_derives(db: &dyn DefsGroup) -> Arc<OrderedHashSet<String>> {
    let mut all_derives = OrderedHashSet::default();
    for plugin in db.macro_plugins() {
        all_derives.extend(plugin.declared_derives());
    }
    Arc::new(all_derives)
}

fn declared_phantom_type_attributes(db: &dyn DefsGroup) -> Arc<OrderedHashSet<String>> {
    let mut all_phantom_type_attributes = OrderedHashSet::from_iter([PHANTOM_ATTR.into()]);
    for plugin in db.macro_plugins() {
        all_phantom_type_attributes.extend(plugin.phantom_type_attributes());
    }
    Arc::new(all_phantom_type_attributes)
}

fn module_main_file(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<FileId> {
    Ok(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_config(crate_id).to_maybe()?.root.file(db.upcast(), "lib.cairo".into())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            let item_module_ast = &db.priv_module_data(parent)?.submodules[&submodule_id];
            match item_module_ast.body(db.upcast()) {
                MaybeModuleBody::Some(_) => {
                    // This is an inline module, we return the file where the inline module was
                    // defined. It can be either the file of the parent module
                    // or a plugin-generated virtual file.
                    db.module_file(submodule_id.module_file_id(db))?
                }
                MaybeModuleBody::None(_) => {
                    let name = submodule_id.name(db).lookup_intern(db);
                    db.module_dir(parent)?.file(db.upcast(), format!("{name}.cairo"))
                }
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
            let name = submodule_id.name(db).lookup_intern(db);
            Ok(db.module_dir(parent)?.subdir(name.to_string()))
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

/// Information about the generation of a virtual file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeneratedFileInfo {
    pub aux_data: Option<DynGeneratedFileAuxData>,
    /// The module and file index from which the current file was generated.
    pub origin: ModuleFileId,
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

    files: Vec<FileId>,
    /// Generation info for each file. Virtual files have Some. Other files have None.
    generated_file_infos: Vec<Option<GeneratedFileInfo>>,
    plugin_diagnostics: Vec<(ModuleFileId, PluginDiagnostic)>,
}

// TODO(spapini): Make this private.
fn priv_module_data(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<ModuleData> {
    let syntax_db = db.upcast();
    let module_file = db.module_main_file(module_id)?;

    let file_syntax = db.file_module_syntax(module_file)?;
    let mut main_file_info: Option<GeneratedFileInfo> = None;
    let item_asts = match module_id {
        ModuleId::CrateRoot(_) => file_syntax.items(syntax_db),
        ModuleId::Submodule(submodule_id) => {
            let parent_module_data = db.priv_module_data(submodule_id.parent_module(db))?;
            let item_module_ast = &parent_module_data.submodules[&submodule_id];

            match item_module_ast.body(syntax_db) {
                MaybeModuleBody::Some(body) => {
                    // TODO(spapini): Diagnostics in this module that get mapped to parent module
                    // should lie in that modules ModuleData, or somehow collected by its
                    // diagnostics collector function.

                    // If this is an inline module, copy its generation file info from the parent
                    // module, from the file where this submodule was defined.
                    main_file_info = parent_module_data
                        .generated_file_infos
                        .into_iter()
                        .nth(submodule_id.file_index(db).0)
                        .unwrap();
                    body.items(syntax_db)
                }
                MaybeModuleBody::None(_) => file_syntax.items(syntax_db),
            }
        }
    };

    let allowed_attributes = db.allowed_attributes();

    let mut module_queue = VecDeque::new();
    module_queue.push_back((module_file, item_asts));
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
    let mut generated_file_infos = Vec::new();
    let mut files = Vec::new();
    let mut plugin_diagnostics = Vec::new();

    let crate_id = module_id.owning_crate(db);
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
        declared_derives: &db.declared_derives(),
        edition,
    };

    let mut items = vec![];
    generated_file_infos.push(main_file_info);
    while let Some((module_file, item_asts)) = module_queue.pop_front() {
        let file_index = FileIndex(files.len());
        let module_file_id = ModuleFileId(module_id, file_index);
        files.push(module_file);

        for item_ast in item_asts.elements(syntax_db) {
            let mut remove_original_item = false;
            // Iterate the plugins by their order. The first one to change something (either
            // generate new code, remove the original code, or both), breaks the loop. If more
            // plugins might have act on the item, they can do it on the generated code.
            for plugin in db.macro_plugins() {
                let result = plugin.generate_code(db.upcast(), item_ast.clone(), &metadata);
                for plugin_diag in result.diagnostics {
                    plugin_diagnostics.push((module_file_id, plugin_diag));
                }
                if result.remove_original_item {
                    remove_original_item = true;
                }

                if let Some(generated) = result.code {
                    let new_file = FileLongId::Virtual(VirtualFile {
                        parent: Some(module_file),
                        name: generated.name,
                        content: generated.content.into(),
                        code_mappings: generated.code_mappings.into(),
                        kind: FileKind::Module,
                    })
                    .intern(db);
                    generated_file_infos.push(Some(GeneratedFileInfo {
                        aux_data: generated.aux_data,
                        origin: module_file_id,
                    }));
                    module_queue
                        .push_back((new_file, db.file_module_syntax(new_file)?.items(syntax_db)));
                }
                if remove_original_item {
                    break;
                }
            }
            if remove_original_item {
                // Don't add the original item to the module data.
                continue;
            }
            validate_attributes(
                syntax_db,
                &allowed_attributes,
                module_file_id,
                &item_ast,
                &mut plugin_diagnostics,
            );
            match item_ast {
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
                    let path_leaves = get_all_path_leaves(db.upcast(), us.use_path(syntax_db));
                    for path_leaf in path_leaves {
                        let path_leaf_id =
                            UseLongId(module_file_id, path_leaf.stable_ptr()).intern(db);
                        uses.insert(path_leaf_id, path_leaf);
                        items.push(ModuleItemId::Use(path_leaf_id));
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
                            inline_macro_ast.name(db.upcast()).text(db.upcast()).lookup_intern(db)
                        ),
                    ),
                )),
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
        files,
        generated_file_infos,
        plugin_diagnostics,
    };
    Ok(res)
}

/// Validates that all attributes on the given item are in the allowed set or adds diagnostics.
pub fn validate_attributes_flat(
    db: &dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    module_file_id: ModuleFileId,
    item: &impl QueryAttrs,
    plugin_diagnostics: &mut Vec<(ModuleFileId, PluginDiagnostic)>,
) {
    for attr in item.attributes_elements(db) {
        if !allowed_attributes.contains(&attr.attr(db).as_syntax_node().get_text_without_trivia(db))
        {
            plugin_diagnostics.push((
                module_file_id,
                PluginDiagnostic::error(&attr, "Unsupported attribute.".to_string()),
            ));
        }
    }
}

/// Validates that all attributes on all items in the given element list are in the allowed set or
/// adds diagnostics.
fn validate_attributes_element_list<Item: QueryAttrs + TypedSyntaxNode, const STEP: usize>(
    db: &dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    module_file_id: ModuleFileId,
    items: &ElementList<Item, STEP>,
    plugin_diagnostics: &mut Vec<(ModuleFileId, PluginDiagnostic)>,
) {
    for item in items.elements(db) {
        validate_attributes_flat(db, allowed_attributes, module_file_id, &item, plugin_diagnostics);
    }
}

/// Validates that all attributes on an item and on items contained within it are in the allowed set
/// or adds diagnostics.
fn validate_attributes(
    db: &dyn SyntaxGroup,
    allowed_attributes: &OrderedHashSet<String>,
    module_file_id: ModuleFileId,
    item_ast: &ast::ModuleItem,
    plugin_diagnostics: &mut Vec<(ModuleFileId, PluginDiagnostic)>,
) {
    validate_attributes_flat(db, allowed_attributes, module_file_id, item_ast, plugin_diagnostics);
    match item_ast {
        ast::ModuleItem::Trait(item) => {
            if let ast::MaybeTraitBody::Some(body) = item.body(db) {
                validate_attributes_element_list(
                    db,
                    allowed_attributes,
                    module_file_id,
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
                    module_file_id,
                    &body.items(db),
                    plugin_diagnostics,
                );
            }
        }
        ast::ModuleItem::Struct(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                module_file_id,
                &item.members(db),
                plugin_diagnostics,
            );
        }
        ast::ModuleItem::Enum(item) => {
            validate_attributes_element_list(
                db,
                allowed_attributes,
                module_file_id,
                &item.variants(db),
                plugin_diagnostics,
            );
        }
        _ => {}
    }
}

/// Returns all the path leaves under a given use path.
pub fn get_all_path_leaves(db: &dyn SyntaxGroup, use_path: ast::UsePath) -> Vec<ast::UsePathLeaf> {
    let mut res = vec![];
    get_all_path_leaves_inner(db, use_path, &mut res);
    res
}

/// Finds all the path leaves under a given use path and adds them to the given vector.
fn get_all_path_leaves_inner(
    db: &dyn SyntaxGroup,
    use_path: ast::UsePath,
    res: &mut Vec<ast::UsePathLeaf>,
) {
    match use_path {
        ast::UsePath::Leaf(use_path) => {
            res.push(use_path);
        }
        ast::UsePath::Single(use_path) => get_all_path_leaves_inner(db, use_path.use_path(db), res),
        ast::UsePath::Multi(use_path) => {
            for use_path in use_path.use_paths(db).elements(db) {
                get_all_path_leaves_inner(db, use_path, res);
            }
        }
    }
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
pub fn module_generated_file_infos(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[Option<GeneratedFileInfo>]>> {
    Ok(db.priv_module_data(module_id)?.generated_file_infos.into())
}

/// Returns all the plugin diagnostics of the given module.
pub fn module_plugin_diagnostics(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Arc<[(ModuleFileId, PluginDiagnostic)]>> {
    Ok(db.priv_module_data(module_id)?.plugin_diagnostics.into())
}

fn module_items(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<[ModuleItemId]>> {
    Ok(db.priv_module_data(module_id)?.items)
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
        ModuleItemId::Use(id) => {
            let use_leaf = &data.uses[id];
            match use_leaf.alias_clause(db) {
                ast::OptionAliasClause::Empty(_) => use_leaf.ident(db).stable_ptr().untyped(),
                ast::OptionAliasClause::AliasClause(alias) => {
                    alias.alias(db).stable_ptr().untyped()
                }
            }
        }
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
