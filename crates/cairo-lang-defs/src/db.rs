use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_diagnostics::{Maybe, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, Directory, FileId, FileLongId, VirtualFile};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;
use itertools::chain;

use crate::ids::*;
use crate::plugin::{DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic};

/// Salsa database interface.
/// See [`super::ids`] for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup:
    FilesGroup
    + SyntaxGroup
    + Upcast<dyn SyntaxGroup>
    + ParserGroup
    + Upcast<dyn FilesGroup>
    + HasMacroPlugins
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
    fn intern_impl_function(&self, id: ImplFunctionLongId) -> ImplFunctionId;
    #[salsa::interned]
    fn intern_struct(&self, id: StructLongId) -> StructId;
    #[salsa::interned]
    fn intern_enum(&self, id: EnumLongId) -> EnumId;
    #[salsa::interned]
    fn intern_type_alias(&self, id: TypeAliasLongId) -> TypeAliasId;
    #[salsa::interned]
    fn intern_member(&self, id: MemberLongId) -> MemberId;
    #[salsa::interned]
    fn intern_variant(&self, id: VariantLongId) -> VariantId;
    #[salsa::interned]
    fn intern_trait(&self, id: TraitLongId) -> TraitId;
    #[salsa::interned]
    fn intern_trait_function(&self, id: TraitFunctionLongId) -> TraitFunctionId;
    #[salsa::interned]
    fn intern_impl(&self, id: ImplLongId) -> ImplId;
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

    // Module to syntax.
    /// Gets the main file of the module.
    /// A module might have more virtual files generated by plugins.
    fn module_main_file(&self, module_id: ModuleId) -> Maybe<FileId>;
    /// Gets all the files of a module - main files and generated virtual files.
    fn module_files(&self, module_id: ModuleId) -> Maybe<Vec<FileId>>;
    /// Gets a file from a module and a FileIndex (i.e. ModuleFileId).
    fn module_file(&self, module_id: ModuleFileId) -> Maybe<FileId>;
    /// Gets the directory of a module.
    fn module_dir(&self, module_id: ModuleId) -> Maybe<Directory>;

    // File to module.
    fn crate_modules(&self, crate_id: CrateId) -> Arc<Vec<ModuleId>>;
    fn priv_file_to_module_mapping(&self) -> OrderedHashMap<FileId, Vec<ModuleId>>;
    fn file_modules(&self, file_id: FileId) -> Maybe<Vec<ModuleId>>;

    // Module level resolving.
    fn priv_module_data(&self, module_id: ModuleId) -> Maybe<ModuleData>;
    fn module_submodules(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<SubmoduleId, ast::ItemModule>>;
    fn module_submodules_ids(&self, module_id: ModuleId) -> Maybe<Vec<SubmoduleId>>;
    fn module_constants(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<ConstantId, ast::ItemConstant>>;
    fn module_constants_ids(&self, module_id: ModuleId) -> Maybe<Vec<ConstantId>>;
    fn module_free_functions(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<FreeFunctionId, ast::FunctionWithBody>>;
    fn module_free_functions_ids(&self, module_id: ModuleId) -> Maybe<Vec<FreeFunctionId>>;
    fn module_items(&self, module_id: ModuleId) -> Maybe<Arc<Vec<ModuleItemId>>>;
    fn module_uses(&self, module_id: ModuleId) -> Maybe<OrderedHashMap<UseId, ast::ItemUse>>;
    fn module_uses_ids(&self, module_id: ModuleId) -> Maybe<Vec<UseId>>;
    fn module_structs(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<StructId, ast::ItemStruct>>;
    fn module_structs_ids(&self, module_id: ModuleId) -> Maybe<Vec<StructId>>;
    fn module_enums(&self, module_id: ModuleId) -> Maybe<OrderedHashMap<EnumId, ast::ItemEnum>>;
    fn module_enums_ids(&self, module_id: ModuleId) -> Maybe<Vec<EnumId>>;
    fn module_type_aliases(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<TypeAliasId, ast::ItemTypeAlias>>;
    fn module_type_aliases_ids(&self, module_id: ModuleId) -> Maybe<Vec<TypeAliasId>>;
    fn module_traits(&self, module_id: ModuleId) -> Maybe<OrderedHashMap<TraitId, ast::ItemTrait>>;
    fn module_traits_ids(&self, module_id: ModuleId) -> Maybe<Vec<TraitId>>;
    fn module_impls(&self, module_id: ModuleId) -> Maybe<OrderedHashMap<ImplId, ast::ItemImpl>>;
    fn module_impls_ids(&self, module_id: ModuleId) -> Maybe<Vec<ImplId>>;
    fn module_extern_types(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<ExternTypeId, ast::ItemExternType>>;
    fn module_extern_types_ids(&self, module_id: ModuleId) -> Maybe<Vec<ExternTypeId>>;
    fn module_extern_functions(
        &self,
        module_id: ModuleId,
    ) -> Maybe<OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>>;
    fn module_extern_functions_ids(&self, module_id: ModuleId) -> Maybe<Vec<ExternFunctionId>>;
    fn module_generated_file_info(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Vec<Option<GeneratedFileInfo>>>;
    fn module_plugin_diagnostics(
        &self,
        module_id: ModuleId,
    ) -> Maybe<Vec<(ModuleFileId, PluginDiagnostic)>>;
}

pub trait HasMacroPlugins {
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>>;
}

fn module_main_file(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<FileId> {
    Ok(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_root_dir(crate_id).to_maybe()?.file(db.upcast(), "lib.cairo".into())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.parent_module(db);
            let item_module_ast = &db.priv_module_data(parent)?.submodules[submodule_id];
            match item_module_ast.body(db.upcast()) {
                MaybeModuleBody::Some(_) => {
                    // This is an inline module, we return the file where the inline module was
                    // defined. It can be either the file of the parent module
                    // or a plugin-generated virtual file.
                    db.module_file(submodule_id.module_file_id(db))?
                }
                MaybeModuleBody::None(_) => {
                    let name = submodule_id.name(db);
                    db.module_dir(parent)?.file(db.upcast(), format!("{name}.cairo").into())
                }
            }
        }
    })
}

fn module_files(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<FileId>> {
    Ok(db.priv_module_data(module_id)?.files)
}

fn module_file(db: &dyn DefsGroup, module_file_id: ModuleFileId) -> Maybe<FileId> {
    Ok(db.module_files(module_file_id.0)?[module_file_id.1.0])
}

fn module_dir(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Directory> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => db.crate_root_dir(crate_id).to_maybe(),
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
    for submodule_module_id in db.module_submodules_ids(module_id).unwrap_or_default().into_iter() {
        collect_modules_under(db, modules, ModuleId::Submodule(submodule_module_id));
    }
}

/// Returns all the modules in the crate, including recursively.
fn crate_modules(db: &dyn DefsGroup, crate_id: CrateId) -> Arc<Vec<ModuleId>> {
    let mut modules = Vec::new();
    collect_modules_under(db, &mut modules, ModuleId::CrateRoot(crate_id));
    Arc::new(modules)
}

fn priv_file_to_module_mapping(db: &dyn DefsGroup) -> OrderedHashMap<FileId, Vec<ModuleId>> {
    let mut mapping = OrderedHashMap::<FileId, Vec<ModuleId>>::default();
    for crate_id in db.crates() {
        for module_id in db.crate_modules(crate_id).iter().copied() {
            if let Ok(files) = db.module_files(module_id) {
                for file_id in files {
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
    mapping
}
fn file_modules(db: &dyn DefsGroup, file_id: FileId) -> Maybe<Vec<ModuleId>> {
    db.priv_file_to_module_mapping().get(&file_id).cloned().to_maybe()
}

/// Information about the generation of a virtual file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GeneratedFileInfo {
    pub aux_data: DynGeneratedFileAuxData,
    /// The module and file index from which the current file was generated.
    pub origin: ModuleFileId,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleData {
    constants: OrderedHashMap<ConstantId, ast::ItemConstant>,
    submodules: OrderedHashMap<SubmoduleId, ast::ItemModule>,
    uses: OrderedHashMap<UseId, ast::ItemUse>,
    free_functions: OrderedHashMap<FreeFunctionId, ast::FunctionWithBody>,
    structs: OrderedHashMap<StructId, ast::ItemStruct>,
    enums: OrderedHashMap<EnumId, ast::ItemEnum>,
    type_aliases: OrderedHashMap<TypeAliasId, ast::ItemTypeAlias>,
    traits: OrderedHashMap<TraitId, ast::ItemTrait>,
    impls: OrderedHashMap<ImplId, ast::ItemImpl>,
    extern_types: OrderedHashMap<ExternTypeId, ast::ItemExternType>,
    extern_functions: OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>,
    files: Vec<FileId>,
    /// Generation info for each file. Virtual files have Some. Other files have None.
    generated_file_info: Vec<Option<GeneratedFileInfo>>,
    plugin_diagnostics: Vec<(ModuleFileId, PluginDiagnostic)>,
}

// TODO(spapini): Make this private.
fn priv_module_data(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<ModuleData> {
    let syntax_db = db.upcast();
    let module_file = db.module_main_file(module_id)?;

    let file_syntax = db.file_syntax(module_file)?;
    let mut main_file_info: Option<GeneratedFileInfo> = None;
    let item_asts = match module_id {
        ModuleId::CrateRoot(_) => file_syntax.items(syntax_db),
        ModuleId::Submodule(submodule_id) => {
            let parent_module_data = db.priv_module_data(submodule_id.parent_module(db))?;
            let item_module_ast = &parent_module_data.submodules[submodule_id];

            match item_module_ast.body(syntax_db) {
                MaybeModuleBody::Some(body) => {
                    // TODO(spapini): Diagnostics in this module that get mapped to parent module
                    // should lie in that modules ModuleData, or somehow collected by its
                    // diagnostics collector function.

                    // If this is an inline module, copy its generation file info from the parent
                    // module, from the file where this submodule was defined.
                    main_file_info = parent_module_data.generated_file_info
                        [submodule_id.file_index(db).0]
                        .clone();
                    body.items(syntax_db)
                }
                MaybeModuleBody::None(_) => file_syntax.items(syntax_db),
            }
        }
    };

    let mut module_queue = VecDeque::new();
    module_queue.push_back((module_file, item_asts));
    let mut res = ModuleData::default();

    res.generated_file_info.push(main_file_info);
    while let Some((module_file, item_asts)) = module_queue.pop_front() {
        let file_index = FileIndex(res.files.len());
        let module_file_id = ModuleFileId(module_id, file_index);
        res.files.push(module_file);

        for item_ast in item_asts.elements(syntax_db) {
            let mut remove_original_item = false;
            // Iterate the plugins by their order. The first one to change something (either
            // generate new code, remove the original code, or both), breaks the loop. If more
            // plugins might have act on the item, they can do it on the generated code.
            for plugin in db.macro_plugins() {
                let result = plugin.generate_code(db.upcast(), item_ast.clone());
                for plugin_diag in result.diagnostics {
                    res.plugin_diagnostics.push((module_file_id, plugin_diag));
                }
                if result.remove_original_item {
                    remove_original_item = true;
                }

                if let Some(generated) = result.code {
                    let new_file = db.intern_file(FileLongId::Virtual(VirtualFile {
                        parent: Some(module_file),
                        name: generated.name,
                        content: Arc::new(generated.content),
                    }));
                    res.generated_file_info.push(Some(GeneratedFileInfo {
                        aux_data: generated.aux_data,
                        origin: module_file_id,
                    }));
                    module_queue.push_back((new_file, db.file_syntax(new_file)?.items(syntax_db)));
                    // New code was generated for this item. If there are more plugins that should
                    // operate on it, they should operate on the result (the rest of the attributes
                    // should be copied to the new generated code).
                    break;
                }
                if remove_original_item {
                    break;
                }
            }
            if remove_original_item {
                // Don't add the original item to the module data.
                continue;
            }
            match item_ast {
                ast::Item::Constant(constant) => {
                    let item_id =
                        db.intern_constant(ConstantLongId(module_file_id, constant.stable_ptr()));
                    res.constants.insert(item_id, constant);
                }
                ast::Item::Module(module) => {
                    let item_id =
                        db.intern_submodule(SubmoduleLongId(module_file_id, module.stable_ptr()));
                    res.submodules.insert(item_id, module);
                }
                ast::Item::Use(us) => {
                    let item_id = db.intern_use(UseLongId(module_file_id, us.stable_ptr()));
                    res.uses.insert(item_id, us);
                }
                ast::Item::FreeFunction(function) => {
                    let item_id = db.intern_free_function(FreeFunctionLongId(
                        module_file_id,
                        function.stable_ptr(),
                    ));
                    res.free_functions.insert(item_id, function);
                }
                ast::Item::ExternFunction(extern_function) => {
                    let item_id = db.intern_extern_function(ExternFunctionLongId(
                        module_file_id,
                        extern_function.stable_ptr(),
                    ));
                    res.extern_functions.insert(item_id, extern_function);
                }
                ast::Item::ExternType(extern_type) => {
                    let item_id = db.intern_extern_type(ExternTypeLongId(
                        module_file_id,
                        extern_type.stable_ptr(),
                    ));
                    res.extern_types.insert(item_id, extern_type);
                }
                ast::Item::Trait(trt) => {
                    let item_id = db.intern_trait(TraitLongId(module_file_id, trt.stable_ptr()));
                    res.traits.insert(item_id, trt);
                }
                ast::Item::Impl(imp) => {
                    let item_id = db.intern_impl(ImplLongId(module_file_id, imp.stable_ptr()));
                    res.impls.insert(item_id, imp);
                }
                ast::Item::Struct(strct) => {
                    let item_id =
                        db.intern_struct(StructLongId(module_file_id, strct.stable_ptr()));
                    res.structs.insert(item_id, strct);
                }
                ast::Item::Enum(enm) => {
                    let item_id = db.intern_enum(EnumLongId(module_file_id, enm.stable_ptr()));
                    res.enums.insert(item_id, enm);
                }
                ast::Item::TypeAlias(type_alias) => {
                    let item_id = db.intern_type_alias(TypeAliasLongId(
                        module_file_id,
                        type_alias.stable_ptr(),
                    ));
                    res.type_aliases.insert(item_id, type_alias);
                }
            }
        }
    }
    Ok(res)
}

/// Returns all the constant definitions of the given module.
pub fn module_constants(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<ConstantId, ast::ItemConstant>> {
    Ok(db.priv_module_data(module_id)?.constants)
}
pub fn module_constants_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<ConstantId>> {
    Ok(db.module_constants(module_id)?.keys().copied().collect())
}

/// Returns all the *direct* submodules of the given module - including those generated by macro
/// plugins. To get all the submodules including nested modules, use [`collect_modules_under`].
fn module_submodules(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<SubmoduleId, ast::ItemModule>> {
    Ok(db.priv_module_data(module_id)?.submodules)
}
fn module_submodules_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<SubmoduleId>> {
    Ok(db.module_submodules(module_id)?.keys().copied().collect())
}

/// Returns all the free functions of the given module.
pub fn module_free_functions(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<FreeFunctionId, ast::FunctionWithBody>> {
    Ok(db.priv_module_data(module_id)?.free_functions)
}
pub fn module_free_functions_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Vec<FreeFunctionId>> {
    Ok(db.module_free_functions(module_id)?.keys().copied().collect())
}

/// Returns all the uses of the given module.
pub fn module_uses(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<UseId, ast::ItemUse>> {
    Ok(db.priv_module_data(module_id)?.uses)
}
pub fn module_uses_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<UseId>> {
    Ok(db.module_uses(module_id)?.keys().copied().collect())
}

/// Returns all the structs of the given module.
pub fn module_structs(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<StructId, ast::ItemStruct>> {
    Ok(db.priv_module_data(module_id)?.structs)
}
pub fn module_structs_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<StructId>> {
    Ok(db.module_structs(module_id)?.keys().copied().collect())
}

/// Returns all the enums of the given module.
pub fn module_enums(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<EnumId, ast::ItemEnum>> {
    Ok(db.priv_module_data(module_id)?.enums)
}
pub fn module_enums_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<EnumId>> {
    Ok(db.module_enums(module_id)?.keys().copied().collect())
}

/// Returns all the type aliases of the given module.
pub fn module_type_aliases(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<TypeAliasId, ast::ItemTypeAlias>> {
    Ok(db.priv_module_data(module_id)?.type_aliases)
}
pub fn module_type_aliases_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<TypeAliasId>> {
    Ok(db.module_type_aliases(module_id)?.keys().copied().collect())
}

/// Returns all the traits of the given module.
pub fn module_traits(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<TraitId, ast::ItemTrait>> {
    Ok(db.priv_module_data(module_id)?.traits)
}
pub fn module_traits_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<TraitId>> {
    Ok(db.module_traits(module_id)?.keys().copied().collect())
}

/// Returns all the impls of the given module.
pub fn module_impls(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<ImplId, ast::ItemImpl>> {
    Ok(db.priv_module_data(module_id)?.impls)
}
pub fn module_impls_ids(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Vec<ImplId>> {
    Ok(db.module_impls(module_id)?.keys().copied().collect())
}

/// Returns all the extern_types of the given module.
pub fn module_extern_types(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<ExternTypeId, ast::ItemExternType>> {
    Ok(db.priv_module_data(module_id)?.extern_types)
}
pub fn module_extern_types_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Vec<ExternTypeId>> {
    Ok(db.module_extern_types(module_id)?.keys().copied().collect())
}

/// Returns all the extern_functions of the given module.
pub fn module_extern_functions(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>> {
    Ok(db.priv_module_data(module_id)?.extern_functions)
}
pub fn module_extern_functions_ids(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Vec<ExternFunctionId>> {
    Ok(db.module_extern_functions(module_id)?.keys().copied().collect())
}

/// Returns the generated_file_info of the given module.
pub fn module_generated_file_info(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Vec<Option<GeneratedFileInfo>>> {
    Ok(db.priv_module_data(module_id)?.generated_file_info)
}

/// Returns all the plugin diagnostics of the given module.
pub fn module_plugin_diagnostics(
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Maybe<Vec<(ModuleFileId, PluginDiagnostic)>> {
    Ok(db.priv_module_data(module_id)?.plugin_diagnostics)
}

fn module_items(db: &dyn DefsGroup, module_id: ModuleId) -> Maybe<Arc<Vec<ModuleItemId>>> {
    // TODO(ilya): Consider returning the items in ast order.
    Ok(Arc::new(
        chain!(
            db.module_constants_ids(module_id)?.into_iter().map(ModuleItemId::Constant),
            db.module_submodules_ids(module_id)?.into_iter().map(ModuleItemId::Submodule),
            db.module_uses_ids(module_id)?.into_iter().map(ModuleItemId::Use),
            db.module_free_functions_ids(module_id)?.into_iter().map(ModuleItemId::FreeFunction),
            db.module_extern_functions_ids(module_id)?
                .into_iter()
                .map(ModuleItemId::ExternFunction),
            db.module_extern_types_ids(module_id)?.into_iter().map(ModuleItemId::ExternType),
            db.module_structs_ids(module_id)?.into_iter().map(ModuleItemId::Struct),
            db.module_enums_ids(module_id)?.into_iter().map(ModuleItemId::Enum),
            db.module_type_aliases_ids(module_id)?.into_iter().map(ModuleItemId::TypeAlias),
            db.module_traits_ids(module_id)?.into_iter().map(ModuleItemId::Trait),
            db.module_impls_ids(module_id)?.into_iter().map(ModuleItemId::Impl),
        )
        .collect(),
    ))
}
