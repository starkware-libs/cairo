use std::sync::Arc;

use db_utils::Upcast;
use filesystem::db::FilesGroup;
use filesystem::ids::{CrateId, Directory, FileId, FileLongId, VirtualFile};
use itertools::chain;
use parser::db::ParserGroup;
use smol_str::SmolStr;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::GetIdentifier;
use syntax::node::{ast, Terminal, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use crate::ids::*;

/// Salsa database interface.
/// See [`super::ids`] for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup:
    FilesGroup + SyntaxGroup + Upcast<dyn SyntaxGroup> + ParserGroup + Upcast<dyn FilesGroup>
{
    #[salsa::interned]
    fn intern_virtual_submodule(&self, virtual_submodule: VirtualSubmodule) -> VirtualSubmoduleId;
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
    fn module_file(&self, module_id: ModuleId) -> Option<FileId>;
    fn module_dir(&self, module_id: ModuleId) -> Option<Directory>;
    fn module_syntax(&self, module_id: ModuleId) -> Option<Arc<SyntaxFile>>;

    // File to module.
    fn crate_modules(&self, crate_id: CrateId) -> Arc<Vec<ModuleId>>;
    fn priv_file_to_module_mapping(&self) -> OrderedHashMap<FileId, Vec<ModuleId>>;
    fn file_modules(&self, file_id: FileId) -> Option<Vec<ModuleId>>;

    // Module level resolving.
    fn module_data(&self, module_id: ModuleId) -> Option<ModuleData>;
    fn module_submodules(&self, module_id: ModuleId) -> Option<Vec<ModuleId>>;
    fn module_items(&self, module_id: ModuleId) -> Option<ModuleItems>;
    fn module_item_by_name(&self, module_id: ModuleId, name: SmolStr) -> Option<ModuleItemId>;

    // Plugins.
    #[salsa::input]
    fn macro_plugins(&self) -> Vec<Arc<dyn MacroPlugin>>;
}

// TOD(spapini): Move to another place.
/// A trait for a macro plugin: external plugin that generates additional code for items.
pub trait MacroPlugin: std::fmt::Debug + Sync + Send {
    /// Generates code for an item. If no code should be generated returns None.
    /// Otherwise, returns (virtual_module_name, module_content), and a virtual submodule
    /// with that name and content should be created.
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item)
    -> Option<(SmolStr, String)>;
}

/// Initializes a database witf DefsGroup.
pub fn init_defs_group(db: &mut (dyn DefsGroup + 'static)) {
    // Initialize inputs.
    db.set_macro_plugins(Vec::new());
}

fn module_file(db: &dyn DefsGroup, module_id: ModuleId) -> Option<FileId> {
    Some(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_root_dir(crate_id)?.file(db.upcast(), "lib.cairo".into())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.module(db);
            let name = submodule_id.name(db);
            db.module_dir(parent)?.file(db.upcast(), format!("{name}.cairo").into())
        }
        ModuleId::VirtualSubmodule(virtual_submodule_id) => {
            db.lookup_intern_virtual_submodule(virtual_submodule_id).file
        }
    })
}

fn module_dir(db: &dyn DefsGroup, module_id: ModuleId) -> Option<Directory> {
    match module_id {
        ModuleId::CrateRoot(crate_id) => db.crate_root_dir(crate_id),
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.module(db);
            let name = submodule_id.name(db);
            Some(db.module_dir(parent)?.subdir(name))
        }
        ModuleId::VirtualSubmodule(_) => None,
    }
}
pub fn module_syntax(db: &dyn DefsGroup, module_id: ModuleId) -> Option<Arc<SyntaxFile>> {
    db.file_syntax(db.module_file(module_id)?)
}

fn collect_modules_under(db: &dyn DefsGroup, modules: &mut Vec<ModuleId>, module_id: ModuleId) {
    modules.push(module_id);
    for submodule_module_id in db.module_submodules(module_id).iter().flatten() {
        collect_modules_under(db, modules, *submodule_module_id);
    }
}
fn crate_modules(db: &dyn DefsGroup, crate_id: CrateId) -> Arc<Vec<ModuleId>> {
    let mut modules = Vec::new();
    collect_modules_under(db, &mut modules, ModuleId::CrateRoot(crate_id));
    Arc::new(modules)
}
fn priv_file_to_module_mapping(db: &dyn DefsGroup) -> OrderedHashMap<FileId, Vec<ModuleId>> {
    let mut mapping = OrderedHashMap::<FileId, Vec<ModuleId>>::default();
    for crate_id in db.crates() {
        for module_id in db.crate_modules(crate_id).iter().copied() {
            if let Some(file_id) = db.module_file(module_id) {
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
    mapping
}
fn file_modules(db: &dyn DefsGroup, file_id: FileId) -> Option<Vec<ModuleId>> {
    db.priv_file_to_module_mapping().get(&file_id).cloned()
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleData {
    pub submodules: OrderedHashMap<SubmoduleId, ast::ItemModule>,
    pub uses: OrderedHashMap<UseId, ast::ItemUse>,
    pub free_functions: OrderedHashMap<FreeFunctionId, ast::ItemFreeFunction>,
    pub structs: OrderedHashMap<StructId, ast::ItemStruct>,
    pub enums: OrderedHashMap<EnumId, ast::ItemEnum>,
    pub traits: OrderedHashMap<TraitId, ast::ItemTrait>,
    pub impls: OrderedHashMap<ImplId, ast::ItemImpl>,
    pub extern_types: OrderedHashMap<ExternTypeId, ast::ItemExternType>,
    pub extern_functions: OrderedHashMap<ExternFunctionId, ast::ItemExternFunction>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleItems {
    pub items: OrderedHashMap<SmolStr, ModuleItemId>,
}

// TODO(spapini): Make this private.
fn module_data(db: &dyn DefsGroup, module_id: ModuleId) -> Option<ModuleData> {
    let mut res = ModuleData::default();
    let syntax_db = db.upcast();

    let syntax_file = db.module_syntax(module_id)?;
    for item in syntax_file.items(syntax_db).elements(syntax_db) {
        match item {
            ast::Item::Module(module) => {
                let item_id = db.intern_submodule(SubmoduleLongId(module_id, module.stable_ptr()));
                res.submodules.insert(item_id, module);
            }
            ast::Item::Use(us) => {
                let item_id = db.intern_use(UseLongId(module_id, us.stable_ptr()));
                res.uses.insert(item_id, us);
            }
            ast::Item::FreeFunction(function) => {
                let item_id =
                    db.intern_free_function(FreeFunctionLongId(module_id, function.stable_ptr()));
                res.free_functions.insert(item_id, function);
            }
            ast::Item::ExternFunction(extern_function) => {
                let item_id = db.intern_extern_function(ExternFunctionLongId(
                    module_id,
                    extern_function.stable_ptr(),
                ));
                res.extern_functions.insert(item_id, extern_function);
            }
            ast::Item::ExternType(extern_type) => {
                let item_id =
                    db.intern_extern_type(ExternTypeLongId(module_id, extern_type.stable_ptr()));
                res.extern_types.insert(item_id, extern_type);
            }
            ast::Item::Trait(trt) => {
                let item_id = db.intern_trait(TraitLongId(module_id, trt.stable_ptr()));
                res.traits.insert(item_id, trt);
            }
            ast::Item::Impl(imp) => {
                let item_id = db.intern_impl(ImplLongId(module_id, imp.stable_ptr()));
                res.impls.insert(item_id, imp);
            }
            ast::Item::Struct(strct) => {
                let item_id = db.intern_struct(StructLongId(module_id, strct.stable_ptr()));
                res.structs.insert(item_id, strct);
            }
            ast::Item::Enum(enm) => {
                let item_id = db.intern_enum(EnumLongId(module_id, enm.stable_ptr()));
                res.enums.insert(item_id, enm);
            }
        }
    }
    Some(res)
}

/// Finds all the submodules of a module - both explicit (using the "mod x" syntax) and virtual
/// submodules, generated by macro plugins.
fn module_submodules(db: &dyn DefsGroup, module_id: ModuleId) -> Option<Vec<ModuleId>> {
    let syntax_db = db.upcast();
    let mut submodules: Vec<_> =
        db.module_data(module_id)?.submodules.keys().copied().map(ModuleId::Submodule).collect();
    for plugin in db.macro_plugins() {
        for item_ast in db.module_syntax(module_id)?.items(syntax_db).elements(syntax_db) {
            let Some((name, content)) = plugin.generate_code(db.upcast(), item_ast) else {continue};
            let file = db.intern_file(FileLongId::Virtual(VirtualFile {
                parent: db.module_file(module_id),
                name: name.clone(),
                content: Arc::new(content),
            }));
            let submodule_id =
                ModuleId::VirtualSubmodule(db.intern_virtual_submodule(VirtualSubmodule {
                    name,
                    parent: module_id,
                    file,
                }));
            submodules.push(submodule_id);
        }
    }
    Some(submodules)
}

fn module_items(db: &dyn DefsGroup, module_id: ModuleId) -> Option<ModuleItems> {
    let syntax_db = db.upcast();
    let module_data = db.module_data(module_id)?;
    // TODO(spapini): Prune other items if name is missing.
    Some(ModuleItems {
        items: chain!(
            module_data.submodules.iter().map(|(submodule_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::Submodule(*submodule_id),
            )),
            module_data.uses.iter().map(|(use_id, syntax)| (
                syntax.name(syntax_db).identifier(syntax_db),
                ModuleItemId::Use(*use_id)
            )),
            module_data.free_functions.iter().map(|(free_function_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::FreeFunction(*free_function_id),
            )),
            module_data.extern_functions.iter().map(|(extern_function_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::ExternFunction(*extern_function_id),
            )),
            module_data.extern_types.iter().map(|(extern_type_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::ExternType(*extern_type_id),
            )),
            module_data.structs.iter().map(|(struct_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::Struct(*struct_id)
            )),
            module_data.enums.iter().map(|(enum_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::Enum(*enum_id)
            )),
            module_data.traits.iter().map(|(trait_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::Trait(*trait_id)
            )),
            module_data.impls.iter().map(|(impl_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::Impl(*impl_id)
            )),
        )
        .collect(),
    })
}

fn module_item_by_name(
    db: &dyn DefsGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Option<ModuleItemId> {
    let module_items = db.module_items(module_id)?;
    module_items.items.get(&name).copied()
}
