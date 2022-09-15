use std::sync::Arc;

use filesystem::db::{AsFilesGroup, FilesGroup};
use filesystem::ids::{CrateId, Directory, FileId};
use itertools::chain;
use parser::db::ParserGroup;
use smol_str::SmolStr;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::{AsSyntaxGroup, SyntaxGroup};
use syntax::node::{ast, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use crate::ids::*;

/// Salsa database interface.
/// See [`super::ids`] for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup: FilesGroup + SyntaxGroup + AsSyntaxGroup + ParserGroup + AsFilesGroup {
    #[salsa::interned]
    fn intern_submodule(&self, id: SubmoduleLongId) -> SubmoduleId;
    #[salsa::interned]
    fn intern_use(&self, id: UseLongId) -> UseId;
    #[salsa::interned]
    fn intern_free_function(&self, id: FreeFunctionLongId) -> FreeFunctionId;
    #[salsa::interned]
    fn intern_struct(&self, id: StructLongId) -> StructId;
    #[salsa::interned]
    fn intern_member(&self, id: MemberLongId) -> MemberId;
    #[salsa::interned]
    fn intern_extern_type(&self, id: ExternTypeLongId) -> ExternTypeId;
    #[salsa::interned]
    fn intern_extern_function(&self, id: ExternFunctionLongId) -> ExternFunctionId;
    #[salsa::interned]
    fn intern_param(&self, id: ParamLongId) -> ParamId;
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
}

pub trait AsDefsGroup {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static);
}

fn module_file(db: &dyn DefsGroup, module_id: ModuleId) -> Option<FileId> {
    Some(match module_id {
        ModuleId::CrateRoot(crate_id) => {
            db.crate_root_dir(crate_id)?.file(db.as_files_group(), "lib.cairo".into())
        }
        ModuleId::Submodule(submodule_id) => {
            let parent = submodule_id.module(db);
            let name = submodule_id.name(db);
            db.module_dir(parent)?.file(db.as_files_group(), format!("{name}.cairo").into())
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
    let syntax_db = db.as_syntax_group();

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
            ast::Item::Trait(_tr) => todo!(),
            ast::Item::Impl(_imp) => todo!(),
            ast::Item::Struct(strct) => {
                let item_id = db.intern_struct(StructLongId(module_id, strct.stable_ptr()));
                res.structs.insert(item_id, strct);
            }
            ast::Item::Enum(_en) => todo!(),
        }
    }
    Some(res)
}

fn module_submodules(db: &dyn DefsGroup, module_id: ModuleId) -> Option<Vec<ModuleId>> {
    Some(db.module_data(module_id)?.submodules.keys().copied().map(ModuleId::Submodule).collect())
}

fn module_items(db: &dyn DefsGroup, module_id: ModuleId) -> Option<ModuleItems> {
    let syntax_db = db.as_syntax_group();
    let module_data = db.module_data(module_id)?;
    // TODO(spapini): Prune other items if name is missing.
    Some(ModuleItems {
        items: chain!(
            module_data.submodules.iter().map(|(submodule_id, syntax)| (
                syntax.name(syntax_db).text(syntax_db),
                ModuleItemId::Submodule(*submodule_id),
            )),
            module_data.uses.iter().flat_map(|(use_id, syntax)| (syntax
                .name(syntax_db)
                .elements(syntax_db)
                .last()
                .map(|segment| (
                    segment.ident(syntax_db).text(syntax_db),
                    ModuleItemId::Use(*use_id)
                )))),
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
