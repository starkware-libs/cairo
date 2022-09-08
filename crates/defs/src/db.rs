use std::sync::Arc;

use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::{AsFilesGroup, FilesGroup};
use filesystem::ids::{Directory, FileId};
use itertools::chain;
use parser::db::ParserGroup;
use parser::ParserDiagnostic;
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
    fn module_syntax(
        &self,
        module_id: ModuleId,
    ) -> WithDiagnostics<Option<Arc<SyntaxFile>>, ParserDiagnostic>;

    // Module level resolving.
    fn module_data(
        &self,
        module_id: ModuleId,
    ) -> WithDiagnostics<Option<ModuleData>, ParserDiagnostic>;
    fn module_items(
        &self,
        module_id: ModuleId,
    ) -> WithDiagnostics<Option<ModuleItems>, ParserDiagnostic>;
    fn module_item_by_name(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> WithDiagnostics<Option<ModuleItemId>, ParserDiagnostic>;
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

#[with_diagnostics]
pub fn module_syntax(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Option<Arc<SyntaxFile>> {
    db.file_syntax(db.module_file(module_id)?).propagate(diagnostics)
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleData {
    pub modules: OrderedHashMap<SubmoduleId, ast::ItemModule>,
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

#[with_diagnostics]
fn module_data(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Option<ModuleData> {
    let mut res = ModuleData::default();
    let syntax_db = db.as_syntax_group();

    let syntax_file = db.module_syntax(module_id).propagate(diagnostics)?;
    for item in syntax_file.items(syntax_db).elements(syntax_db) {
        match item {
            ast::Item::Module(module) => {
                let item_id = db.intern_submodule(SubmoduleLongId(module_id, module.stable_ptr()));
                res.modules.insert(item_id, module);
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

#[with_diagnostics]
fn module_items(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Option<ModuleItems> {
    let syntax_db = db.as_syntax_group();
    let module_data = db.module_data(module_id).propagate(diagnostics)?;
    // TODO(spapini): Prune other items if name is missing.
    Some(ModuleItems {
        items: chain!(
            module_data.modules.iter().map(|(submodule_id, syntax)| (
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

#[with_diagnostics]
fn module_item_by_name(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Option<ModuleItemId> {
    let module_items = db.module_items(module_id).propagate(diagnostics)?;
    module_items.items.get(&name).copied()
}
