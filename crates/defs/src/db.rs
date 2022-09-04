use std::collections::HashMap;

use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::ModuleId;
use itertools::chain;
use parser::db::ParserGroup;
use parser::parser::ParserDiagnostic;
use smol_str::SmolStr;
use syntax::node::ast;
use syntax::node::db::{AsSyntaxGroup, SyntaxGroup};

use crate::ids::*;

/// Salsa database interface.
/// See [`super::ids`] for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup: FilesGroup + SyntaxGroup + AsSyntaxGroup + ParserGroup {
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
    fn intern_block(&self, id: BlockLongId) -> BlockId;
    #[salsa::interned]
    fn intern_local_var(&self, id: LocalVarLongId) -> LocalVarId;

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
    fn module_resolve_generic_function(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> WithDiagnostics<Option<GenericFunctionId>, ParserDiagnostic>;
    fn module_resolve_generic_type(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> WithDiagnostics<Option<GenericTypeId>, ParserDiagnostic>;
}

pub trait AsDefsGroup {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static);
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleData {
    pub free_functions: HashMap<FreeFunctionId, ast::ItemFunction>,
    pub structs: HashMap<StructId, ast::ItemStruct>,
    pub extern_types: HashMap<ExternTypeId, ast::ItemExternType>,
    pub extern_functions: HashMap<ExternFunctionId, ast::ItemExternFunction>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleItems {
    pub items: HashMap<SmolStr, ModuleItemId>,
}

#[with_diagnostics]
fn module_data(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Option<ModuleData> {
    let mut res = ModuleData::default();
    let syntax_db = db.as_syntax_group();

    let syntax_file = db.module_syntax(module_id).unwrap(diagnostics)?;
    for item in syntax_file.items(syntax_db).elements(syntax_db) {
        match item {
            ast::Item::Module(_module) => todo!(),
            ast::Item::Function(function) => {
                let name = function.name(syntax_db).text(syntax_db);
                let item_id =
                    db.intern_free_function(FreeFunctionLongId { parent: module_id, name });
                res.free_functions.insert(item_id, function);
            }
            ast::Item::ExternFunction(extern_function) => {
                let name = extern_function.name(syntax_db).text(syntax_db);
                let item_id =
                    db.intern_extern_function(ExternFunctionLongId { parent: module_id, name });
                res.extern_functions.insert(item_id, extern_function);
            }
            ast::Item::ExternType(extern_type) => {
                let name = extern_type.name(syntax_db).text(syntax_db);
                let item_id = db.intern_extern_type(ExternTypeLongId { parent: module_id, name });
                res.extern_types.insert(item_id, extern_type);
            }
            ast::Item::Trait(_tr) => todo!(),
            ast::Item::Impl(_imp) => todo!(),
            ast::Item::Struct(strct) => {
                let name = strct.name(syntax_db).text(syntax_db);
                let item_id = db.intern_struct(StructLongId { parent: module_id, name });
                res.structs.insert(item_id, strct);
            }
            ast::Item::Enum(_en) => todo!(),
            ast::Item::Use(_us) => todo!(),
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
    let module_data = db.module_data(module_id).unwrap(diagnostics)?;
    Some(ModuleItems {
        items: chain!(
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
    let module_items = db.module_items(module_id).unwrap(diagnostics)?;
    module_items.items.get(&name).copied()
}

#[with_diagnostics]
fn module_resolve_generic_function(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Option<GenericFunctionId> {
    match db.module_item_by_name(module_id, name).unwrap(diagnostics)? {
        ModuleItemId::FreeFunction(item) => Some(GenericFunctionId::Free(item)),
        ModuleItemId::Struct(_) => None,
        ModuleItemId::ExternType(_) => None,
        ModuleItemId::ExternFunction(item) => Some(GenericFunctionId::Extern(item)),
    }
}

#[with_diagnostics]
fn module_resolve_generic_type(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Option<GenericTypeId> {
    match db.module_item_by_name(module_id, name).unwrap(diagnostics)? {
        ModuleItemId::FreeFunction(_) => None,
        ModuleItemId::Struct(item) => Some(GenericTypeId::Struct(item)),
        ModuleItemId::ExternType(item) => Some(GenericTypeId::Extern(item)),
        ModuleItemId::ExternFunction(_) => None,
    }
}
