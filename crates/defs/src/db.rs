use std::collections::HashMap;

use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::ModuleId;
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

    fn module_items(
        &self,
        module_id: ModuleId,
    ) -> WithDiagnostics<Option<ModuleItems>, ParserDiagnostic>;
    fn resolve_module_identifier(
        &self,
        module_id: ModuleId,
        name: SmolStr,
    ) -> WithDiagnostics<Option<ModuleItemId>, ParserDiagnostic>;
}

pub trait AsDefsGroup {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static);
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleItems {
    pub items: HashMap<SmolStr, ModuleItemId>,
}

#[with_diagnostics]
fn module_items(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Option<ModuleItems> {
    let syntax_group = db.as_syntax_group();

    let syntax_file = db.file_syntax(db.module_file(module_id)?).unwrap(diagnostics)?;
    Some(ModuleItems {
        items: HashMap::from_iter(
            syntax_file.items(syntax_group).elements(syntax_group).iter().map(|item| match item {
                ast::Item::Module(_module) => todo!(),
                ast::Item::Function(function) => {
                    let name =
                        function.signature(syntax_group).name(syntax_group).text(syntax_group);
                    (
                        name.clone(),
                        ModuleItemId::FreeFunction(
                            db.intern_free_function(FreeFunctionLongId { parent: module_id, name }),
                        ),
                    )
                }
                ast::Item::ExternFunction(extern_function) => {
                    let name = extern_function
                        .signature(syntax_group)
                        .name(syntax_group)
                        .text(syntax_group);
                    (
                        name.clone(),
                        ModuleItemId::ExternFunction(db.intern_extern_function(
                            ExternFunctionLongId { parent: module_id, name },
                        )),
                    )
                }
                ast::Item::ExternType(_extern_type) => todo!(),
                ast::Item::Trait(_tr) => todo!(),
                ast::Item::Impl(_imp) => todo!(),
                ast::Item::Struct(strct) => {
                    let name = strct.name(db.as_syntax_group()).text(db.as_syntax_group());
                    (
                        name.clone(),
                        ModuleItemId::Struct(
                            db.intern_struct(StructLongId { parent: module_id, name }),
                        ),
                    )
                }
                ast::Item::Enum(_en) => todo!(),
                ast::Item::Use(_us) => todo!(),
            }),
        ),
    })
}

#[with_diagnostics]
fn resolve_module_identifier(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
    name: SmolStr,
) -> Option<ModuleItemId> {
    let module_items = db.module_items(module_id).unwrap(diagnostics)?;
    module_items.items.get(&name).copied()
}
