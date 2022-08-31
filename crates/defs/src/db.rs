use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::ModuleId;
use parser::db::ParserGroup;
use parser::parser::ParserDiagnostic;
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
}

pub trait AsDefsGroup {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static);
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModuleItems {
    items: Vec<ModuleItemId>,
}

#[with_diagnostics]
fn module_items(
    diagnostics: &mut Diagnostics<ParserDiagnostic>,
    db: &dyn DefsGroup,
    module_id: ModuleId,
) -> Option<ModuleItems> {
    let syntax_group = db.as_syntax_group();

    let syntax_file = db.file_syntax(db.module_file(module_id)?).unwrap(diagnostics)?;
    let mut items = Vec::new();
    for item in syntax_file.items(syntax_group).elements(syntax_group) {
        match item {
            ast::Item::Module(_module) => todo!(),
            ast::Item::Function(function) => {
                items.push(ModuleItemId::FreeFunction(db.intern_free_function(
                    FreeFunctionLongId {
                        parent: module_id,
                        name:
                            function.signature(syntax_group).name(syntax_group).text(syntax_group),
                    },
                )));
            }
            ast::Item::ExternFunction(extern_function) => {
                items.push(ModuleItemId::ExternFunction(
                    db.intern_extern_function(ExternFunctionLongId {
                        parent: module_id,
                        name: extern_function
                            .signature(syntax_group)
                            .name(syntax_group)
                            .text(syntax_group),
                    }),
                ));
            }
            ast::Item::ExternType(_extern_type) => todo!(),
            ast::Item::Trait(_tr) => todo!(),
            ast::Item::Impl(_imp) => todo!(),
            ast::Item::Struct(strct) => {
                items.push(ModuleItemId::Struct(db.intern_struct(StructLongId {
                    parent: module_id,
                    name: strct.name(db.as_syntax_group()).text(db.as_syntax_group()),
                })));
            }
            ast::Item::Enum(_en) => todo!(),
            ast::Item::Use(_us) => todo!(),
        }
    }
    Some(ModuleItems { items })
}
