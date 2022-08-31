use filesystem::db::FilesGroup;
use filesystem::ids::ModuleId;
use parser::db::ParserGroup;
use syntax::node::ast;
use syntax::node::db::{AsSyntaxGroup, SyntaxGroup};

use crate::ids::*;

// Salsa database interface.
// See ids.rs for further details.
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

    fn module_defs(&self, item: ModuleId) -> Option<ModuleDefs>;
}

pub trait AsDefsGroup {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static);
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModuleDefs {
    items: Vec<ModuleItemId>,
}

fn module_defs(db: &dyn DefsGroup, module_id: ModuleId) -> Option<ModuleDefs> {
    let syntax_group = db.as_syntax_group();

    let syntax_file = db.file_syntax(db.module_file(module_id)?)?;
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
            ast::Item::FunctionSignature(sig) => {
                items.push(ModuleItemId::ExternFunction(db.intern_extern_function(
                    ExternFunctionLongId {
                        parent: module_id,
                        name: sig.signature(syntax_group).name(syntax_group).text(syntax_group),
                    },
                )));
            }
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
    Some(ModuleDefs { items })
}
