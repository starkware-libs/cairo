use defs::db::DefsGroup;
use defs::ids::{FunctionId, ModuleId};

use crate::ids::*;
use crate::semantic;

// Salsa database interface.
#[salsa::query_group(HirDatabase)]
pub trait SemanticGroup: DefsGroup {
    #[salsa::interned]
    fn function_instance(&self, id: FunctionInstanceLongId) -> FunctionInstanceId;
    #[salsa::interned]
    fn type_instance(&self, id: TypeInstanceLongId) -> TypeInstanceId;
    #[salsa::interned]
    fn expr(&self, expr: semantic::Expr) -> ExprId;
    #[salsa::interned]
    fn statement(&self, statement: semantic::Statement) -> StatementId;

    fn module_semantic(&self, item: ModuleId) -> semantic::Module;
    fn function_semantic(&self, item: FunctionId) -> semantic::Function;
}

fn module_semantic(_db: &dyn SemanticGroup, _item: ModuleId) -> semantic::Module {
    todo!()
}

fn function_semantic(_db: &dyn SemanticGroup, _item: FunctionId) -> semantic::Function {
    todo!()
}
