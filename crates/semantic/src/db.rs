use defs::db::DefsGroup;
use defs::ids::{FreeFunctionId, ModuleId, StructId};

use crate::ids::*;
use crate::semantic;

// Salsa database interface.
#[salsa::query_group(HirDatabase)]
pub trait SemanticGroup: DefsGroup {
    #[salsa::interned]
    fn function_instance(&self, id: FunctionInstanceLongId) -> FunctionInstanceId;
    #[salsa::interned]
    fn type_instance(&self, id: Type) -> TypeId;
    #[salsa::interned]
    fn expr(&self, expr: semantic::Expr) -> ExprId;
    #[salsa::interned]
    fn statement(&self, statement: semantic::Statement) -> StatementId;

    fn module_semantic(&self, item: ModuleId) -> semantic::Module;
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    fn function_semantic(&self, item: FreeFunctionId) -> semantic::FreeFunction;
}

fn module_semantic(_db: &dyn SemanticGroup, _item: ModuleId) -> semantic::Module {
    todo!()
}

fn struct_semantic(_db: &dyn SemanticGroup, _item: StructId) -> semantic::Struct {
    todo!()
}

fn function_semantic(_db: &dyn SemanticGroup, _item: FreeFunctionId) -> semantic::FreeFunction {
    todo!()
}
