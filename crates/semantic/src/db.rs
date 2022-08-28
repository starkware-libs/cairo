use defs::db::DefsGroup;
use defs::ids::{FreeFunctionId, ModuleId, StructId};

use crate::ids::{
    ConcreteFunctionId, ConcreteFunctionLongId, ExprId, StatementId, TypeId, TypeLongId,
};
use crate::{semantic, ExprLongId, StatementLongId};

// Salsa database interface.
#[salsa::query_group(SemanticDatabase)]
pub trait SemanticGroup: DefsGroup {
    #[salsa::interned]
    fn function_instance(&self, id: ConcreteFunctionLongId) -> ConcreteFunctionId;
    #[salsa::interned]
    fn type_instance(&self, id: TypeLongId) -> TypeId;
    #[salsa::interned]
    fn expr(&self, expr: ExprLongId) -> ExprId;
    #[salsa::interned]
    fn statement(&self, statement: StatementLongId) -> StatementId;

    // Queries to compute the semantic model for definitions.
    fn module_semantic(&self, item: ModuleId) -> semantic::Module;
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    fn function_semantic(&self, item: FreeFunctionId) -> semantic::FreeFunction;
    fn expr_semantic(&self, item: ExprId) -> semantic::Expr;
    fn statement_semantic(&self, item: StatementId) -> semantic::Statement;
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

fn expr_semantic(db: &dyn SemanticGroup, item: ExprId) -> semantic::Expr {
    db.lookup_expr(item).expr
}

fn statement_semantic(db: &dyn SemanticGroup, item: StatementId) -> semantic::Statement {
    db.lookup_statement(item).statement
}
