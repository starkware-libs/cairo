use defs::db::DefsGroup;
use defs::ids::{EnumId, FunctionId, ImplId, ModuleId, StructId, TraitId};

use crate::ids::*;
use crate::semantic;

// Salsa database interface.
#[salsa::query_group(HirDatabase)]
pub trait SemanticGroup: DefsGroup {
    #[salsa::interned]
    fn function_instance(&self, id: FunctionInstanceLongId) -> FunctionInstanceId;
    #[salsa::interned]
    fn impl_instance(&self, id: ImplInstanceLongId) -> ImplInstanceId;
    #[salsa::interned]
    fn type_instance(&self, id: TypeInstanceLongId) -> TypeInstanceId;
    #[salsa::interned]
    fn expr(&self, expr: semantic::Expr) -> ExprId;
    #[salsa::interned]
    fn statement(&self, statement: semantic::Statement) -> StatementId;

    fn module_semantic(&self, item: ModuleId) -> semantic::Module;
    fn function_semantic(&self, item: FunctionId) -> semantic::Function;
    fn struct_semantic(&self, item: StructId) -> semantic::Struct;
    fn enum_semantic(&self, item: EnumId) -> semantic::Enum;
    fn trait_semantic(&self, item: TraitId) -> semantic::Trait;
    fn impl_semantic(&self, item: ImplId) -> semantic::Impl;
}

fn module_semantic(_db: &dyn SemanticGroup, _item: ModuleId) -> semantic::Module {
    todo!()
}
fn function_semantic(_db: &dyn SemanticGroup, _item: FunctionId) -> semantic::Function {
    todo!()
}
fn struct_semantic(_db: &dyn SemanticGroup, _item: StructId) -> semantic::Struct {
    todo!()
}
fn enum_semantic(_db: &dyn SemanticGroup, _item: EnumId) -> semantic::Enum {
    todo!()
}
fn trait_semantic(_db: &dyn SemanticGroup, _item: TraitId) -> semantic::Trait {
    todo!()
}
fn impl_semantic(_db: &dyn SemanticGroup, _item: ImplId) -> semantic::Impl {
    todo!()
}
