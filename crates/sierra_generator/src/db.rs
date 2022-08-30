use semantic::db::SemanticGroup;
use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId, FunctionId};
use sierra::program::{ConcreteLibFuncLongId, ConcreteTypeLongId};

#[salsa::query_group(SierraGenDatabase)]
pub trait SierraGenGroup: SemanticGroup {
    #[salsa::interned]
    fn intern_concrete_lib_func(&self, id: ConcreteLibFuncLongId) -> ConcreteLibFuncId;
    #[salsa::interned]
    fn intern_concrete_type(&self, id: ConcreteTypeLongId) -> ConcreteTypeId;
    /// Creates a Sierra function id for a function id of the semantic model.
    #[salsa::interned]
    fn intern_function(&self, id: semantic::ConcreteFunctionId) -> FunctionId;
}
