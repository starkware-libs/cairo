use semantic::db::SemanticGroup;
use sierra::ids::{ConcreteLibFuncId, ConcreteTypeId};
use sierra::program::{ConcreteLibFuncLongId, ConcreteTypeLongId};

#[salsa::query_group(SierraGenDatabase)]
pub trait SierraGenGroup: SemanticGroup {
    #[salsa::interned]
    fn intern_concrete_lib_func(&self, id: ConcreteLibFuncLongId) -> ConcreteLibFuncId;
    #[salsa::interned]
    fn intern_concrete_type(&self, id: ConcreteTypeLongId) -> ConcreteTypeId;
}
