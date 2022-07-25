use crate::ids::*;

// Salsa database interface.
// See ids.rs for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup {
    #[salsa::interned]
    fn intern_function(&self, id: FreeFunctionLongId) -> FreeFunctionId;
    #[salsa::interned]
    fn intern_struct(&self, id: StructLongId) -> StructId;
    #[salsa::interned]
    fn intern_member(&self, id: MemberLongId) -> MemberId;
    #[salsa::interned]
    fn intern_extern_type(&self, id: ExternTypeLongId) -> ExternTypeId;
    #[salsa::interned]
    fn intern_extern_function(&self, id: ExternFunctionLongId) -> ExternFunctionId;
    #[salsa::interned]
    fn intern_function_arg(&self, id: ParamLongId) -> ParamId;
    #[salsa::interned]
    fn intern_block(&self, id: BlockLongId) -> BlockId;
    #[salsa::interned]
    fn intern_let_var(&self, id: LocalVarLongId) -> LocalVarId;
}
