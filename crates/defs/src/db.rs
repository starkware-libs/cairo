use crate::ids::*;

// Salsa database interface.
// See ids.rs for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup {
    #[salsa::interned]
    fn intern_sub_module(&self, id: SubmoduleLongId) -> SubmoduleId;
    #[salsa::interned]
    fn intern_function(&self, id: FunctionLongId) -> FunctionId;
    #[salsa::interned]
    fn intern_struct(&self, id: StructLongId) -> StructId;
    #[salsa::interned]
    fn intern_member(&self, id: MemberLongId) -> MemberId;
    #[salsa::interned]
    fn intern_enum(&self, id: EnumLongId) -> EnumId;
    #[salsa::interned]
    fn intern_variant(&self, id: VariantLongId) -> VariantId;
    #[salsa::interned]
    fn intern_trait(&self, id: TraitLongId) -> TraitId;
    #[salsa::interned]
    fn intern_impl(&self, id: ImplLongId) -> ImplId;
    #[salsa::interned]
    fn intern_use(&self, id: UseLongId) -> UseId;
    #[salsa::interned]
    fn intern_extern_type(&self, id: ExternTypeLongId) -> ExternTypeId;
    #[salsa::interned]
    fn intern_extern_function(&self, id: ExternFunctionLongId) -> ExternFunctionId;
    #[salsa::interned]
    fn intern_trait_associated_type(&self, id: TraitAssociatedTypeLongId) -> TraitAssociatedTypeId;
    #[salsa::interned]
    fn intern_trait_function(&self, id: TraitFunctionLongId) -> TraitFunctionId;
    #[salsa::interned]
    fn intern_impl_associated_type(&self, id: ImplAssociatedTypeLongId) -> ImplAssociatedTypeId;
    #[salsa::interned]
    fn intern_impl_function(&self, id: ImplFunctionLongId) -> ImplFunctionId;
    #[salsa::interned]
    fn intern_generic_type_arg(&self, id: GenericTypeArgLongId) -> GenericTypeArgId;
    #[salsa::interned]
    fn intern_generic_impl_arg(&self, id: GenericImplArgLongId) -> GenericImplArgId;
    #[salsa::interned]
    fn intern_function_arg(&self, id: FunctionArgLongId) -> FunctionArgId;
    #[salsa::interned]
    fn intern_block(&self, id: BlockLongId) -> BlockId;
    #[salsa::interned]
    fn intern_let_var(&self, id: LetVarLongId) -> LetVarId;
}
