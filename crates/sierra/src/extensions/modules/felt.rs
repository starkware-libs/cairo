use super::arithmetic::{ArithmeticTraits, ConstLibFunc, OperationLibFunc};
use super::pod::{DuplicateLibFunc, IgnoreLibFunc, PodTraits};
use crate::define_libfunc_hierarchy;
use crate::extensions::{ConcreteType, NamedType, NoGenericArgsGenericType};
use crate::ids::{GenericLibFuncId, GenericTypeId};

define_libfunc_hierarchy! {
    pub enum FeltLibFunc {
        Operation(FeltOperationLibFunc),
        Const(FeltConstLibFunc),
        Ignore(FeltIgnoreLibFunc),
        Duplicate(FeltDuplicateLibFunc),
    }, FeltConcrete
}

#[derive(Default)]
pub struct FeltTraits {}
impl PodTraits for FeltTraits {
    const IGNORE: GenericLibFuncId = GenericLibFuncId::new_inline("felt_ignore");
    const DUPLICATE: GenericLibFuncId = GenericLibFuncId::new_inline("felt_dup");
    const GENERIC_TYPE_ID: GenericTypeId = <FeltType as NamedType>::ID;
}
impl ArithmeticTraits for FeltTraits {
    const ADD: GenericLibFuncId = GenericLibFuncId::new_inline("felt_add");
    const SUB: GenericLibFuncId = GenericLibFuncId::new_inline("felt_sub");
    const MUL: GenericLibFuncId = GenericLibFuncId::new_inline("felt_mul");
    const DIV: GenericLibFuncId = GenericLibFuncId::new_inline("felt_div");
    const MOD: GenericLibFuncId = GenericLibFuncId::new_inline("felt_mod");
    const CONST: GenericLibFuncId = GenericLibFuncId::new_inline("felt_const");
    const GENERIC_TYPE_ID: GenericTypeId = <FeltType as NamedType>::ID;
}
pub type FeltIgnoreLibFunc = IgnoreLibFunc<FeltTraits>;
pub type FeltDuplicateLibFunc = DuplicateLibFunc<FeltTraits>;
pub type FeltOperationLibFunc = OperationLibFunc<FeltTraits>;
pub type FeltConstLibFunc = ConstLibFunc<FeltTraits>;

/// Type for felt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct FeltType {}
impl NoGenericArgsGenericType for FeltType {
    type Concrete = FeltConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("felt");
}
#[derive(Default)]
pub struct FeltConcreteType {}
impl ConcreteType for FeltConcreteType {}
