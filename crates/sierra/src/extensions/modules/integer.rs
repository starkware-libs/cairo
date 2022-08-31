use super::arithmetic::{ArithmeticTraits, ConstLibFunc, OperationLibFunc};
use super::jump_not_zero::{JumpNotZeroLibFunc, JumpNotZeroTraits};
use super::pod::{DuplicateLibFunc, IgnoreLibFunc, PodTraits};
use crate::define_libfunc_hierarchy;
use crate::extensions::{ConcreteType, NamedType, NoGenericArgsGenericType};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Type for int.
#[derive(Default)]
pub struct IntegerType {}
impl NoGenericArgsGenericType for IntegerType {
    type Concrete = IntegerConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("int");
}
#[derive(Default)]
pub struct IntegerConcreteType {}
impl ConcreteType for IntegerConcreteType {}

define_libfunc_hierarchy! {
    pub enum IntegerLibFunc {
        Operation(IntOperationLibFunc),
        Const(IntConstLibFunc),
        Ignore(IntIgnoreLibFunc),
        Duplicate(IntDuplicateLibFunc),
        JumpNotZero(IntJumpNotZeroLibFunc),
    }, IntegerConcrete
}

#[derive(Default)]
pub struct IntegerTraits {}
impl PodTraits for IntegerTraits {
    const IGNORE: GenericLibFuncId = GenericLibFuncId::new_inline("int_ignore");
    const DUPLICATE: GenericLibFuncId = GenericLibFuncId::new_inline("int_dup");
    const GENERIC_TYPE_ID: GenericTypeId = <IntegerType as NamedType>::ID;
}
impl ArithmeticTraits for IntegerTraits {
    const ADD: GenericLibFuncId = GenericLibFuncId::new_inline("int_add");
    const SUB: GenericLibFuncId = GenericLibFuncId::new_inline("int_sub");
    const MUL: GenericLibFuncId = GenericLibFuncId::new_inline("int_mul");
    const DIV: GenericLibFuncId = GenericLibFuncId::new_inline("int_div");
    const MOD: GenericLibFuncId = GenericLibFuncId::new_inline("int_mod");
    const CONST: GenericLibFuncId = GenericLibFuncId::new_inline("int_const");
    const GENERIC_TYPE_ID: GenericTypeId = <IntegerType as NamedType>::ID;
}
impl JumpNotZeroTraits for IntegerTraits {
    const JUMP_NOT_ZERO: GenericLibFuncId = GenericLibFuncId::new_inline("int_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <IntegerType as NamedType>::ID;
}
pub type IntIgnoreLibFunc = IgnoreLibFunc<IntegerTraits>;
pub type IntDuplicateLibFunc = DuplicateLibFunc<IntegerTraits>;
pub type IntOperationLibFunc = OperationLibFunc<IntegerTraits>;
pub type IntConstLibFunc = ConstLibFunc<IntegerTraits>;
pub type IntJumpNotZeroLibFunc = JumpNotZeroLibFunc<IntegerTraits>;
