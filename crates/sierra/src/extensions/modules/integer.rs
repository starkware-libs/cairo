use super::jump_not_zero::{JumpNotZeroLibFunc, JumpNotZeroTraits};
use super::wrapping_arithmetic::{ConstLibFunc, OperationLibFunc, WrappingArithmeticTraits};
use crate::define_libfunc_hierarchy;
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{NamedType, NoGenericArgsGenericType};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Type for uint128.
#[derive(Default)]
pub struct IntegerType {}
impl NoGenericArgsGenericType for IntegerType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("uint128");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: true,
                duplicatable: true,
            },
        }
    }
}

define_libfunc_hierarchy! {
    pub enum IntegerLibFunc {
        WrappingOp(IntWrappingOpLibFunc),
        Const(IntConstLibFunc),
        JumpNotZero(IntJumpNotZeroLibFunc),
    }, IntegerConcrete
}

#[derive(Default)]
pub struct IntegerTraits {}
impl WrappingArithmeticTraits for IntegerTraits {
    const ADD: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_wrapping_add");
    const SUB: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_wrapping_sub");
    const MUL: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_wrapping_mul");
    const DIV: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_div");
    const MOD: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_mod");
    const CONST: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_const");
    const GENERIC_TYPE_ID: GenericTypeId = <IntegerType as NamedType>::ID;
}
impl JumpNotZeroTraits for IntegerTraits {
    const JUMP_NOT_ZERO: GenericLibFuncId = GenericLibFuncId::new_inline("uint128_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <IntegerType as NamedType>::ID;
}
pub type IntWrappingOpLibFunc = OperationLibFunc<IntegerTraits>;
pub type IntConstLibFunc = ConstLibFunc<IntegerTraits>;
pub type IntJumpNotZeroLibFunc = JumpNotZeroLibFunc<IntegerTraits>;
