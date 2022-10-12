use super::arithmetic::{ArithmeticTraits, ConstLibFunc, OperationLibFunc};
use super::jump_not_zero::{JumpNotZeroLibFunc, JumpNotZeroTraits};
use crate::define_libfunc_hierarchy;
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{NamedType, NoGenericArgsGenericType};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Type for int.
#[derive(Default)]
pub struct IntegerType {}
impl NoGenericArgsGenericType for IntegerType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("int");

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
        Operation(IntOperationLibFunc),
        Const(IntConstLibFunc),
        JumpNotZero(IntJumpNotZeroLibFunc),
    }, IntegerConcrete
}

#[derive(Default)]
pub struct IntegerTraits {}
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
pub type IntOperationLibFunc = OperationLibFunc<IntegerTraits>;
pub type IntConstLibFunc = ConstLibFunc<IntegerTraits>;
pub type IntJumpNotZeroLibFunc = JumpNotZeroLibFunc<IntegerTraits>;
