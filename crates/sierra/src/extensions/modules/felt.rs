use super::arithmetic::{ArithmeticTraits, ConstLibFunc, OperationLibFunc};
use super::jump_not_zero::{JumpNotZeroLibFunc, JumpNotZeroTraits};
use crate::define_libfunc_hierarchy;
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{NamedType, NoGenericArgsGenericType};
use crate::ids::{GenericLibFuncId, GenericTypeId};

/// Type for felt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct FeltType {}
impl NoGenericArgsGenericType for FeltType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("felt");

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
    pub enum FeltLibFunc {
        Operation(FeltOperationLibFunc),
        Const(FeltConstLibFunc),
        JumpNotZero(FeltJumpNotZeroLibFunc),
    }, FeltConcrete
}

#[derive(Default)]
pub struct FeltTraits {}
impl ArithmeticTraits for FeltTraits {
    const ADD: GenericLibFuncId = GenericLibFuncId::new_inline("felt_add");
    const SUB: GenericLibFuncId = GenericLibFuncId::new_inline("felt_sub");
    const MUL: GenericLibFuncId = GenericLibFuncId::new_inline("felt_mul");
    const DIV: GenericLibFuncId = GenericLibFuncId::new_inline("felt_div");
    /// Modulo operation for felt would always return 0, as felt division is not interger division.
    const MOD: GenericLibFuncId = GenericLibFuncId::new_inline("felt_mod");
    const CONST: GenericLibFuncId = GenericLibFuncId::new_inline("felt_const");
    const GENERIC_TYPE_ID: GenericTypeId = <FeltType as NamedType>::ID;
}
impl JumpNotZeroTraits for FeltTraits {
    const JUMP_NOT_ZERO: GenericLibFuncId = GenericLibFuncId::new_inline("felt_jump_nz");
    const GENERIC_TYPE_ID: GenericTypeId = <FeltType as NamedType>::ID;
}
pub type FeltOperationLibFunc = OperationLibFunc<FeltTraits>;
pub type FeltConstLibFunc = ConstLibFunc<FeltTraits>;
pub type FeltJumpNotZeroLibFunc = JumpNotZeroLibFunc<FeltTraits>;
