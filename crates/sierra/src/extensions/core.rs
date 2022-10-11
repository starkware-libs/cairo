use super::ap_tracking::RevokeApTrackingLibFunc;
use super::array::{ArrayLibFunc, ArrayType};
use super::drop::DropLibFunc;
use super::duplicate::DupLibFunc;
use super::enm::{EnumLibFunc, EnumType};
use super::modules::boxing::{BoxLibFunc, BoxType};
use super::modules::felt::{FeltLibFunc, FeltType};
use super::modules::function_call::FunctionCallLibFunc;
use super::modules::gas::{GasBuiltinType, GasLibFunc};
use super::modules::integer::{IntegerLibFunc, IntegerType};
use super::modules::mem::MemLibFunc;
use super::modules::non_zero::{NonZeroType, UnwrapNonZeroLibFunc};
use super::modules::unconditional_jump::UnconditionalJumpLibFunc;
use super::uninitialized::UninitializedType;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum CoreType {
        Array(ArrayType),
        Box(BoxType),
        Felt(FeltType),
        GasBuiltin(GasBuiltinType),
        Integer(IntegerType),
        NonZero(NonZeroType),
        Uninitialized(UninitializedType),
        Enum(EnumType),
    }, CoreTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        ApTracking(RevokeApTrackingLibFunc),
        Array(ArrayLibFunc),
        Box(BoxLibFunc),
        Drop(DropLibFunc),
        Dup(DupLibFunc),
        Felt(FeltLibFunc),
        FunctionCall(FunctionCallLibFunc),
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnwrapNonZero(UnwrapNonZeroLibFunc),
        UnconditionalJump(UnconditionalJumpLibFunc),
        Enum(EnumLibFunc),
    }, CoreConcreteLibFunc
}
