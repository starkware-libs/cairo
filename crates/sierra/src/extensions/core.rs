use super::ap_tracking::RevokeApTrackingLibFunc;
use super::modules::felt::{FeltLibFunc, FeltType};
use super::modules::function_call::FunctionCallLibFunc;
use super::modules::gas::{GasBuiltinType, GasLibFunc};
use super::modules::integer::{IntegerLibFunc, IntegerType};
use super::modules::mem::MemLibFunc;
use super::modules::non_zero::{NonZeroType, UnwrapNonZeroLibFunc};
use super::modules::unconditional_jump::UnconditionalJumpLibFunc;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum CoreType {
        Felt(FeltType),
        GasBuiltin(GasBuiltinType),
        Integer(IntegerType),
        NonZero(NonZeroType),
    }, CoreTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        ApTracking(RevokeApTrackingLibFunc),
        Felt(FeltLibFunc),
        FunctionCall(FunctionCallLibFunc),
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnwrapNonZero(UnwrapNonZeroLibFunc),
        UnconditionalJump(UnconditionalJumpLibFunc),
    }, CoreConcreteLibFunc
}
