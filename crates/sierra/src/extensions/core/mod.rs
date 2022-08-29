use self::felt::{FeltLibFunc, FeltType};
use self::function_call::FunctionCallLibFunc;
use self::gas::{GasBuiltinType, GasLibFunc};
use self::integer::{IntegerLibFunc, IntegerType};
use self::mem::{DeferredType, MemLibFunc};
use self::non_zero::{NonZeroType, UnwrapNonZeroLibFunc};
use self::unconditional_jump::UnconditionalJumpLibFunc;
use super::{GenericLibFunc, SpecializationError};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod felt;
pub mod function_call;
pub mod gas;
pub mod integer;
pub mod mem;
pub mod non_zero;
pub mod unconditional_jump;

define_type_hierarchy! {
    pub enum CoreType {
        Felt(FeltType),
        GasBuiltin(GasBuiltinType),
        Integer(IntegerType),
        NonZero(NonZeroType),
        Deferred(DeferredType),
    }, CoreTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        Felt(FeltLibFunc),
        FunctionCall(FunctionCallLibFunc),
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnwrapNonZero(UnwrapNonZeroLibFunc),
        UnconditionalJump(UnconditionalJumpLibFunc),
    }, CoreConcreteLibFunc
}

/// Helper for extracting the type from the template arguments.
fn as_single_type(args: &[GenericArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty.clone()),
        _ => Err(SpecializationError::UnsupportedGenericArg),
    }
}
