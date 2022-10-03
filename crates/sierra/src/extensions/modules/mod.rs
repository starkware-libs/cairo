use super::SpecializationError;
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

pub mod ap_tracking;
pub mod arithmetic;
pub mod drop;
pub mod duplicate;
pub mod felt;
pub mod function_call;
pub mod gas;
pub mod integer;
pub mod jump_not_zero;
pub mod mem;
pub mod non_zero;
pub mod reference;
pub mod unconditional_jump;
pub mod uninitialized;

/// Helper for extracting the type from the template arguments.
fn as_single_type(args: &[GenericArg]) -> Result<ConcreteTypeId, SpecializationError> {
    match args {
        [GenericArg::Type(ty)] => Ok(ty.clone()),
        [_] => Err(SpecializationError::UnsupportedGenericArg),
        _ => Err(SpecializationError::WrongNumberOfGenericArgs),
    }
}
