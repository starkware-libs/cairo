use self::enm::EnumType;
use self::strct::StructType;
use super::lib_func::SignatureSpecializationContext;
use super::{NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;

pub mod ap_tracking;
pub mod array;
pub mod bitwise;
pub mod boolean;
pub mod boxing;
pub mod branch_align;
pub mod builtin_cost;
pub mod consts;
pub mod debug;
pub mod dict_felt_to;
pub mod dict_manager;
pub mod drop;
pub mod duplicate;
pub mod ec;
pub mod enm;
pub mod felt;
pub mod function_call;
pub mod gas;
pub mod is_zero;
pub mod mem;
pub mod non_zero;
pub mod nullable;
pub mod pedersen;
pub mod range_check;
pub mod squashed_dict_felt_to;
pub mod starknet;
pub mod strct;
pub mod uint;
pub mod uint128;
pub mod unconditional_jump;
pub mod uninitialized;

/// Helper for Unit type def.
fn get_unit_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_concrete_type(
        StructType::id(),
        &[GenericArg::UserType(UserTypeId::from_string("Tuple"))],
    )
}

/// Helper for Bool type def.
fn get_bool_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let unit_type = get_unit_type(context)?;
    context.get_concrete_type(
        EnumType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::bool")),
            GenericArg::Type(unit_type.clone()),
            GenericArg::Type(unit_type),
        ],
    )
}
