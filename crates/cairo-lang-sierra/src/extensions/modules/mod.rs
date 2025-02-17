use self::enm::EnumType;
use self::int::unsigned128::Uint128Type;
use self::structure::StructType;
use super::lib_func::SignatureSpecializationContext;
use super::{NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;

pub mod ap_tracking;
pub mod array;
pub mod bitwise;
pub mod blake;
pub mod boolean;
pub mod bounded_int;
pub mod boxing;
pub mod branch_align;
pub mod bytes31;
pub mod casts;
pub mod circuit;
pub mod const_type;
pub mod consts;
pub mod coupon;
pub mod debug;
pub mod drop;
pub mod duplicate;
pub mod ec;
pub mod enm;
pub mod felt252;
pub mod felt252_dict;
pub mod function_call;
pub mod gas;
pub mod int;
pub mod is_zero;
pub mod mem;
pub mod non_zero;
pub mod nullable;
pub mod pedersen;
pub mod poseidon;
pub mod range;
pub mod range_check;
pub mod segment_arena;
pub mod snapshot;
pub mod span;
pub mod squashed_felt252_dict;
pub mod starknet;
pub mod structure;
pub mod try_from_felt252;
pub mod unconditional_jump;
pub mod uninitialized;
pub mod utils;

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

/// Helper for u256 type def.
fn get_u256_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::integer::u256")),
            GenericArg::Type(u128_ty.clone()),
            GenericArg::Type(u128_ty),
        ],
    )
}
