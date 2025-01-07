//! The `ClassHash` type represents a Starknet contract class hash, with a value range of
//! `[0, 2**251)`.
//!
//!
//! A variable of type `ClassHash` can be created from a `felt252` value using the
//! `class_hash_const` function, or using the `TryInto` trait.
//!
//! # Examples
//!
//! ```
//! use core::starknet::class_hash::class_hash_const;
//!
//! let hash = class_hash_const::<0x123>();
//! let hash = 0x123.try_into().unwrap();
//! ```

use core::RangeCheck;
#[allow(unused_imports)]
use core::hash::{Hash, HashStateTrait};
use core::serde::Serde;

/// Represents a Starknet contract class hash.
/// The value range of this type is `[0, 2**251)`.
#[derive(Copy, Drop)]
pub extern type ClassHash;

/// Returns a `ClassHash` given a `felt252` value.
///
/// # Examples
///
/// ```
/// use core::starknet::class_hash::class_hash_const;
///
/// let class_hash = class_hash_const::<0x123>();
/// ```
pub extern fn class_hash_const<const address: felt252>() -> ClassHash nopanic;

pub(crate) extern fn class_hash_to_felt252(address: ClassHash) -> felt252 nopanic;

pub(crate) extern fn class_hash_try_from_felt252(
    address: felt252,
) -> Option<ClassHash> implicits(RangeCheck) nopanic;

pub(crate) impl Felt252TryIntoClassHash of TryInto<felt252, ClassHash> {
    fn try_into(self: felt252) -> Option<ClassHash> {
        class_hash_try_from_felt252(self)
    }
}

pub(crate) impl ClassHashIntoFelt252 of Into<ClassHash, felt252> {
    fn into(self: ClassHash) -> felt252 {
        class_hash_to_felt252(self)
    }
}

impl ClassHashZero of core::num::traits::Zero<ClassHash> {
    fn zero() -> ClassHash {
        class_hash_const::<0>()
    }

    #[inline]
    fn is_zero(self: @ClassHash) -> bool {
        core::num::traits::Zero::<felt252>::is_zero(@class_hash_to_felt252(*self))
    }

    #[inline]
    fn is_non_zero(self: @ClassHash) -> bool {
        !self.is_zero()
    }
}

pub(crate) impl ClassHashZeroable =
    core::zeroable::zero_based::ZeroableImpl<ClassHash, ClassHashZero>;

impl ClassHashSerde of Serde<ClassHash> {
    fn serialize(self: @ClassHash, ref output: Array<felt252>) {
        class_hash_to_felt252(*self).serialize(ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<ClassHash> {
        Option::Some(class_hash_try_from_felt252(Serde::<felt252>::deserialize(ref serialized)?)?)
    }
}

impl ClassHashPartialEq of PartialEq<ClassHash> {
    #[inline]
    fn eq(lhs: @ClassHash, rhs: @ClassHash) -> bool {
        class_hash_to_felt252(*lhs) == class_hash_to_felt252(*rhs)
    }
}

impl HashClassHash<S, +HashStateTrait<S>, +Drop<S>> =
    core::hash::into_felt252_based::HashImpl<ClassHash, S>;

impl DebugClassHash = core::fmt::into_felt252_based::DebugImpl<ClassHash>;
impl LowerHexClassHash = core::fmt::into_felt252_based::LowerHexImpl<ClassHash>;
