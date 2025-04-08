//! The `ContractAddress` type represents a Starknet contract address, with a value range of
//! `[0, 2**251)`.
//!
//!
//! A variable of type `ContractAddress` can be created from a `felt252` value using the
//! `contract_address_const` function, or using the `TryInto` trait.
//!
//! # Examples
//!
//! ```
//! use starknet::contract_address::contract_address_const;
//!
//! let contract_address = contract_address_const::<0x0>();
//! ```

use core::RangeCheck;
#[allow(unused_imports)]
use core::hash::{Hash, HashStateTrait};
use core::serde::Serde;
#[allow(unused_imports)]
use core::zeroable::Zeroable;

/// Represents a Starknet contract address.
/// The value range of this type is `[0, 2**251)`.
pub extern type ContractAddress;

impl ContractAddressCopy of Copy<ContractAddress>;
impl ContractAddressDrop of Drop<ContractAddress>;

/// Returns a `ContractAddress` given a `felt252` value.
///
/// # Examples
///
/// ```
/// use starknet::contract_address::contract_address_const;
///
/// let contract_address = contract_address_const::<0x0>();
/// ```
#[deprecated(
    feature: "deprecated-starknet-consts",
    note: "Use `TryInto::try_into` in const context instead.",
)]
pub extern fn contract_address_const<const address: felt252>() -> ContractAddress nopanic;

pub(crate) extern const fn contract_address_to_felt252(address: ContractAddress) -> felt252 nopanic;

pub(crate) extern const fn contract_address_try_from_felt252(
    address: felt252,
) -> Option<ContractAddress> implicits(RangeCheck) nopanic;

pub(crate) impl Felt252TryIntoContractAddress of TryInto<felt252, ContractAddress> {
    const fn try_into(self: felt252) -> Option<ContractAddress> {
        contract_address_try_from_felt252(self)
    }
}

pub(crate) impl ContractAddressIntoFelt252 of Into<ContractAddress, felt252> {
    const fn into(self: ContractAddress) -> felt252 {
        contract_address_to_felt252(self)
    }
}

impl ContractAddressZero of core::num::traits::Zero<ContractAddress> {
    #[feature("deprecated-starknet-consts")]
    fn zero() -> ContractAddress {
        contract_address_const::<0>()
    }

    #[inline]
    fn is_zero(self: @ContractAddress) -> bool {
        core::num::traits::Zero::<felt252>::is_zero(@contract_address_to_felt252(*self))
    }

    #[inline]
    fn is_non_zero(self: @ContractAddress) -> bool {
        !self.is_zero()
    }
}

pub(crate) impl ContractAddressZeroable =
    core::zeroable::zero_based::ZeroableImpl<ContractAddress, ContractAddressZero>;

impl ContractAddressSerde of Serde<ContractAddress> {
    fn serialize(self: @ContractAddress, ref output: Array<felt252>) {
        contract_address_to_felt252(*self).serialize(ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<ContractAddress> {
        Some(contract_address_try_from_felt252(Serde::<felt252>::deserialize(ref serialized)?)?)
    }
}

impl ContractAddressPartialEq of PartialEq<ContractAddress> {
    #[inline]
    fn eq(lhs: @ContractAddress, rhs: @ContractAddress) -> bool {
        contract_address_to_felt252(*lhs) == contract_address_to_felt252(*rhs)
    }
}

impl ContractAddressPartialOrd of PartialOrd<ContractAddress> {
    fn lt(lhs: ContractAddress, rhs: ContractAddress) -> bool {
        // TODO(orizi): Check if implementing a libfunc for `felt252` ordering is more efficient.
        let lhs: u256 = contract_address_to_felt252(lhs).into();
        lhs < contract_address_to_felt252(rhs).into()
    }
}

impl HashContractAddress<S, +HashStateTrait<S>, +Drop<S>> =
    core::hash::into_felt252_based::HashImpl<ContractAddress, S>;

impl DebugContractAddress = core::fmt::into_felt252_based::DebugImpl<ContractAddress>;
impl LowerHexContractAddress = core::fmt::into_felt252_based::LowerHexImpl<ContractAddress>;
