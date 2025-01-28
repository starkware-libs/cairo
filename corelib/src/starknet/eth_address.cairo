//! Ethereum address type for working with Ethereum primitives.
//!
//! This module provides the [`EthAddress`] type, which is used when interacting with Ethereum
//! primitives, such as signatures and L1 <-> L2 messages.

use core::debug::PrintTrait;
#[allow(unused_imports)]
use core::integer::{U128TryIntoNonZero, U256TryIntoFelt252, u128_safe_divmod};
use core::option::{Option, OptionTrait};
use core::serde::Serde;
use core::traits::{Into, TryInto};

/// An Ethereum address, 20 bytes in length.
#[derive(Copy, Drop, Hash, PartialEq)]
pub struct EthAddress {
    address: felt252,
}

impl EthAddressStorePacking of starknet::StorePacking<EthAddress, felt252> {
    fn pack(value: EthAddress) -> felt252 {
        value.address
    }

    fn unpack(value: felt252) -> EthAddress {
        EthAddress { address: value }
    }
}

pub(crate) impl Felt252TryIntoEthAddress of TryInto<felt252, EthAddress> {
    fn try_into(self: felt252) -> Option<EthAddress> {
        let ETH_ADDRESS_BOUND = 0x10000000000000000000000000000000000000000_u256; // 2 ** 160

        if self.into() < ETH_ADDRESS_BOUND {
            Some(EthAddress { address: self })
        } else {
            None
        }
    }
}

pub(crate) impl EthAddressIntoFelt252 of Into<EthAddress, felt252> {
    fn into(self: EthAddress) -> felt252 {
        self.address
    }
}

/// Creates an `EthAddress` from the 20 least significant bytes of a `u256`.
pub(crate) impl U256IntoEthAddress of Into<u256, EthAddress> {
    fn into(self: u256) -> EthAddress {
        // The Ethereum address is the 20 least significant bytes (=160=128+32 bits) of the value.
        let high_32_bits = self.high % 0x100000000_u128;
        EthAddress {
            address: high_32_bits.into() * 0x100000000000000000000000000000000_felt252
                + self.low.into(),
        }
    }
}

pub(crate) impl EthAddressSerde of Serde<EthAddress> {
    fn serialize(self: @EthAddress, ref output: Array<felt252>) {
        self.address.serialize(ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<EthAddress> {
        Serde::<felt252>::deserialize(ref serialized)?.try_into()
    }
}

impl EthAddressZero of core::num::traits::Zero<EthAddress> {
    fn zero() -> EthAddress {
        0.try_into().unwrap()
    }

    #[inline]
    fn is_zero(self: @EthAddress) -> bool {
        core::num::traits::Zero::<felt252>::is_zero(self.address)
    }

    #[inline]
    fn is_non_zero(self: @EthAddress) -> bool {
        !self.is_zero()
    }
}

pub(crate) impl EthAddressZeroable =
    core::zeroable::zero_based::ZeroableImpl<EthAddress, EthAddressZero>;

pub(crate) impl EthAddressPrintImpl of PrintTrait<EthAddress> {
    fn print(self: EthAddress) {
        self.address.print();
    }
}

impl DebugEthAddress = core::fmt::into_felt252_based::DebugImpl<EthAddress>;
impl LowerHexEthAddress = core::fmt::into_felt252_based::LowerHexImpl<EthAddress>;
