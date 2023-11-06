use core::debug::PrintTrait;
use core::integer::{u128_safe_divmod, U128TryIntoNonZero, U256TryIntoFelt252};
use core::option::{Option, OptionTrait};
use core::serde::Serde;
use core::traits::{Into, TryInto};

// An Ethereum address (160 bits).
#[derive(Copy, Drop, Hash, PartialEq, starknet::Store)]
struct EthAddress {
    address: felt252,
}
impl Felt252TryIntoEthAddress of TryInto<felt252, EthAddress> {
    fn try_into(self: felt252) -> Option<EthAddress> {
        let ETH_ADDRESS_BOUND = 0x10000000000000000000000000000000000000000_u256; // 2 ** 160

        if self.into() < ETH_ADDRESS_BOUND {
            Option::Some(EthAddress { address: self })
        } else {
            Option::None
        }
    }
}
impl EthAddressIntoFelt252 of Into<EthAddress, felt252> {
    fn into(self: EthAddress) -> felt252 {
        self.address
    }
}
impl U256IntoEthAddress of Into<u256, EthAddress> {
    fn into(self: u256) -> EthAddress {
        // The Ethereum address is the 20 least significant bytes (=160=128+32 bits) of the value.
        let high_32_bits = self.high % 0x100000000_u128;
        EthAddress {
            address: high_32_bits.into() * 0x100000000000000000000000000000000_felt252
                + self.low.into()
        }
    }
}
impl EthAddressSerde of Serde<EthAddress> {
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
    #[inline(always)]
    fn is_zero(self: @EthAddress) -> bool {
        core::felt_252::Felt252Zero::is_zero(self.address)
    }
    #[inline(always)]
    fn is_non_zero(self: @EthAddress) -> bool {
        !self.is_zero()
    }
}

impl EthAddressZeroable = core::zeroable::zero_based::ZeroableImpl<EthAddress, EthAddressZero>;

impl EthAddressPrintImpl of PrintTrait<EthAddress> {
    fn print(self: EthAddress) {
        self.address.print();
    }
}
