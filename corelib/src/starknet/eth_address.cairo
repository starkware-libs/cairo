use serde::Serde;
use traits::{Into, TryInto};
use zeroable::Zeroable;
use option::{Option, OptionTrait};

// An Ethereum address (160 bits).
#[derive(Copy, Drop)]
struct EthAddress {
    address: felt252, 
}
impl Felt252TryIntoEthAddress of TryInto<felt252, EthAddress> {
    fn try_into(self: felt252) -> Option<EthAddress> {
        // TODO(yuval): change to a constant once u256 literals are supported.
        let ETH_ADDRESS_BOUND = u256 { high: 0x100000000_u128, low: 0_u128 }; // 2 ** 160

        if self.into() < ETH_ADDRESS_BOUND {
            Option::Some(EthAddress { address: self })
        } else {
            Option::None(())
        }
    }
}
impl EthAddressIntoFelt252 of Into<EthAddress, felt252> {
    fn into(self: EthAddress) -> felt252 {
        self.address
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
impl EthAddressZeroable of Zeroable<EthAddress> {
    fn zero() -> EthAddress {
        0.try_into().unwrap()
    }
    #[inline(always)]
    fn is_zero(self: EthAddress) -> bool {
        self.address.is_zero()
    }
    #[inline(always)]
    fn is_non_zero(self: EthAddress) -> bool {
        !self.is_zero()
    }
}
impl ContractAddressPartialEq of PartialEq<EthAddress> {
    #[inline(always)]
    fn eq(lhs: EthAddress, rhs: EthAddress) -> bool {
        lhs.address == rhs.address
    }
    #[inline(always)]
    fn ne(lhs: EthAddress, rhs: EthAddress) -> bool {
        !(lhs == rhs)
    }
}
