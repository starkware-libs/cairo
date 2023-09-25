use zeroable::Zeroable;
use serde::Serde;
use hash::{Hash, HashStateTrait};

#[derive(Copy, Drop)]
extern type ContractAddress;


extern fn contract_address_const<const address: felt252>() -> ContractAddress nopanic;
extern fn contract_address_to_felt252(address: ContractAddress) -> felt252 nopanic;

extern fn contract_address_try_from_felt252(
    address: felt252
) -> Option<ContractAddress> implicits(RangeCheck) nopanic;

impl Felt252TryIntoContractAddress of TryInto<felt252, ContractAddress> {
    fn try_into(self: felt252) -> Option<ContractAddress> {
        contract_address_try_from_felt252(self)
    }
}
impl ContractAddressIntoFelt252 of Into<ContractAddress, felt252> {
    fn into(self: ContractAddress) -> felt252 {
        contract_address_to_felt252(self)
    }
}

impl ContractAddressZeroable of Zeroable<ContractAddress> {
    fn zero() -> ContractAddress {
        contract_address_const::<0>()
    }
    #[inline(always)]
    fn is_zero(self: ContractAddress) -> bool {
        contract_address_to_felt252(self).is_zero()
    }
    #[inline(always)]
    fn is_non_zero(self: ContractAddress) -> bool {
        !self.is_zero()
    }
}

impl ContractAddressSerde of serde::Serde<ContractAddress> {
    fn serialize(self: @ContractAddress, ref output: Array<felt252>) {
        contract_address_to_felt252(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<ContractAddress> {
        Option::Some(
            contract_address_try_from_felt252(
                serde::Serde::<felt252>::deserialize(ref serialized)?
            )?
        )
    }
}

impl ContractAddressPartialEq of PartialEq<ContractAddress> {
    #[inline(always)]
    fn eq(lhs: @ContractAddress, rhs: @ContractAddress) -> bool {
        contract_address_to_felt252(*lhs) == contract_address_to_felt252(*rhs)
    }
    #[inline(always)]
    fn ne(lhs: @ContractAddress, rhs: @ContractAddress) -> bool {
        !(lhs == rhs)
    }
}

impl HashContractAddress<S, +HashStateTrait<S>, +Drop<S>> =
    core::hash::into_felt252_based::HashImpl<ContractAddress, S>;
