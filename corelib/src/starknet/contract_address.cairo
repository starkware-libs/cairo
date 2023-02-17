use zeroable::Zeroable;

#[derive(Copy, Drop)]
extern type ContractAddress;


extern fn contract_address_const<const address>() -> ContractAddress nopanic;
extern fn contract_address_to_felt(address: ContractAddress) -> felt nopanic;

extern fn contract_address_try_from_felt(
    address: felt
) -> Option::<ContractAddress> implicits(RangeCheck) nopanic;

impl FeltTryIntoContractAddress of TryInto::<felt, ContractAddress> {
    fn try_into(self: felt) -> Option::<ContractAddress> {
        contract_address_try_from_felt(self)
    }
}
impl ContractAddressIntoFelt of Into::<ContractAddress, felt> {
    fn into(self: ContractAddress) -> felt {
        contract_address_to_felt(self)
    }
}

impl ContractAddressZeroable of Zeroable::<ContractAddress> {
    fn zero() -> ContractAddress {
        contract_address_const::<0>()
    }

    #[inline(always)]
    fn is_zero(self: ContractAddress) -> bool {
        contract_address_to_felt(self).is_zero()
    }
}
