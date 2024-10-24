use starknet::{ClassHash, ContractAddress, StorageBaseAddress};

const U251_MAX: felt252 = 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;
type u251 = core::internal::BoundedInt<0, U251_MAX>;
const STORAGE_BASE_MAX: felt252 = U251_MAX - 256;

extern fn class_hash_const<const address: u251>() -> ClassHash nopanic;
extern fn contract_address_const<const address: u251>() -> ContractAddress nopanic;

type StorageBaseRange = core::internal::BoundedInt<0, STORAGE_BASE_MAX>;
extern fn storage_base_address_const<
    const address: StorageBaseRange
>() -> StorageBaseAddress nopanic;

trait AddressTrait<Address> {
    type Range;
    fn from_const<const VALUE: Self::Range>() -> Address;
}

impl ClassHashAddressTrait of AddressTrait<ClassHash> {
    type Range = u251;
    fn from_const<const VALUE: u251>() -> ClassHash {
        class_hash_const::<VALUE>()
    }
}

impl ContractAddressAddressTrait of AddressTrait<ContractAddress> {
    type Range = u251;
    fn from_const<const VALUE: u251>() -> ContractAddress {
        contract_address_const::<VALUE>()
    }
}

impl StorageBaseAddressAddressTrait of AddressTrait<StorageBaseAddress> {
    type Range = StorageBaseRange;
    fn from_const<const VALUE: StorageBaseRange>() -> StorageBaseAddress {
        storage_base_address_const::<VALUE>()
    }
}

pub fn address<
    const VALUE: AddressImpl::Range, Address, impl AddressImpl: AddressTrait<Address>
>() -> Address {
    AddressImpl::from_const::<VALUE>()
}
