extern type System;
#[derive(Copy, Drop)]
extern type StorageBaseAddress;
#[derive(Copy, Drop)]
extern type StorageAddress;
#[derive(Copy, Drop)]
extern type ContractAddress;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {
}

// Storage.
extern fn storage_base_address_const<address>() -> StorageBaseAddress nopanic;
extern fn storage_base_address_from_felt(
    addr: felt
) -> StorageBaseAddress implicits(RangeCheck) nopanic;
extern fn storage_address_from_base_and_offset(
    base: StorageBaseAddress, offset: u8
) -> StorageAddress nopanic;
extern fn storage_address_from_base(base: StorageBaseAddress) -> StorageAddress nopanic;

// Only address_domain 0 is currently supported.
// This parameter is going to be used to access address spaces with different
// data availability guarantees.
extern fn storage_read_syscall(
    address_domain: felt, address: StorageAddress, 
) -> Result::<felt, felt> implicits(GasBuiltin, System) nopanic;
extern fn storage_write_syscall(
    address_domain: felt, address: StorageAddress, value: felt
) -> Result::<(), felt> implicits(GasBuiltin, System) nopanic;

// Interoperability.
extern fn contract_address_const<address>() -> ContractAddress nopanic;
type CallContractResult = Result::<Array::<felt>, (felt, Array::<felt>)>;
extern fn call_contract_syscall(
    address: ContractAddress, calldata: Array::<felt>
) -> CallContractResult implicits(GasBuiltin, System) nopanic;

// Events.
extern fn emit_event_syscall(
    keys: Array::<felt>, data: Array::<felt>
) -> Result::<(), felt> implicits(GasBuiltin, System) nopanic;

// Getters.
extern fn get_caller_address() -> Result::<felt, felt> implicits(GasBuiltin, System) nopanic;

trait StorageAccess<T> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<T, felt>;
    fn write(address_domain: felt, base: StorageBaseAddress, value: T) -> Result::<(), felt>;
}

impl StorageAccessFelt of StorageAccess::<felt> {
    #[inline(always)]
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<felt, felt> {
        storage_read_syscall(address_domain, storage_address_from_base(base))
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: felt) -> Result::<(), felt> {
        storage_write_syscall(address_domain, storage_address_from_base(base), value)
    }
}

impl StorageAccessBool of StorageAccess::<bool> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<bool, felt> {
        Result::Ok(StorageAccess::<felt>::read(address_domain, base)? != 0)
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: bool) -> Result::<(), felt> {
            StorageAccess::<felt>::write(address_domain, base, if value {
                1
            } else {
                0
        })
    }
}

impl StorageAccessU8 of StorageAccess::<u8> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<u8, felt> {
        Result::Ok(u8_from_felt(StorageAccess::<felt>::read(address_domain, base)?))
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u8) -> Result::<(), felt> {
        StorageAccess::<felt>::write(address_domain, base, u8_to_felt(value))
    }
}

impl StorageAccessU128 of StorageAccess::<u128> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<u128, felt> {
        Result::Ok(u128_from_felt(StorageAccess::<felt>::read(address_domain, base)?))
    }
    #[inline(always)]
    fn write(address_domain: felt, base: StorageBaseAddress, value: u128) -> Result::<(), felt> {
        StorageAccess::<felt>::write(address_domain, base, u128_to_felt(value))
    }
}

impl StorageAccessU256 of StorageAccess::<u256> {
    fn read(address_domain: felt, base: StorageBaseAddress) -> Result::<u256, felt> {
        Result::Ok(
            u256 {
            low: StorageAccess::<u128>::read(address_domain, base)?,
            high: u128_from_felt(
                storage_read_syscall(
                    address_domain,
                    storage_address_from_base_and_offset(base, 1_u8)
                )?
            )
            }
        )
    }
    fn write(address_domain: felt, base: StorageBaseAddress, value: u256) -> Result::<(), felt> {
        StorageAccess::<u128>::write(address_domain, base, value.low)?;
        storage_write_syscall(
            address_domain,
            storage_address_from_base_and_offset(base, 1_u8),
            u128_to_felt(value.high)
        )
    }
}
