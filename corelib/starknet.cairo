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
type CallContractResult = Result::<Array::<felt>,
(
felt, Array::<felt>
)>; extern fn call_contract_syscall(
    address: ContractAddress, calldata: Array::<felt>
) -> CallContractResult implicits(GasBuiltin, System) nopanic;

// Events.
extern fn emit_event_syscall(
    keys: Array::<felt>, data: Array::<felt>
) -> Result::<(), felt> implicits(GasBuiltin, System) nopanic;
