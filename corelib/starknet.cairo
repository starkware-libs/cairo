extern type System;
extern type StorageAddress;
extern type ContractAddress;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {
}

// Storage.
extern fn storage_address_const<address>() -> StorageAddress nopanic;
extern fn storage_read_syscall(address: StorageAddress) -> felt implicits(System) nopanic;
extern fn storage_write_syscall(
    address: StorageAddress, value: felt
) -> Result::<(), felt> implicits(GasBuiltin, System) nopanic;

// Interoperability.
type CallContractResult = Result::<Array::<felt>,
(
felt, Array::<felt>
)>; extern fn call_contract_syscall(
    address: ContractAddress, calldata: Array::<felt>
) -> CallContractResult implicits(GasBuiltin, System) nopanic;
