extern type System;
extern type StorageAddress;
extern type ContractAddress;

extern fn storage_address_const<address>() -> StorageAddress nopanic;
extern fn storage_read_syscall(address: StorageAddress) -> felt implicits(System) nopanic;
extern fn storage_write_syscall(
    address: StorageAddress, value: felt
) -> Result::<(), felt> implicits(GasBuiltin, System) nopanic;


extern fn call_contract_syscall(
    address: ContractAddress, calldata: Array::<felt>
) -> Result::<Array::<felt>, (felt, Array::<felt>)> implicits(GasBuiltin, System) nopanic;
