extern type System;
extern type StorageAddress;

extern fn storage_address_const<address>() -> StorageAddress nopanic;
extern fn storage_read_syscall(address: StorageAddress) -> felt implicits(System) nopanic;
extern fn storage_write_syscall(
    address: StorageAddress, value: felt
) -> Result::<(), felt> implicits(GasBuiltin, System) nopanic;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {
}
