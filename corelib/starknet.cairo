extern type System;
extern type StorageAddress;

extern fn storage_address_const<address>() -> StorageAddress nopanic;
extern fn storage_read_syscall(ref system: System, address: StorageAddress) -> felt nopanic;
extern fn storage_write_syscall(
    ref system: System, address: StorageAddress, value: felt
) -> Result::<(), felt> implicits(GasBuiltin) nopanic;
