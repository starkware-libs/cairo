extern type System;
extern type StorageAddress;

extern func storage_address_const<address>() -> StorageAddress nopanic;
extern func storage_read_syscall(ref system: System, address: StorageAddress) -> felt nopanic;
extern func storage_write_syscall(
    ref system: System, address: StorageAddress, value: felt
) -> Result::<(), felt> implicits(GasBuiltin) nopanic;
