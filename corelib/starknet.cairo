extern type System;
extern type StorageAddress;

extern func storage_address_const<address>() -> StorageAddress nopanic;
extern func storage_read_syscall(ref system: System, address: StorageAddress) -> felt nopanic;
