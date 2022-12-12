extern type SyscallPtr;
extern type StorageAddress;

extern func storage_address_const<address>() -> StorageAddress nopanic;
extern func storage_read_syscall(
    ref syscall_ptr: SyscallPtr,
    address: StorageAddress
) -> felt nopanic;
