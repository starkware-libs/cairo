#[contract]
mod TestContract {
    use array::ArrayTrait;
    use starknet::get_caller_address;
    use starknet::storage_read_syscall;
    use starknet::storage_write_syscall;
    use starknet::syscalls::emit_event_syscall;
    use starknet::StorageAddress;
    use starknet::storage_access::storage_base_address_from_felt252;
    use starknet::storage_access::storage_address_from_base_and_offset;
    use starknet::ContractAddress;

    #[starknet::storage]
    struct Storage {
        my_storage_var: felt252
    }

    #[external]
    fn test(ref self: Storage, ref arg: felt252, arg1: felt252, arg2: felt252) -> felt252 {
        let x = self.my_storage_var.read();
        self.my_storage_var.write(x + 1);
        x + 1
    }

    #[external]
    fn test_storage_read(ref self: Storage, address: felt252) -> felt252 {
        let domain_address = 0_u32; // Only address_domain 0 is currently supported.
        let storage_address = storage_address_from_base_and_offset(
            storage_base_address_from_felt252(address), 0_u8
        );
        storage_read_syscall(domain_address, storage_address).unwrap_syscall()
    }

    #[external]
    fn test_storage_write(ref self: Storage, address: felt252, value: felt252) {
        let domain_address = 0_u32; // Only address_domain 0 is currently supported.
        let storage_address = storage_address_from_base_and_offset(
            storage_base_address_from_felt252(address), 0_u8
        );
        storage_write_syscall(domain_address, storage_address, value).unwrap_syscall();
    }

    #[external]
    fn test_get_caller_address(ref self: Storage) -> ContractAddress {
        let caller_address = get_caller_address();
        caller_address
    }

    #[external]
    fn test_emit_event(ref self: Storage, keys: Array<felt252>, data: Array<felt252>) {
        emit_event_syscall(keys.span(), data.span()).unwrap_syscall();
    }

    #[external]
    fn test_emit_simple_event(ref self: Storage, arg0: felt252) {}
}
