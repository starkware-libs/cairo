#[starknet::contract]
mod test_contract {
    use starknet::storage_access::{
        storage_address_from_base_and_offset, storage_base_address_from_felt252,
    };
    use starknet::syscalls::emit_event_syscall;
    use starknet::{
        ContractAddress, SyscallResultTrait, get_caller_address, storage_read_syscall,
        storage_write_syscall,
    };

    #[storage]
    struct Storage {
        my_storage_var: felt252,
    }

    #[external(v0)]
    fn test(ref self: ContractState, ref arg: felt252, arg1: felt252, arg2: felt252) -> felt252 {
        let x = self.my_storage_var.read();
        self.my_storage_var.write(x + 1);
        x + 1
    }

    #[external(v0)]
    fn test_storage_read(ref self: ContractState, address: felt252) -> felt252 {
        let domain_address = 0_u32; // Only address_domain 0 is currently supported.
        let storage_address = storage_address_from_base_and_offset(
            storage_base_address_from_felt252(address), 0_u8,
        );
        storage_read_syscall(domain_address, storage_address).unwrap_syscall()
    }

    #[external(v0)]
    fn test_storage_write(ref self: ContractState, address: felt252, value: felt252) {
        let domain_address = 0_u32; // Only address_domain 0 is currently supported.
        let storage_address = storage_address_from_base_and_offset(
            storage_base_address_from_felt252(address), 0_u8,
        );
        storage_write_syscall(domain_address, storage_address, value).unwrap_syscall();
    }

    #[external(v0)]
    fn test_get_caller_address(ref self: ContractState) -> ContractAddress {
        let caller_address = get_caller_address();
        caller_address
    }

    #[external(v0)]
    fn test_emit_event(ref self: ContractState, keys: Array<felt252>, data: Array<felt252>) {
        emit_event_syscall(keys.span(), data.span()).unwrap_syscall();
    }

    #[external(v0)]
    fn test_emit_simple_event(ref self: ContractState, arg0: felt252) {}
}
