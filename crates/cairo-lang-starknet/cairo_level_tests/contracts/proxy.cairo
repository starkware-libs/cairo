#[starknet::contract]
mod proxy_contract {
    use starknet::storage::StoragePointerReadAccess;
    use starknet::syscalls::library_call_syscall;
    use starknet::{ClassHash, SyscallResultTrait};

    #[storage]
    struct Storage {
        forward: ClassHash,
    }

    #[raw_input]
    #[raw_output]
    #[external(v0)]
    fn foo(ref self: ContractState, data: Span<felt252>) -> Span<felt252> {
        library_call_syscall(self.forward.read(), selector!("foo"), data).unwrap_syscall()
    }

    #[raw_input]
    #[raw_output]
    #[external(v0)]
    fn bar(ref self: ContractState, data: Span<felt252>) -> Span<felt252> {
        library_call_syscall(self.forward.read(), selector!("bar"), data).unwrap_syscall()
    }
}
