trait ITestContract { func test(ref syscall_ptr: SyscallPtr) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(ref syscall_ptr: SyscallPtr) -> felt {
    get_my_storage_var(syscall_ptr)
}
}

#[contract(TestContractImpl)]
struct TestContract { my_storage_var: felt, }
