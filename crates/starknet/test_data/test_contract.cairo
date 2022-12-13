trait ITestContract { func test(ref syscall_ptr: SyscallPtr) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(ref syscall_ptr: SyscallPtr) -> felt {
    my_storage_var::read(syscall_ptr)
}
}

#[contract(TestContractImpl)]
struct TestContract { my_storage_var: felt, }
