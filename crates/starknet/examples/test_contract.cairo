trait ITestContract { func test(ref syscall_ptr: SyscallPtr) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(ref syscall_ptr: SyscallPtr) -> felt {
    1
}
}

#[contract(TestContractImpl)]
struct TestContract { }
