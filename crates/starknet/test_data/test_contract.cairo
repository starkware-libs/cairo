trait ITestContract { func test(ref system: System, arg1: felt, arg2: felt) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(
    ref system: System, arg1: felt, arg2: felt
) -> felt {
    my_storage_var::read(system)
}
}

#[contract(TestContractImpl)]
struct TestContract { my_storage_var: felt, }
