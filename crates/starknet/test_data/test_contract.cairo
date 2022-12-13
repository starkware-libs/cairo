trait ITestContract { func test(ref system: System) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(ref system: System) -> felt {
    my_storage_var::read(system)
}
}

#[contract(TestContractImpl)]
struct TestContract { my_storage_var: felt, }
