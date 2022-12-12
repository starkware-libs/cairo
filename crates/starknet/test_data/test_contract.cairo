trait ITestContract { func test(ref system: System) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(ref system: System) -> felt {
    get_my_storage_var(system)
}
}

#[contract(TestContractImpl)]
struct TestContract { my_storage_var: felt, }
