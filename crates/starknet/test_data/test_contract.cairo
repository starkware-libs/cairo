trait ITestContract { func test(ref system: System) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(ref system: System) -> felt {
    1
}
}

#[contract(TestContractImpl)]
struct TestContract { }
