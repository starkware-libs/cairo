trait ITestContract { func test() -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test() -> felt {
    1
}
}

#[contract(TestContractImpl)]
struct TestContract { }
