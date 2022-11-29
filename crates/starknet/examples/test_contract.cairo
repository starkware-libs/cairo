trait ITestContract { func test() -> felt; }

impl TestContractImpl of ITestContract { func test() -> felt {
    return 1;
}
}

#[contract(TestContractImpl)]
struct TestContract { }
