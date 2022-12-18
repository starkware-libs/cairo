trait ITestContract { func test(ref system: System, arg1: felt, arg2: felt) -> felt; }

#[ContractImpl]
impl TestContractImpl of ITestContract { func test(
    ref system: System, arg1: felt, arg2: felt
) -> felt {
    let x = my_storage_var::read(system);
    my_storage_var::write(system, x + 1);
    x
}
}

#[contract(TestContractImpl)]
struct TestContract { my_storage_var: felt, }
