fn fib_program() -> sierra::graph::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        # 0
        split_gas<9, 1>(cost) -> (cost, jump_cost);
        jump_nz<int>(n, jump_cost) { 2(n) fallthrough() };
        # 1
        split_gas<7, 1, 1>(cost) -> (cost, push_gb, push_one);
        refund_gas<7>(gb, cost) -> (gb);
        store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one, push_one) -> (one);
        return(gb, one);
        # 2
        split_gas<7, 1, 1>(cost) -> (cost, push_n, jump_cost);
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n);
        store<Temp, int>(n, push_n) -> (n);
        jump_nz<int>(n, jump_cost) { 4(n) fallthrough() };
        # 3
        split_gas<5, 1, 1>(cost) -> (cost, push_gb, push_one);
        refund_gas<5>(gb, cost) -> (gb);
        store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one, push_one) -> (one);
        return(gb, one);
        # 4
        split_gas<1, 1, 1, 1, 1, 2>(cost) -> (
            push_b, push_n, push_gb, push_a, get_gas_cost, final_cost
        );
        constant_num<int, 1>() -> (b);
        store<Temp, int>(b, push_b) -> (b);
        move<NonZero<int>>(n) -> (n);
        store<Temp, NonZero<int>>(n, push_n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
        constant_num<int, 1>() -> (a);
        store<Temp, int>(a, push_a) { fallthrough(a) };
        # 5
        get_gas<1, 1, 1, 1, 1>(gb, get_gas_cost) {
            7(gb, push_n, push_gb, push_a, jump_cost, get_gas_cost)
            fallthrough(gb)
        };
        # 6
        ignore_num<int>(a) -> ();
        ignore_num<int>(b) -> ();
        unwrap_nz<int>(n) -> (n);
        ignore_num<int>(n) -> ();
        split_gas<1, 1>(final_cost) -> (push_gb, push_err);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
        constant_num<int, -1>() -> (err);
        store<Temp, int>(err, push_err) -> (err);
        return(gb, err);
        # 7
        duplicate_num<int>(a) -> (a, prev_a);
        add<int>(a, b) -> (a);
        rename<int>(prev_a) -> (b);
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n);
        store<Temp, int>(n, push_n) -> (n);
        store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
        store<Temp, int>(a, push_a) -> (a);
        jump_nz<int>(n, jump_cost) { 5(n) fallthrough() };
        # 8
        ignore_num<int>(b) -> ();
        refund_gas<1>(gb, get_gas_cost) -> (gb);
        split_gas<1, 1>(final_cost) -> (push_gb, push_a);
        store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
        move<int>(a) -> (a);
        store<Temp, int>(a, push_a) -> (a);
        return(gb, a);

        Fibonacci@0(gb: GasBuiltin, n: int, cost: Gas<10>) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn soundness_test() {
    assert_eq!(sierra::soundness::validate(&fib_program()), Ok(()));
}

#[test]
fn vm_test() {
    let prog = fib_program();
    // 1, 1, 2, 3, 5, 8, 13, 21, 34
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 0]),
        Ok(vec![1007, 1])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 1]),
        Ok(vec![1005, 1])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 2]),
        Ok(vec![996, 2])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 3]),
        Ok(vec![991, 3])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 4]),
        Ok(vec![986, 5])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 5]),
        Ok(vec![981, 8])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 6]),
        Ok(vec![976, 13])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 7]),
        Ok(vec![971, 21])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 8]),
        Ok(vec![966, 34])
    );
}
