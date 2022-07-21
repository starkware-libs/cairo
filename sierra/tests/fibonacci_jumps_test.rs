fn fib_program() -> sierra::graph::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        # 0
        jump_nz<int>(n) { 2(n) fallthrough() };
        # 1
        refund_gas<7>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        # 2
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        jump_nz<int>(n) { 4(n) fallthrough() };
        # 3
        refund_gas<5>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        # 4
        constant_num<int, 1>() -> (b);
        store<Temp, int>(b) -> (b);
        move<NonZero<int>>(n) -> (n);
        store<Temp, NonZero<int>>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 1>() -> (a);
        store<Temp, int>(a) { fallthrough(a) };
        # 5
        get_gas<5>(gb) { 7(gb) fallthrough(gb) };
        # 6
        ignore_num<int>(a) -> ();
        ignore_num<int>(b) -> ();
        unwrap_nz<int>(n) -> (n);
        ignore_num<int>(n) -> ();
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, -1>() -> (err);
        store<Temp, int>(err) -> (err);
        return(gb, err);
        # 7
        duplicate_num<int>(a) -> (a, prev_a);
        add<int>(a, b) -> (a);
        rename<int>(prev_a) -> (b);
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        store<Temp, GasBuiltin>(gb) -> (gb);
        store<Temp, int>(a) -> (a);
        jump_nz<int>(n) { 5(n) fallthrough() };
        # 8
        ignore_num<int>(b) -> ();
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(a) -> (a);
        store<Temp, int>(a) -> (a);
        return(gb, a);

        Fibonacci@0[ap += unknown, gas -= 12](gb: GasBuiltin, n: int) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn soundness_test() {
    assert_eq!(sierra::soundness::validate(&fib_program()), Ok(()));
}

#[test]
fn simulation_test() {
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
