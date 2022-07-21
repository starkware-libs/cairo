fn fib_program() -> sierra::graph::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        # 0
        alloc_locals() -> ();
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one) -> (one);
        jump_nz<int>(n) { 2(n) fallthrough() };
        # 1
        refund_gas<3>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(one) -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        # 2
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n_1);
        store<Temp, int>(n_1) -> (n_1);
        jump_nz<int>(n_1) { 4(n_1) fallthrough() };
        # 3
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(one) -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        # 4
        unwrap_nz<int>(n_1) -> (n_1);
        ignore_num<int>(one) -> ();
        get_gas<26>(gb) { 6(gb) fallthrough(gb) };
        # 5
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        ignore_num<int>(n_1) -> ();
        constant_num<int, -10000>() -> (minus);
        store<Temp, int>(minus) -> (minus);
        return(gb, minus);
        # 6
        store<Temp, GasBuiltin>(gb) -> (gb);
        duplicate_num<int>(n_1) -> (n_1, n_2);
        add<int, -1>(n_2) -> (n_2);
        store<Local, int>(n_2,) -> (n_2);
        move<int>(n_1) -> (n_1);
        store<Temp, int>(n_1) -> (n_1);
        tuple_pack<GasBuiltin, int>(gb, n_1) -> (input);
        Fibonacci(input) -> (output);
        tuple_unpack<GasBuiltin, int>(output) -> (gb, r1);
        move<int>(r1) -> (r1);
        store<Local, int>(r1) -> (r1);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(n_2) -> (n_2);
        store<Temp, int>(n_2) -> (n_2);
        tuple_pack<GasBuiltin, int>(gb, n_2) -> (input);
        Fibonacci(input) -> (output);
        tuple_unpack<GasBuiltin, int>(output) -> (gb, r2);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        add<int>(r1, r2) -> (r);
        store<Temp, int>(r) -> (r);
        return(gb, r);

        Fibonacci@0[ap += unknown, gas -= 10](gb: GasBuiltin, n: int) -> (GasBuiltin, int);"#,
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
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 5]),
        Ok(vec![832, 8])
    );
}
