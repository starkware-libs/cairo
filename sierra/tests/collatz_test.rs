fn collatz_program() -> sierra::graph::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        # 0
        move<int>(n) -> (n);
        store<Temp, int>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 0>() -> (counter);
        store<Temp, int>(counter) -> (counter);
        jump() { 7() };
        # 1
        unwrap_nz<int>(to_drop) -> (to_drop);
        ignore_num<int>(to_drop) -> ();
        get_gas<11>(gb) { 3(gb) fallthrough(gb) };
        # 2
        ignore_num<int>(n) -> ();
        ignore_num<int>(counter) -> ();
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, -1>() -> (err);
        store<Temp, int>(err) -> (err);
        return(gb, err);
        # 3
        duplicate_num<int>(n) -> (n, parity);
        mod<int, 2>(parity) -> (parity);
        store<Temp, int>(parity) -> (parity);
        store<Temp, GasBuiltin>(gb) -> (gb);
        jump_nz<int>(parity) { 5(to_drop) fallthrough() };
        # 4
        align_temps<1>() -> ();
        div<int, 2>(n) -> (n);
        store<Temp, int>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        jump() { 6() };
        # 5
        unwrap_nz<int>(to_drop) -> (to_drop);
        ignore_num<int>(to_drop) -> ();
        mul<int, 3>(n) -> (n);
        store<Temp, int>(n) -> (n);
        add<int, 1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) { fallthrough(gb) };
        # 6
        add<int, 1>(counter) -> (counter);
        store<Temp, int>(counter) { fallthrough(counter) };
        # 7
        duplicate_num<int>(n) -> (n, n_1);
        add<int, -1>(n_1) -> (n_1);
        store<Temp, int>(n_1) -> (n_1);
        jump_nz<int>(n_1) { 1(to_drop) fallthrough() };
        # 8
        ignore_num<int>(n) -> ();
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(counter) -> (counter);
        store<Temp, int>(counter) -> (counter);
        return(gb, counter);

        Collatz@0[ap += unknown, gas -= 11](gb: GasBuiltin, n: int) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn soundness_test() {
    assert_eq!(sierra::soundness::validate(&collatz_program()), Ok(()));
}

#[test]
fn simulation_test() {
    let prog = collatz_program();
    assert_eq!(
        sierra::simulation::run(&prog, "Collatz", vec![100, 5]), // 5 -> 16 -> 8 -> 4 -> 2 -> 1
        Ok(vec![47, 5])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Collatz", vec![200, 7]), // 7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
        Ok(vec![30, 16])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Collatz", vec![100, 7]), // OOG
        Ok(vec![5, -1])
    );
}
