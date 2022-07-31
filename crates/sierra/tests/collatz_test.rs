fn collatz_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        #  0
        move<int>(n) -> (n);
        store<Temp, int>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_const<0>() -> (counter);
        store<Temp, int>(counter) -> (counter);
        jump() { 38() };
        #  7
        move<int>(to_drop) -> (to_drop);
        int_ignore<int>(to_drop) -> ();
        get_gas<11>(gb) { 17(gb) fallthrough(gb) };
        # 10
        int_ignore<int>(n) -> ();
        int_ignore<int>(counter) -> ();
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_const<-1>() -> (err);
        store<Temp, int>(err) -> (err);
        return(gb, err);
        # 17
        int_dup(n) -> (n, parity);
        int_mod_const<2>(parity) -> (parity);
        store<Temp, int>(parity) -> (parity);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_jump_nz(parity) { 28(to_drop) fallthrough() };
        # 22
        jump() { 23() }; # align_temps<1>() -> ();
        int_div_const<2>(n) -> (n);
        store<Temp, int>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        jump() { 36() };
        # 28
        int_unwrap_nz(to_drop) -> (to_drop);
        int_ignore<int>(to_drop) -> ();
        int_mul_const<3>(n) -> (n);
        store<Temp, int>(n) -> (n);
        int_add_const<1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        # 36
        int_add_const<1>(counter) -> (counter);
        store<Temp, int>(counter) -> (counter);
        # 38
        int_dup(n) -> (n, n_1);
        int_add_const<-1>(n_1) -> (n_1);
        store<Temp, int>(n_1) -> (n_1);
        int_jump_nz(n_1) { 7(to_drop) fallthrough() };
        # 42
        int_ignore<int>(n) -> ();
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(counter) -> (counter);
        store<Temp, int>(counter) -> (counter);
        return(gb, counter);

        Collatz@0(gb: GasBuiltin, n: int) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn parse_test() {
    collatz_program();
}

#[test]
fn simulation_test() {
    let prog = collatz_program();
    let id = sierra::program::Identifier("Collatz".to_string());
    // 5 -> 16 -> 8 -> 4 -> 2 -> 1
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![100], vec![5]]),
        Ok(vec![vec![47], vec![5]])
    );
    //  0     1     2     3     4     5     6     7     8     9
    //  7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 ->
    // 10 ->  5 -> 16 ->  8 ->  4 ->  2 ->  1 */
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![200], vec![7]]),
        Ok(vec![vec![30], vec![16]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![100], vec![7]]), // Out of gas.
        Ok(vec![vec![5], vec![-1]])
    );
}
