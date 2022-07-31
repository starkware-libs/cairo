fn collatz_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        #  0
        move<int>(n) -> (n);
        store<Temp, int>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 0>() -> (counter);
        store<Temp, int>(counter) -> (counter);
        jump() { 38() };
        #  7
        unwrap_nz<int>(to_drop) -> (to_drop);
        ignore_num<int>(to_drop) -> ();
        get_gas<11>(gb) { 17(gb) fallthrough(gb) };
        # 10
        ignore_num<int>(n) -> ();
        ignore_num<int>(counter) -> ();
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, -1>() -> (err);
        store<Temp, int>(err) -> (err);
        return(gb, err);
        # 17
        duplicate_num<int>(n) -> (n, parity);
        mod<int, 2>(parity) -> (parity);
        store<Temp, int>(parity) -> (parity);
        store<Temp, GasBuiltin>(gb) -> (gb);
        jump_nz<int>(parity) { 28(to_drop) fallthrough() };
        # 22
        align_temps<1>() -> ();
        div<int, 2>(n) -> (n);
        store<Temp, int>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        jump() { 36() };
        # 28
        unwrap_nz<int>(to_drop) -> (to_drop);
        ignore_num<int>(to_drop) -> ();
        mul<int, 3>(n) -> (n);
        store<Temp, int>(n) -> (n);
        add<int, 1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        # 36
        add<int, 1>(counter) -> (counter);
        store<Temp, int>(counter) -> (counter);
        # 38
        duplicate_num<int>(n) -> (n, n_1);
        add<int, -1>(n_1) -> (n_1);
        store<Temp, int>(n_1) -> (n_1);
        jump_nz<int>(n_1) { 7(to_drop) fallthrough() };
        # 42
        ignore_num<int>(n) -> ();
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
