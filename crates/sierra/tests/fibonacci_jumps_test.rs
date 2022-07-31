fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        #  0
        jump_nz<int>(n) { 6(n) fallthrough() };
        #  1
        refund_gas<7>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        #  6
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        jump_nz<int>(n) { 15(n) fallthrough() };
        # 10
        refund_gas<5>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 1>() -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        # 15
        constant_num<int, 1>() -> (b);
        store<Temp, int>(b) -> (b);
        move<NonZero<int>>(n) -> (n);
        store<Temp, NonZero<int>>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, 1>() -> (a);
        store<Temp, int>(a) -> (a);
        # 23
        get_gas<5>(gb) { 33(gb) fallthrough(gb) };
        # 24
        ignore_num<int>(a) -> ();
        ignore_num<int>(b) -> ();
        unwrap_nz<int>(n) -> (n);
        ignore_num<int>(n) -> ();
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        constant_num<int, -1>() -> (err);
        store<Temp, int>(err) -> (err);
        return(gb, err);
        # 33
        duplicate_num<int>(a) -> (a, prev_a);
        add<int>(a, b) -> (a);
        rename<int>(prev_a) -> (b);
        unwrap_nz<int>(n) -> (n);
        add<int, -1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        store<Temp, GasBuiltin>(gb) -> (gb);
        store<Temp, int>(a) -> (a);
        jump_nz<int>(n) { 23(n) fallthrough() };
        # 42
        ignore_num<int>(b) -> ();
        refund_gas<1>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        move<int>(a) -> (a);
        store<Temp, int>(a) -> (a);
        return(gb, a);

        Fibonacci@0(gb: GasBuiltin, n: int) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn parse_test() {
    fib_program();
}
