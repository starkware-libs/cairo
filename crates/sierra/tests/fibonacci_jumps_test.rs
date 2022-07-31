fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(
            r#"
        #  0
        int_jump_nz(n) { 6(n) fallthrough() };
        #  1
        refund_gas<7>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_const<1>() -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        #  6
        int_unwrap_nz(n) -> (n);
        int_add_const<-1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        int_jump_nz(n) { 15(n) fallthrough() };
        # 10
        refund_gas<5>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_const<1>() -> (one);
        store<Temp, int>(one) -> (one);
        return(gb, one);
        # 15
        int_const<1>() -> (b);
        store<Temp, int>(b) -> (b);
        move<NonZero<int>>(n) -> (n);
        store<Temp, NonZero<int>>(n) -> (n);
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_const<1>() -> (a);
        store<Temp, int>(a) -> (a);
        # 23
        get_gas<5>(gb) { 33(gb) fallthrough(gb) };
        # 24
        int_ignore(a) -> ();
        int_ignore(b) -> ();
        int_unwrap_nz(n) -> (n);
        int_ignore(n) -> ();
        move<GasBuiltin>(gb) -> (gb);
        store<Temp, GasBuiltin>(gb) -> (gb);
        int_const<-1>() -> (err);
        store<Temp, int>(err) -> (err);
        return(gb, err);
        # 33
        int_dup(a) -> (a, prev_a);
        int_add(a, b) -> (a);
        rename<int>(prev_a) -> (b);
        int_unwrap_nz(n) -> (n);
        int_add_const<-1>(n) -> (n);
        store<Temp, int>(n) -> (n);
        store<Temp, GasBuiltin>(gb) -> (gb);
        store<Temp, int>(a) -> (a);
        int_jump_nz(n) { 23(n) fallthrough() };
        # 42
        int_ignore(b) -> ();
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

#[test]
fn simulation_test() {
    let prog = fib_program();
    let id = sierra::program::Identifier("Fibonacci".to_string());
    // 1, 1, 2, 3, 5, 8, 13, 21, 34
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![0]]),
        Ok(vec![vec![1007], vec![1]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![1]]),
        Ok(vec![vec![1005], vec![1]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![2]]),
        Ok(vec![vec![996], vec![2]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![3]]),
        Ok(vec![vec![991], vec![3]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![4]]),
        Ok(vec![vec![986], vec![5]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![5]]),
        Ok(vec![vec![981], vec![8]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![6]]),
        Ok(vec![vec![976], vec![13]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![7]]),
        Ok(vec![vec![971], vec![21]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![1000], vec![8]]),
        Ok(vec![vec![966], vec![34]])
    );
    assert_eq!(
        sierra::simulation::run(&prog, &id, vec![vec![100], vec![80]]),
        Ok(vec![vec![0], vec![-1]])
    );
}
