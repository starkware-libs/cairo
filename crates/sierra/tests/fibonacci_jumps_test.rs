use indoc::indoc;
use sierra::program_registry::ProgramRegistry;

fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
        type int = int;
        type GasBuiltin = GasBuiltin;
        type int_non_zero = NonZero<int>;

        ext move_int = move<int>;
        ext move_nz_int = move<int_non_zero>;
        ext move_gb = move<GasBuiltin>;
        ext store_temp_int = store_temp<int>;
        ext store_temp_nz_int = store_temp<int_non_zero>;
        ext store_temp_gb = store_temp<GasBuiltin>;
        ext rename_int = rename<int>;
        ext int_const_1 = int_const<1>;
        ext int_const_minus_1 = int_const<-1>;
        ext int_add = int_add;
        ext int_sub_1 = int_sub<1>;
        ext int_dup = int_dup;
        ext int_ignore = int_ignore;
        ext int_jump_nz = int_jump_nz;
        ext int_unwrap_nz = int_unwrap_nz;
        ext get_gas_5 = get_gas<5>;
        ext refund_gas_1 = refund_gas<1>;
        ext refund_gas_5 = refund_gas<5>;
        ext refund_gas_7 = refund_gas<7>;

        // Statement #  0 - tests if n == 0.
        int_jump_nz(n) { 6(n) fallthrough() };
        // Statement #  1 - n == 0, so we return updated gb and 1.
        refund_gas_7(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_1() -> (one);
        store_temp_int(one) -> (one);
        return(gb, one);
        // Statement #  6 - Calculates n - 1 and tests if n - 1 == 0.
        int_unwrap_nz(n) -> (n);
        int_sub_1(n) -> (n);
        store_temp_int(n) -> (n);
        int_jump_nz(n) { 15(n) fallthrough() };
        // Statement # 10  - n == 1, so we return updated gb and 1.
        refund_gas_5(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_1() -> (one);
        store_temp_int(one) -> (one);
        return(gb, one);
        // Statement # 15
        // Setting up the latest memory to be of the form [b=1, n=n-1, gb, a=1].
        int_const_1() -> (b);
        store_temp_int(b) -> (b);
        move_nz_int(n) -> (n);
        store_temp_nz_int(n) -> (n);
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_1() -> (a);
        store_temp_int(a) -> (a);
        // Statement # 23 - Getting gas for the main loop.
        get_gas_5(gb) { 33(gb) fallthrough(gb) };
        // Statement # 24  - Ran out of gas - returning updated gb and -1.
        int_ignore(a) -> ();
        int_ignore(b) -> ();
        int_unwrap_nz(n) -> (n);
        int_ignore(n) -> ();
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_minus_1() -> (err);
        store_temp_int(err) -> (err);
        return(gb, err);
        // Statement # 33
        // The main loop - given [b, n, gb, a] - adds [n-1, updated_gb, a+b]
        // Memory cells form is now [b'=a, n'=n-1, gb'=updated_gb, a'=a+b]
        int_dup(a) -> (a, prev_a);
        int_add(a, b) -> (a);
        rename_int(prev_a) -> (b);
        int_unwrap_nz(n) -> (n);
        int_sub_1(n) -> (n);
        store_temp_int(n) -> (n);
        store_temp_gb(gb) -> (gb);
        store_temp_int(a) -> (a);
        int_jump_nz(n) { 23(n) fallthrough() };
        // Statement # 42 - n == 0, so we can return the latest a.
        int_ignore(b) -> ();
        refund_gas_1(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        move_int(a) -> (a);
        store_temp_int(a) -> (a);
        return(gb, a);

        Fibonacci@0(gb: GasBuiltin, n: int) -> (GasBuiltin, int);
        "})
        .unwrap()
}

#[test]
fn parse_test() {
    fib_program();
}

#[test]
fn create_registry_test() {
    ProgramRegistry::new(&fib_program()).unwrap();
}

#[test]
fn simulate_test() {
    let program = fib_program();
    let id = "Fibonacci".into();
    // 1, 1, 2, 3, 5, 8, 13, 21, 34
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![0.into()]]),
        Ok(vec![vec![1007.into()], vec![1.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![1.into()]]),
        Ok(vec![vec![1005.into()], vec![1.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![2.into()]]),
        Ok(vec![vec![996.into()], vec![2.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![3.into()]]),
        Ok(vec![vec![991.into()], vec![3.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![4.into()]]),
        Ok(vec![vec![986.into()], vec![5.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![5.into()]]),
        Ok(vec![vec![981.into()], vec![8.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![6.into()]]),
        Ok(vec![vec![976.into()], vec![13.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![7.into()]]),
        Ok(vec![vec![971.into()], vec![21.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![1000.into()], vec![8.into()]]),
        Ok(vec![vec![966.into()], vec![34.into()]])
    );
    assert_eq!(
        sierra::simulate::simulate(&program, &id, vec![vec![100.into()], vec![80.into()]]),
        Ok(vec![vec![0.into()], vec![(-1).into()]])
    );
}
