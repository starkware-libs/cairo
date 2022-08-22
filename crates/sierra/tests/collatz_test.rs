use indoc::indoc;
use sierra::program_registry::ProgramRegistry;
use sierra::simulation;

fn collatz_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
        type int = int;
        type GasBuiltin = GasBuiltin;
        type NonZero_int = NonZero<int>;

        libfunc move_int = move<int>;
        libfunc move_gb = move<GasBuiltin>;
        libfunc store_temp_int = store_temp<int>;
        libfunc store_temp_gb = store_temp<GasBuiltin>;
        libfunc int_const_0 = int_const<0>;
        libfunc int_const_minus_1 = int_const<-1>;
        libfunc int_mod_2 = int_mod<2>;
        libfunc int_div_2 = int_div<2>;
        libfunc int_mul_3 = int_mul<3>;
        libfunc int_add_1 = int_add<1>;
        libfunc int_sub_1 = int_sub<1>;
        libfunc int_dup = int_dup;
        libfunc int_ignore = int_ignore;
        libfunc int_jump_nz = int_jump_nz;
        libfunc int_unwrap_nz = int_unwrap_nz;
        libfunc get_gas_11 = get_gas<11>;
        libfunc refund_gas_1 = refund_gas<1>;
        libfunc jump = jump;
        libfunc align_temps = align_temps<int>;

        // Statement #  0 - Setting up memory the form [n, gb, counter=0].
        move_int(n) -> (n);
        store_temp_int(n) -> (n);
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_0() -> (counter);
        store_temp_int(counter) -> (counter);
        jump() { 38() };
        // Statement #  7 - Getting gas for main loop.
        // Unwrapping and ignoring jump_nz result, as we don't use it.
        int_unwrap_nz(to_drop) -> (to_drop);
        int_ignore(to_drop) -> ();
        get_gas_11(gb) { 17(gb) fallthrough(gb) };
        // Statement # 10 - Ran out of gas - returning updated gb and -1.
        int_ignore(n) -> ();
        int_ignore(counter) -> ();
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_minus_1() -> (err);
        store_temp_int(err) -> (err);
        return(gb, err);
        // Statement # 17 - Testing if n is odd or even.
        int_dup(n) -> (n, parity);
        int_mod_2(parity) -> (parity);
        store_temp_int(parity) -> (parity);
        store_temp_gb(gb) -> (gb);
        int_jump_nz(parity) { 28(to_drop) fallthrough() };
        // Statement # 22 - Handling even case. Adding [_, n/2, gb] to memory.
        align_temps() -> ();
        int_div_2(n) -> (n);
        store_temp_int(n) -> (n);
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        jump() { 36() };
        // Statement # 28 - Handling odd case. Adding [n*3, n*3+1, gb] to memory.
        int_unwrap_nz(to_drop) -> (to_drop);
        int_ignore(to_drop) -> ();
        int_mul_3(n) -> (n);
        store_temp_int(n) -> (n);
        int_add_1(n) -> (n);
        store_temp_int(n) -> (n);
        refund_gas_1(gb) -> (gb); // Aligning gas usage.
        store_temp_gb(gb) -> (gb);
        // Statement # 36 - Adding [counter + 1]. Memory now looks like [n', gb', counter'].
        int_add_1(counter) -> (counter);
        store_temp_int(counter) -> (counter);
        // Statement # 38 - Testing if n == 1 - to check if we need to stop running.
        int_dup(n) -> (n, n_1);
        int_sub_1(n_1) -> (n_1);
        store_temp_int(n_1) -> (n_1);
        int_jump_nz(n_1) { 7(to_drop) fallthrough() };
        // Statement # 42 - n == 1 - we are done - returning the counter result.
        int_ignore(n) -> ();
        refund_gas_1(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        move_int(counter) -> (counter);
        store_temp_int(counter) -> (counter);
        return(gb, counter);

        Collatz@0(gb: GasBuiltin, n: int) -> (GasBuiltin, int);
        "})
        .unwrap()
}

#[test]
fn parse_test() {
    collatz_program();
}

#[test]
fn create_registry_test() {
    ProgramRegistry::new(&collatz_program()).unwrap();
}

#[test]
fn simulate_test() {
    let program = collatz_program();
    let id = "Collatz".into();
    // 5 -> 16 -> 8 -> 4 -> 2 -> 1
    assert_eq!(
        simulation::run(&program, &id, vec![vec![/* gb= */ 100.into()], vec![/* n= */ 5.into()]]),
        Ok(vec![vec![/* gb= */ 47.into()], vec![/* index= */ 5.into()]])
    );
    //  0     1     2     3     4     5     6     7     8     9
    //  7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 ->
    // 10 ->  5 -> 16 ->  8 ->  4 ->  2 ->  1
    assert_eq!(
        simulation::run(&program, &id, vec![vec![/* gb= */ 200.into()], vec![/* n= */ 7.into()]]),
        Ok(vec![vec![/* gb= */ 30.into()], vec![/* index= */ 16.into()]])
    );
    // Out of gas.
    assert_eq!(
        simulation::run(&program, &id, vec![vec![/* gb= */ 100.into()], vec![/* n= */ 7.into()]]),
        Ok(vec![
            vec![/* gb= */ 5.into()],
            vec![
                (
                    // index=
                    -1
                )
                .into()
            ]
        ])
    );
}
