use indoc::indoc;

fn collatz_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
        type int = int;
        type GasBuiltin = GasBuiltin;
        type NonZero_int = NonZero<int>;

        ext move_int = move<int>;
        ext move_gb = move<GasBuiltin>;
        ext store_temp_int = store_temp<int>;
        ext store_temp_gb = store_temp<GasBuiltin>;
        ext int_const_0 = int_const<0>;
        ext int_const_minus_1 = int_const<-1>;
        ext int_mod_2 = int_mod<2>;
        ext int_div_2 = int_div<2>;
        ext int_mul_3 = int_mul<3>;
        ext int_add_1 = int_add<1>;
        ext int_sub_1 = int_sub<1>;
        ext int_dup = int_dup;
        ext int_ignore = int_ignore;
        ext int_jump_nz = int_jump_nz;
        ext int_unwrap_nz = int_unwrap_nz;
        ext get_gas_11 = get_gas<11>;
        ext refund_gas_1 = refund_gas<1>;
        ext jump = jump;
        ext align_temps = align_temps<1>;

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
