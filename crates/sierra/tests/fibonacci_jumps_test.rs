use indoc::indoc;

fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
        type int = int;
        type GasBuiltin = GasBuiltin;
        type NonZero_int = NonZero<int>;

        ext move_int = move<int>;
        ext move_nz_int = move<NonZero_int>;
        ext move_gb = move<GasBuiltin>;
        ext store_temp_int = store_temp<int>;
        ext store_temp_nz_int = store_temp<NonZero_int>;
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

        //  0
        int_jump_nz(n) { 6(n) fallthrough() };
        //  1
        refund_gas_7(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_1() -> (one);
        store_temp_int(one) -> (one);
        return(gb, one);
        //  6
        int_unwrap_nz(n) -> (n);
        int_sub_1(n) -> (n);
        store_temp_int(n) -> (n);
        int_jump_nz(n) { 15(n) fallthrough() };
        // 10
        refund_gas_5(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_1() -> (one);
        store_temp_int(one) -> (one);
        return(gb, one);
        // 15
        int_const_1() -> (b);
        store_temp_int(b) -> (b);
        move_nz_int(n) -> (n);
        store_temp_nz_int(n) -> (n);
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_1() -> (a);
        store_temp_int(a) -> (a);
        // 23
        get_gas_5(gb) { 33(gb) fallthrough(gb) };
        // 24
        int_ignore(a) -> ();
        int_ignore(b) -> ();
        int_nz_unwrap(n) -> (n);
        int_ignore(n) -> ();
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_const_minus_1() -> (err);
        store_temp_int(err) -> (err);
        return(gb, err);
        // 33
        int_dup(a) -> (a, prev_a);
        int_add(a, b) -> (a);
        rename_int(prev_a) -> (b);
        int_unwrap_nz(n) -> (n);
        int_sub_1(n) -> (n);
        store_temp_int(n) -> (n);
        store_temp_gb(gb) -> (gb);
        store_temp_int(a) -> (a);
        int_jump_nz(n) { 23(n) fallthrough() };
        // 42
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
