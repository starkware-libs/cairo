use indoc::indoc;
use sierra::program_registry::ProgramRegistry;

fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
        type int = int;
        type GasBuiltin = GasBuiltin;
        type NonZero_int = NonZero<int>;

        ext move_int = move<int>;
        ext move_gb = move<GasBuiltin>;
        ext store_temp_int = store_temp<int>;
        ext store_local_int = store_local<int>;
        ext store_temp_gb = store_temp<GasBuiltin>;
        ext int_const_1 = int_const<1>;
        ext int_const_minus_10000 = int_const<-10000>;
        ext int_add = int_add;
        ext int_sub_1 = int_sub<1>;
        ext int_dup = int_dup;
        ext int_ignore = int_ignore;
        ext int_jump_nz = int_jump_nz;
        ext int_unwrap_nz = int_unwrap_nz;
        ext get_gas_26 = get_gas<26>;
        ext refund_gas_1 = refund_gas<1>;
        ext refund_gas_3 = refund_gas<3>;
        ext alloc_locals = alloc_locals;
        ext call_fib = Call<&Fibonacci>;

        // Statement #  0 - tests if n == 0 and initiates 1 for the early return values.
        alloc_locals() -> ();
        int_const_1() -> (one);
        store_temp_int(one) -> (one);
        int_jump_nz(n) { 9(n) fallthrough() };
        // Statement #  4 - n == 0, so we return updated gb and 1.
        refund_gas_3(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        move_int(one) -> (one);
        store_temp_int(one) -> (one);
        return(gb, one);
        // Statement #  9 - calculating n - 1, and testing if n - 1 == 0.
        int_unwrap_nz(n) -> (n);
        int_sub_1(n) -> (n_1);
        store_temp_int(n_1) -> (n_1);
        int_jump_nz(n_1) { 18(n_1) fallthrough() };
        // Statement # 13 - n == 1, so we return updated gb and 1.
        refund_gas_1(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        move_int(one) -> (one);
        store_temp_int(one) -> (one);
        return(gb, one);
        // Statement # 18 - Get gas for the recursive calls.
        int_unwrap_nz(n_1) -> (n_1);
        int_ignore(one) -> ();
        get_gas_26(gb) { 27(gb) fallthrough(gb) };
        // Statement # 21 - Ran out of gas - returning update gb and error value.
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_ignore(n_1) -> ();
        int_const_minus_10000() -> (minus);
        store_temp_int(minus) -> (minus);
        return(gb, minus);
        // Statement # 27 - Performing both recursive calculations and returning their sum.
        store_temp_gb(gb) -> (gb);
        int_dup(n_1) -> (n_1, n_2);
        int_sub_1(n_2) -> (n_2);
        store_local_int(n_2,) -> (n_2);
        move_int(n_1) -> (n_1);
        store_temp_int(n_1) -> (n_1);
        call_fib(gb, n_1) -> (gb, r1);
        move_int(r1) -> (r1);
        store_local_int(r1) -> (r1);
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        move_int(n_2) -> (n_2);
        store_temp_int(n_2) -> (n_2);
        call_fib(gb, n_2) -> (gb, r2);
        move_gb(gb) -> (gb);
        store_temp_gb(gb) -> (gb);
        int_add(r1, r2) -> (r);
        store_temp_int(r) -> (r);
        return(gb, r);

        Fibonacci@0(gb: GasBuiltin, n: int) -> (GasBuiltin, int);
        "})
        .unwrap()
}

#[test]
fn parse_test() {
    fib_program();
}

#[test]
#[ignore] // TODO(oziv) - enable again when function call extension is added.
fn create_registry_test() {
    ProgramRegistry::new(&fib_program()).unwrap();
}
