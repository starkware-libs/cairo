use indoc::indoc;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program_registry::ProgramRegistry;
use sierra::simulation;
use test_case::test_case;

fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
            type int = int;
            type GasBuiltin = GasBuiltin;
            type NonZeroInt = NonZero<int>;

            libfunc store_temp_int = store_temp<int>;
            libfunc store_local_int = store_local<int>;
            libfunc store_temp_gb = store_temp<GasBuiltin>;
            libfunc int_const_1 = int_const<1>;
            libfunc int_const_minus_10000 = int_const<-10000>;
            libfunc int_add = int_add;
            libfunc int_sub_1 = int_sub<1>;
            libfunc int_dup = int_dup;
            libfunc int_ignore = int_ignore;
            libfunc int_jump_nz = int_jump_nz;
            libfunc int_unwrap_nz = unwrap_nz<int>;
            libfunc get_gas_26 = get_gas<26>;
            libfunc refund_gas_1 = refund_gas<1>;
            libfunc refund_gas_3 = refund_gas<3>;
            libfunc alloc_locals = alloc_locals;
            libfunc call_fib = function_call<user@Fibonacci>;

            // Statement #  0 - tests if n == 0 and initiates 1 for the early return values.
            alloc_locals() -> ();
            int_const_1() -> (one);
            store_temp_int(one) -> (one);
            int_jump_nz(n) { 8(n) fallthrough() };
            // Statement #  4 - n == 0, so we return updated gb and 1.
            refund_gas_3(gb) -> (gb);
            store_temp_gb(gb) -> (gb);
            store_temp_int(one) -> (one);
            return(gb, one);
            // Statement #  8 - calculating n - 1, and testing if n - 1 == 0.
            int_unwrap_nz(n) -> (n);
            int_sub_1(n) -> (n_1);
            store_temp_int(n_1) -> (n_1);
            int_jump_nz(n_1) { 16(n_1) fallthrough() };
            // Statement # 13 - n == 1, so we return updated gb and 1.
            refund_gas_1(gb) -> (gb);
            store_temp_gb(gb) -> (gb);
            store_temp_int(one) -> (one);
            return(gb, one);
            // Statement # 16 - Get gas for the recursive calls.
            int_unwrap_nz(n_1) -> (n_1);
            int_ignore(one) -> ();
            get_gas_26(gb) { 24(gb) fallthrough(gb) };
            // Statement # 19 - Ran out of gas - returning update gb and error value.
            store_temp_gb(gb) -> (gb);
            int_ignore(n_1) -> ();
            int_const_minus_10000() -> (minus);
            store_temp_int(minus) -> (minus);
            return(gb, minus);
            // Statement # 24 - Performing both recursive calculations and returning their sum.
            store_temp_gb(gb) -> (gb);
            int_dup(n_1) -> (n_1, n_2);
            int_sub_1(n_2) -> (n_2);
            store_local_int(n_2,) -> (n_2);
            store_temp_int(n_1) -> (n_1);
            call_fib(gb, n_1) -> (gb, r1);
            store_local_int(r1) -> (r1);
            store_temp_gb(gb) -> (gb);
            store_temp_int(n_2) -> (n_2);
            call_fib(gb, n_2) -> (gb, r2);
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
fn create_registry_test() {
    ProgramRegistry::<CoreType, CoreLibFunc>::new(&fib_program()).unwrap();
}

#[test_case((1000, 0), (1003, 1); "fib(0) => 1")]
#[test_case((1000, 1), (1001, 1); "fib(1) => 1")]
#[test_case((1000, 2), (978, 2);  "fib(2) => 2")]
#[test_case((1000, 3), (953, 3);  "fib(3) => 3")]
#[test_case((1000, 4), (905, 5);  "fib(4) => 5")]
#[test_case((1000, 5), (832, 8);  "fib(5) => 8")]
#[test_case((1000, 6), (711, 13);  "fib(6) => 13")]
#[test_case((1000, 7), (517, 21);  "fib(7) => 21")]
#[test_case((1000, 8), (202, 34);  "fib(8) => 34")]
#[test_case((100, 80), (22, -40000); "Out of gas.")]
fn simulate((gb, n): (i64, i64), (new_gb, fib): (i64, i64)) {
    assert_eq!(
        simulation::run(&fib_program(), &"Fibonacci".into(), vec![vec![gb.into()], vec![n.into()]]),
        Ok(vec![vec![new_gb.into()], vec![fib.into()]])
    );
}
