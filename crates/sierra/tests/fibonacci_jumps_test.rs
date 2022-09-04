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
            libfunc store_temp_nz_int = store_temp<NonZeroInt>;
            libfunc store_temp_gb = store_temp<GasBuiltin>;
            libfunc rename_int = rename<int>;
            libfunc int_const_1 = int_const<1>;
            libfunc int_const_minus_1 = int_const<-1>;
            libfunc int_add = int_add;
            libfunc int_sub_1 = int_sub<1>;
            libfunc int_dup = int_dup;
            libfunc int_ignore = int_ignore;
            libfunc int_jump_nz = int_jump_nz;
            libfunc int_unwrap_nz = unwrap_nz<int>;
            libfunc get_gas_5 = get_gas<5>;
            libfunc refund_gas_1 = refund_gas<1>;
            libfunc refund_gas_5 = refund_gas<5>;
            libfunc refund_gas_7 = refund_gas<7>;

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
            store_temp_nz_int(n) -> (n);
            store_temp_gb(gb) -> (gb);
            int_const_1() -> (a);
            store_temp_int(a) -> (a);
            // Statement # 21 - Getting gas for the main loop.
            get_gas_5(gb) { 30(gb) fallthrough(gb) };
            // Statement # 22  - Ran out of gas - returning updated gb and -1.
            int_ignore(a) -> ();
            int_ignore(b) -> ();
            int_unwrap_nz(n) -> (n);
            int_ignore(n) -> ();
            store_temp_gb(gb) -> (gb);
            int_const_minus_1() -> (err);
            store_temp_int(err) -> (err);
            return(gb, err);
            // Statement # 30
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
            int_jump_nz(n) { 21(n) fallthrough() };
            // Statement # 39 - n == 0, so we can return the latest a.
            int_ignore(b) -> ();
            refund_gas_1(gb) -> (gb);
            store_temp_gb(gb) -> (gb);
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
    ProgramRegistry::<CoreType, CoreLibFunc>::new(&fib_program()).unwrap();
}

#[test_case((1000, 0), (1007, 1); "fib(0) => 1")]
#[test_case((1000, 1), (1005, 1); "fib(1) => 1")]
#[test_case((1000, 2), (996, 2);  "fib(2) => 2")]
#[test_case((1000, 3), (991, 3);  "fib(3) => 3")]
#[test_case((1000, 4), (986, 5);  "fib(4) => 5")]
#[test_case((1000, 5), (981, 8);  "fib(5) => 8")]
#[test_case((1000, 6), (976, 13);  "fib(6) => 13")]
#[test_case((1000, 7), (971, 21);  "fib(7) => 21")]
#[test_case((1000, 8), (966, 34);  "fib(8) => 34")]
#[test_case((100, 80), (0, -1); "Out of gas.")]
fn simulate((gb, n): (i64, i64), (new_gb, fib): (i64, i64)) {
    assert_eq!(
        simulation::run(&fib_program(), &"Fibonacci".into(), vec![vec![gb.into()], vec![n.into()]]),
        Ok(vec![vec![new_gb.into()], vec![fib.into()]])
    );
}
