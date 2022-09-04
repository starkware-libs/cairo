use indoc::indoc;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program_registry::ProgramRegistry;
use sierra::simulation;
use test_case::test_case;

fn fib_program() -> sierra::program::Program {
    sierra::ProgramParser::new()
        .parse(indoc! {"
            type felt = felt;
            type NonZeroFelt = NonZero<felt>;

            libfunc store_temp_felt = store_temp<felt>;
            libfunc felt_const_minus_1 = felt_const<-1>;
            libfunc felt_add = felt_add;
            libfunc felt_dup = felt_dup;
            libfunc felt_ignore = felt_ignore;
            libfunc felt_jump_nz = felt_jump_nz;
            libfunc felt_unwrap_nz = unwrap_nz<felt>;
            libfunc call_fib = function_call<user@Fibonacci>;

            // Statement #  0 - tests if n == 0.
            felt_jump_nz(n) { 4(n) fallthrough() };
            // Statement #  1 - n == 0, so we return a.
            felt_ignore(b) -> ();
            store_temp_felt(a)  -> (a);
            return(a);
            // Statement #  4 - calculates arguments for recursion call.
            felt_unwrap_nz(n) -> (n);
            felt_const_minus_1() -> (minus1);
            felt_add(n, minus1) -> (n);
            felt_dup(b) -> (b, b_);
            felt_add(a, b_) -> (a_plus_b);
            store_temp_felt(b) -> (b);
            store_temp_felt(a_plus_b) -> (a_plus_b);
            store_temp_felt(n) -> (n);
            call_fib(b, a_plus_b, n) -> (r);
            return(r);

            Fibonacci@0(a: felt, b: felt, n: felt) -> (felt);
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

#[test_case(0 , 1; "fib(0) => 1")]
#[test_case(1, 1; "fib(1) => 1")]
#[test_case(2, 2;  "fib(2) => 2")]
#[test_case(3, 3;  "fib(3) => 3")]
#[test_case(4, 5;  "fib(4) => 5")]
#[test_case(5, 8;  "fib(5) => 8")]
#[test_case(6, 13;  "fib(6) => 13")]
#[test_case(7, 21;  "fib(7) => 21")]
#[test_case(8, 34;  "fib(8) => 34")]
fn simulate(n: i64, fib: i64) {
    assert_eq!(
        simulation::run(
            &fib_program(),
            &"Fibonacci".into(),
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![n.into()]]
        ),
        Ok(vec![vec![fib.into()]])
    );
}
