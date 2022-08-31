use indoc::indoc;
use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program_registry::ProgramRegistry;
use sierra::simulation;

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

#[test]
fn simulate_test() {
    let program = fib_program();
    let id = "Fibonacci".into();
    // 1, 1, 2, 3, 5, 8, 13, 21, 34
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 0.into()]],
        ),
        Ok(vec![vec![/* fib= */ 1.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 1.into()]]
        ),
        Ok(vec![vec![/* fib= */ 1.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 2.into()]]
        ),
        Ok(vec![vec![/* fib= */ 2.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 3.into()]]
        ),
        Ok(vec![vec![/* fib= */ 3.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 4.into()]]
        ),
        Ok(vec![vec![/* fib= */ 5.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 5.into()]]
        ),
        Ok(vec![vec![/* fib= */ 8.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 6.into()]]
        ),
        Ok(vec![vec![/* fib= */ 13.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 7.into()]]
        ),
        Ok(vec![vec![/* fib= */ 21.into()]])
    );
    assert_eq!(
        simulation::run(
            &program,
            &id,
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![/* n= */ 8.into()]]
        ),
        Ok(vec![vec![/* fib= */ 34.into()]])
    );
}
