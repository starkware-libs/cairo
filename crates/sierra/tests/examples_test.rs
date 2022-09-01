use std::fs;
use std::path::PathBuf;

use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program::Program;
use sierra::program_registry::ProgramRegistry;
use sierra::simulation;
use test_case::test_case;

const COLLATZ: &str = "examples/collatz.sierra";
const FIB_JUMPS: &str = "examples/fib_jumps.sierra";
const FIB_NO_GAS: &str = "examples/fib_no_gas.sierra";
const FIB_RECURSIVE: &str = "examples/fib_recursive.sierra";

/// Returns a parsed example program from the example directory.
fn get_example_program(path: &str) -> Program {
    sierra::ProgramParser::new().parse(&fs::read_to_string(PathBuf::from(path)).unwrap()).unwrap()
}

#[test_case(COLLATZ)]
#[test_case(FIB_JUMPS)]
#[test_case(FIB_NO_GAS)]
#[test_case(FIB_RECURSIVE)]
fn parse(name: &str) {
    get_example_program(name);
}

#[test_case(COLLATZ)]
#[test_case(FIB_JUMPS)]
#[test_case(FIB_NO_GAS)]
#[test_case(FIB_RECURSIVE)]
fn create_registry(name: &str) {
    ProgramRegistry::<CoreType, CoreLibFunc>::new(&get_example_program(name)).unwrap();
}

// 5 -> 16 -> 8 -> 4 -> 2 -> 1
#[test_case((100, 5), (47, 5); "5 => 5")]
//  0     1     2     3     4     5     6     7     8     9
//  7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 ->
// 10 ->  5 -> 16 ->  8 ->  4 ->  2 ->  1
#[test_case((200, 7), (30, 16); "7 => 16")]
// Out of gas.
#[test_case((100, 7), (5, -1); "Out of gas.")]
fn simulate((gb, n): (i64, i64), (new_gb, index): (i64, i64)) {
    assert_eq!(
        simulation::run(
            &get_example_program(COLLATZ),
            &"Collatz".into(),
            vec![vec![gb.into()], vec![n.into()]]
        ),
        Ok(vec![vec![new_gb.into()], vec![index.into()]])
    );
}

#[test_case((1000, 0), (1007, 1); "0 => 1")]
#[test_case((1000, 1), (1005, 1); "1 => 1")]
#[test_case((1000, 2), (996, 2); "2 => 2")]
#[test_case((1000, 3), (991, 3); "3 => 3")]
#[test_case((1000, 4), (986, 5); "4 => 5")]
#[test_case((1000, 5), (981, 8); "5 => 8")]
#[test_case((1000, 6), (976, 13); "6 => 13")]
#[test_case((1000, 7), (971, 21); "7 => 21")]
#[test_case((1000, 8), (966, 34); "8 => 34")]
#[test_case((100, 80), (0, -1); "Out of gas.")]
fn simulate_fib_jumps((gb, n): (i64, i64), (new_gb, fib): (i64, i64)) {
    assert_eq!(
        simulation::run(
            &get_example_program(FIB_JUMPS),
            &"Fibonacci".into(),
            vec![vec![gb.into()], vec![n.into()]]
        ),
        Ok(vec![vec![new_gb.into()], vec![fib.into()]])
    );
}

#[test_case(0, 1; "0 => 1")]
#[test_case(1, 1; "1 => 1")]
#[test_case(2, 2; "2 => 2")]
#[test_case(3, 3; "3 => 3")]
#[test_case(4, 5; "4 => 5")]
#[test_case(5, 8; "5 => 8")]
#[test_case(6, 13; "6 => 13")]
#[test_case(7, 21; "7 => 21")]
#[test_case(8, 34; "8 => 34")]
fn simulate_fib_no_gas(n: i64, fib: i64) {
    assert_eq!(
        simulation::run(
            &get_example_program(FIB_NO_GAS),
            &"Fibonacci".into(),
            vec![vec![/* a= */ 1.into()], vec![/* b= */ 1.into()], vec![n.into()]]
        ),
        Ok(vec![vec![fib.into()]])
    );
}

#[test_case((1000, 0), (1003, 1); "0 => 1")]
#[test_case((1000, 1), (1001, 1); "1 => 1")]
#[test_case((1000, 2), (978, 2); "2 => 2")]
#[test_case((1000, 3), (953, 3); "3 => 3")]
#[test_case((1000, 4), (905, 5); "4 => 5")]
#[test_case((1000, 5), (832, 8); "5 => 8")]
#[test_case((1000, 6), (711, 13); "6 => 13")]
#[test_case((1000, 7), (517, 21); "7 => 21")]
#[test_case((1000, 8), (202, 34); "8 => 34")]
#[test_case((100, 80), (22, -40000); "Out of gas.")]
fn simulate_fib_recursive((gb, n): (i64, i64), (new_gb, fib): (i64, i64)) {
    assert_eq!(
        simulation::run(
            &get_example_program(FIB_RECURSIVE),
            &"Fibonacci".into(),
            vec![vec![gb.into()], vec![n.into()]]
        ),
        Ok(vec![vec![new_gb.into()], vec![fib.into()]])
    );
}
