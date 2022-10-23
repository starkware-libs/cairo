use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use sierra::extensions::core::{CoreLibFunc, CoreType};
use sierra::program::{Program, StatementIdx};
use sierra::program_registry::ProgramRegistry;
use sierra::simulation::value::CoreValue;
use sierra::simulation::{self};
use test_case::test_case;

/// Returns a parsed example program from the example directory.
fn get_example_program(name: &str) -> Program {
    let path: PathBuf =
        [env!("CARGO_MANIFEST_DIR"), "examples", &format!("{name}.sierra")].into_iter().collect();
    sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

#[test_case("collatz")]
#[test_case("fib_jumps")]
#[test_case("fib_no_gas")]
#[test_case("fib_recursive")]
fn parse(name: &str) {
    get_example_program(name);
}

#[test_case("collatz")]
#[test_case("fib_jumps")]
#[test_case("fib_no_gas")]
#[test_case("fib_recursive")]
fn create_registry(name: &str) {
    ProgramRegistry::<CoreType, CoreLibFunc>::new(&get_example_program(name)).unwrap();
}

// 5 -> 16 -> 8 -> 4 -> 2 -> 1
#[test_case((800, 5), (659, 5); "5 => 5")]
//  0     1     2     3     4     5     6     7     8     9
//  7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 ->
// 10 ->  5 -> 16 ->  8 ->  4 ->  2 ->  1
#[test_case((800, 7), (343, 16); "7 => 16")]
// Out of gas.
#[test_case((400, 7), (26, u128::MAX); "Out of gas.")]
fn simulate_collatz((gb, n): (i64, u128), (new_gb, index): (i64, u128)) {
    assert_eq!(
        simulation::run(
            &get_example_program("collatz"),
            &HashMap::from([
                (StatementIdx(7), 30),
                (StatementIdx(10), 0),
                (StatementIdx(23), 2),
                (StatementIdx(32), 0),
                (StatementIdx(41), 1),
            ]),
            &"Collatz".into(),
            vec![CoreValue::GasBuiltin(gb), CoreValue::Uint128(n)]
        ),
        Ok(vec![CoreValue::GasBuiltin(new_gb), CoreValue::Uint128(index)])
    );
}

#[test_case((1000, 0), (1011, 1); "0 => 1")]
#[test_case((1000, 1), (1005, 1); "1 => 1")]
#[test_case((1000, 2), (988, 2); "2 => 2")]
#[test_case((1000, 3), (975, 3); "3 => 3")]
#[test_case((1000, 4), (962, 5); "4 => 5")]
#[test_case((1000, 5), (949, 8); "5 => 8")]
#[test_case((1000, 6), (936, 13); "6 => 13")]
#[test_case((1000, 7), (923, 21); "7 => 21")]
#[test_case((1000, 8), (910, 34); "8 => 34")]
#[test_case((100, 80), (9, u128::MAX); "Out of gas.")]
fn simulate_fib_jumps((gb, n): (i64, u128), (new_gb, fib): (i64, u128)) {
    assert_eq!(
        simulation::run(
            &get_example_program("fib_jumps"),
            &HashMap::from([
                (StatementIdx(2), 11),
                (StatementIdx(11), 5),
                (StatementIdx(23), 13),
                (StatementIdx(28), 0),
                (StatementIdx(43), 1),
            ]),
            &"Fibonacci".into(),
            vec![CoreValue::GasBuiltin(gb), CoreValue::Uint128(n)]
        ),
        Ok(vec![CoreValue::GasBuiltin(new_gb), CoreValue::Uint128(fib)])
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
fn simulate_fib_no_gas(n: i128, fib: i128) {
    assert_eq!(
        simulation::run(
            &get_example_program("fib_no_gas"),
            &HashMap::new(),
            &"Fibonacci".into(),
            vec![
                // a=
                CoreValue::Felt(1),
                // b=
                CoreValue::Felt(1),
                CoreValue::Felt(n)
            ]
        ),
        Ok(vec![CoreValue::Felt(fib)])
    );
}

#[test_case((1000, 0), (1006, 1); "0 => 1")]
#[test_case((1000, 1), (1001, 1); "1 => 1")]
#[test_case((1000, 2), (965, 2); "2 => 2")]
#[test_case((1000, 3), (924, 3); "3 => 3")]
#[test_case((1000, 4), (847, 5); "4 => 5")]
#[test_case((1000, 5), (729, 8); "5 => 8")]
#[test_case((1000, 6), (534, 13); "6 => 13")]
#[test_case((1000, 7), (221, 21); "7 => 21")]
#[test_case((1500, 8), (213, 34); "8 => 34")]
#[test_case((100, 80), (16, u128::MAX.wrapping_mul(/*error_count=*/3)); "Out of gas.")]
fn simulate_fib_recursive((gb, n): (i64, u128), (new_gb, fib): (i64, u128)) {
    assert_eq!(
        simulation::run(
            &get_example_program("fib_recursive"),
            &HashMap::from([
                (StatementIdx(3), 6),
                (StatementIdx(11), 1),
                (StatementIdx(17), 42),
                (StatementIdx(18), 0),
                (StatementIdx(37), 0),
            ]),
            &"Fibonacci".into(),
            vec![CoreValue::GasBuiltin(gb), CoreValue::Uint128(n)]
        ),
        Ok(vec![CoreValue::GasBuiltin(new_gb), CoreValue::Uint128(fib)])
    );
}
