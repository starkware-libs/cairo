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
                (StatementIdx(11), 0),
                (StatementIdx(25), 2),
                (StatementIdx(34), 0),
                (StatementIdx(43), 1),
            ]),
            &"Collatz".into(),
            vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gb), CoreValue::Uint128(n)]
        ),
        Ok(vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(new_gb), CoreValue::Uint128(index)])
    );
}

#[test_case((1000, 0), (1011, 1); "0 => 1")]
#[test_case((1000, 1), (1006, 1); "1 => 1")]
#[test_case((1000, 2), (987, 2); "2 => 2")]
#[test_case((1000, 3), (973, 3); "3 => 3")]
#[test_case((1000, 4), (959, 5); "4 => 5")]
#[test_case((1000, 5), (945, 8); "5 => 8")]
#[test_case((1000, 6), (931, 13); "6 => 13")]
#[test_case((1000, 7), (917, 21); "7 => 21")]
#[test_case((1000, 8), (903, 34); "8 => 34")]
#[test_case((100, 80), (2, -1); "Out of gas.")]
fn simulate_fib_jumps((gb, n): (i64, i128), (new_gb, fib): (i64, i128)) {
    assert_eq!(
        simulation::run(
            &get_example_program("fib_jumps"),
            &HashMap::from([
                (StatementIdx(3), 11),
                (StatementIdx(13), 6),
                (StatementIdx(27), 14),
                (StatementIdx(33), 0),
                (StatementIdx(50), 1),
            ]),
            &"Fibonacci".into(),
            vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gb), CoreValue::Felt(n)]
        ),
        Ok(vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(new_gb), CoreValue::Felt(fib)])
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
#[test_case((1000, 2), (962, 2); "2 => 2")]
#[test_case((1000, 3), (918, 3); "3 => 3")]
#[test_case((1000, 4), (835, 5); "4 => 5")]
#[test_case((1000, 5), (708, 8); "5 => 8")]
#[test_case((1000, 6), (498, 13); "6 => 13")]
#[test_case((1000, 7), (161, 21); "7 => 21")]
#[test_case((1500, 8), (114, 34); "8 => 34")]
#[test_case((100, 80), (10, -3); "Out of gas.")]
fn simulate_fib_recursive((gb, n): (i64, i128), (new_gb, fib): (i64, i128)) {
    assert_eq!(
        simulation::run(
            &get_example_program("fib_recursive"),
            &HashMap::from([
                (StatementIdx(3), 6),
                (StatementIdx(12), 1),
                (StatementIdx(19), 45),
                (StatementIdx(21), 0),
                (StatementIdx(42), 0),
            ]),
            &"Fibonacci".into(),
            vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(gb), CoreValue::Felt(n)]
        ),
        Ok(vec![CoreValue::RangeCheck, CoreValue::GasBuiltin(new_gb), CoreValue::Felt(fib)])
    );
}
