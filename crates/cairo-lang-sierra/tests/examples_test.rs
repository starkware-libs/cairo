use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use cairo_lang_sierra::extensions::core::{CoreLibfunc, CoreType};
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_sierra::program_registry::ProgramRegistry;
use cairo_lang_sierra::simulation::value::CoreValue;
use cairo_lang_sierra::simulation::{self};
use num_bigint::ToBigInt;
use pretty_assertions::assert_eq;
use test_case::test_case;

/// Returns a parsed example program from the example directory.
fn get_example_program(name: &str) -> Program {
    let path: PathBuf =
        [env!("CARGO_MANIFEST_DIR"), "examples", &format!("{name}.sierra")].into_iter().collect();
    cairo_lang_sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

#[test_case("fib_jumps")]
#[test_case("fib_no_gas")]
fn parse(name: &str) {
    get_example_program(name);
}

#[test_case("fib_jumps")]
#[test_case("fib_no_gas")]
fn create_registry(name: &str) {
    ProgramRegistry::<CoreType, CoreLibfunc>::new(&get_example_program(name)).unwrap();
}

#[test_case((1000, 0), (1000, 1); "0 => 1")]
#[test_case((1000, 1), (989, 1); "1 => 1")]
#[test_case((1000, 2), (978, 2); "2 => 2")]
#[test_case((1000, 3), (967, 3); "3 => 3")]
#[test_case((1000, 4), (956, 5); "4 => 5")]
#[test_case((1000, 5), (945, 8); "5 => 8")]
#[test_case((1000, 6), (934, 13); "6 => 13")]
#[test_case((1000, 7), (923, 21); "7 => 21")]
#[test_case((1000, 8), (912, 34); "8 => 34")]
#[test_case((100, 80), (1, -1); "Out of gas.")]
fn simulate_fib_jumps((gb, n): (i64, i128), (new_gb, fib): (i64, i128)) {
    assert_eq!(
        simulation::run(
            &get_example_program("fib_jumps"),
            &HashMap::from([
                (StatementIdx(28), 11),
                (StatementIdx(29), 0),
                (StatementIdx(40), 0),
                (StatementIdx(45), 0),
                (StatementIdx(19), 5),
                (StatementIdx(22), 0),
                (StatementIdx(26), 0),
                (StatementIdx(2), 14),
                (StatementIdx(4), 0),
                (StatementIdx(9), 0),
            ]),
            &"Fibonacci".into(),
            vec![
                CoreValue::RangeCheck,
                CoreValue::GasBuiltin(gb),
                CoreValue::Felt252(n.to_bigint().unwrap())
            ]
        ),
        Ok(vec![
            CoreValue::RangeCheck,
            CoreValue::GasBuiltin(new_gb),
            CoreValue::Felt252(fib.to_bigint().unwrap())
        ])
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
            &HashMap::from([(StatementIdx(1), 0), (StatementIdx(6), 0),]),
            &"Fibonacci".into(),
            vec![
                // a=
                CoreValue::Felt252(1.to_bigint().unwrap()),
                // b=
                CoreValue::Felt252(1.to_bigint().unwrap()),
                CoreValue::Felt252(n.to_bigint().unwrap())
            ]
        ),
        Ok(vec![CoreValue::Felt252(fib.to_bigint().unwrap())])
    );
}
