use std::fs;
use std::path::PathBuf;

use sierra::program::{Program, StatementIdx};
use test_case::test_case;

use crate::gas_info::GasInfo;
use crate::{calc_gas_info, CostError};

/// Returns a parsed example program from the example directory.
fn get_example_program(name: &str) -> Program {
    // Pop the "/sierra_gas" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["sierra", "examples", &format!("{name}.sierra")].into_iter());
    sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

#[test_case("collatz" =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(7), 35),
                    (StatementIdx(19), 3),
                    (StatementIdx(25), 0),
                    (StatementIdx(38), 0),
                    (StatementIdx(49), 1),
                ].into_iter().collect(),
                function_costs: [("Collatz".into(), 14)].into_iter().collect()
            }))]
#[test_case("fib_jumps" =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(3), 10),
                    (StatementIdx(13), 8),
                    (StatementIdx(27), 6),
                    (StatementIdx(40), 1),
                    (StatementIdx(49), 0),
                ].into_iter().collect(),
                function_costs: [("Fibonacci".into(), 14)].into_iter().collect()
            });
            "fib_jumps")]
#[test_case("fib_recursive" =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(3), 3),
                    (StatementIdx(12), 1),
                    (StatementIdx(19), 31),
                    (StatementIdx(35), 0),
                    (StatementIdx(42), 0),
                ].into_iter().collect(),
                function_costs: [("Fibonacci".into(), 8)].into_iter().collect()
            }))]
fn solve_gas(path: &str) -> Result<GasInfo, CostError> {
    calc_gas_info(&get_example_program(path))
}
