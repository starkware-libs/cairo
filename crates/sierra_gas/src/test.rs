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
                    (StatementIdx(7), 29),
                    (StatementIdx(10), 0),
                    (StatementIdx(23), 2),
                    (StatementIdx(32), 0),
                    (StatementIdx(41), 1),
                ].into_iter().collect(),
                function_costs: [("Collatz".into(), 12)].into_iter().collect()
            }))]
#[test_case("fib_jumps" =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(2), 11),
                    (StatementIdx(11), 6),
                    (StatementIdx(23), 13),
                    (StatementIdx(28), 0),
                    (StatementIdx(43), 1),
                ].into_iter().collect(),
                function_costs: [("Fibonacci".into(), 14)].into_iter().collect()
            });
            "fib_jumps")]
#[test_case("fib_recursive" =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(3), 6),
                    (StatementIdx(11), 1),
                    (StatementIdx(17), 41),
                    (StatementIdx(18), 0),
                    (StatementIdx(37), 0),
                ].into_iter().collect(),
                function_costs: [("Fibonacci".into(), 10)].into_iter().collect()
            }))]
fn solve_gas(path: &str) -> Result<GasInfo, CostError> {
    calc_gas_info(&get_example_program(path))
}
