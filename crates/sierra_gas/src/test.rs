use std::fs;
use std::path::PathBuf;

use sierra::extensions::builtin_cost::CostTokenType;
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

#[test_case("fib_jumps" =>
            Ok(GasInfo {
                variable_values: [
                    ((StatementIdx(3), CostTokenType::Step), 13),
                    ((StatementIdx(13), CostTokenType::Step), 11),
                    ((StatementIdx(27), CostTokenType::Step), 8),
                    ((StatementIdx(40), CostTokenType::Step), 4),
                    ((StatementIdx(49), CostTokenType::Step), 0),
                ].into_iter().collect(),
                function_costs: [(
                    "Fibonacci".into(),
                    [(CostTokenType::Step, 17)].into_iter().collect()
                )].into_iter().collect()
            });
            "fib_jumps")]
#[test_case("fib_recursive" =>
            Ok(GasInfo {
                variable_values: [
                    ((StatementIdx(3), CostTokenType::Step), 6),
                    ((StatementIdx(12), CostTokenType::Step), 4),
                    ((StatementIdx(19), CostTokenType::Step), 36),
                    ((StatementIdx(35), CostTokenType::Step), 0),
                    ((StatementIdx(42), CostTokenType::Step), 0),
                ].into_iter().collect(),
                function_costs: [(
                    "Fibonacci".into(),
                    [(CostTokenType::Step, 11)].into_iter().collect()
                )].into_iter().collect()
            }))]
fn solve_gas(path: &str) -> Result<GasInfo, CostError> {
    calc_gas_info(&get_example_program(path))
}
