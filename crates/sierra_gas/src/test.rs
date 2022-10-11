use std::fs;
use std::path::PathBuf;

use sierra::program::{Program, StatementIdx};
use test_case::test_case;

use crate::gas_info::GasInfo;
use crate::{calc_gas_info, CostError};

const COLLATZ: &str = "collatz.sierra";
const FIB_JUMPS: &str = "fib_jumps.sierra";
const FIB_RECURSIVE: &str = "fib_recursive.sierra";

/// Returns a parsed example program from the example directory.
fn get_example_program(path: &str) -> Program {
    let path: PathBuf = [env!("CARGO_MANIFEST_DIR"), "../sierra/examples", path].iter().collect();
    sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

#[test_case(COLLATZ =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(7), 30),
                    (StatementIdx(10), 0),
                    (StatementIdx(23), 2),
                    (StatementIdx(32), 0),
                    (StatementIdx(41), 1),
                ].into_iter().collect(),
                function_costs: [("Collatz".into(), 15)].into_iter().collect()
            });
            "collatz")]
#[test_case(FIB_JUMPS =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(1), 10),
                    (StatementIdx(10), 5),
                    (StatementIdx(21), 13),
                    (StatementIdx(26), 0),
                    (StatementIdx(41), 1),
                ].into_iter().collect(),
                function_costs: [("Fibonacci".into(), 16)].into_iter().collect()
            });
            "fib_jumps")]
#[test_case(FIB_RECURSIVE =>
            Ok(GasInfo {
                variable_values: [
                    (StatementIdx(3), 6),
                    (StatementIdx(11), 1),
                    (StatementIdx(17), 43),
                    (StatementIdx(18), 0),
                    (StatementIdx(37), 0),
                ].into_iter().collect(),
                function_costs: [("Fibonacci".into(), 13)].into_iter().collect()
            });
            "fib_recursive")]
fn solve_gas(path: &str) -> Result<GasInfo, CostError> {
    calc_gas_info(&get_example_program(path))
}
