use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use sierra::program::{Program, StatementIdx};
use test_case::test_case;

use crate::{calc_gas_symbols, CostError};

const COLLATZ: &str = "collatz.sierra";
const FIB_JUMPS: &str = "fib_jumps.sierra";
const FIB_RECURSIVE: &str = "fib_recursive.sierra";

/// Returns a parsed example program from the example directory.
fn get_example_program(path: &str) -> Program {
    let path: PathBuf = [env!("CARGO_MANIFEST_DIR"), "../sierra/examples", path].iter().collect();
    sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

#[test_case(COLLATZ =>
            Ok(HashMap::from([
                (StatementIdx(7), 30),
                (StatementIdx(10), 0),
                (StatementIdx(23), 2),
                (StatementIdx(32), 0),
                (StatementIdx(41), 1),
            ]));
            "collatz")]
#[test_case(FIB_JUMPS =>
            Ok(HashMap::from([
                (StatementIdx(1), 10),
                (StatementIdx(10), 5),
                (StatementIdx(21), 13),
                (StatementIdx(26), 0),
                (StatementIdx(41), 1),
            ]));
            "fib_jumps")]
#[test_case(FIB_RECURSIVE =>
            Ok(HashMap::from([
                (StatementIdx(3), 6),
                (StatementIdx(11), 1),
                (StatementIdx(17), 43),
                (StatementIdx(18), 0),
                (StatementIdx(37), 0),
            ]));
            "fib_recursive")]
fn solve_gas(path: &str) -> Result<HashMap<StatementIdx, i64>, CostError> {
    calc_gas_symbols(&get_example_program(path))
}
