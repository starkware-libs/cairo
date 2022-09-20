use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use sierra::ids::SymbolId;
use sierra::program::Program;
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
                ("gas1".into(), 30),
                ("gas2".into(), 0),
                ("gas3".into(), 2),
                ("gas4".into(), 0),
                ("gas5".into(), 1),
            ]));
            "collatz")]
#[test_case(FIB_JUMPS =>
            Ok(HashMap::from([
                ("gas1".into(), 13),
                ("gas2".into(), 10),
                ("gas3".into(), 5),
                ("gas4".into(), 0),
                ("gas5".into(), 1),
            ]));
            "fib_jumps")]
#[test_case(FIB_RECURSIVE =>
            Ok(HashMap::from([
                ("gas1".into(), 42),
                ("gas2".into(), 6),
                ("gas3".into(), 1),
                ("gas4".into(), 0),
                ("gas5".into(), 0),
            ]));
            "fib_recursive")]
fn solve_gas(path: &str) -> Result<HashMap<SymbolId, i64>, CostError> {
    calc_gas_symbols(&get_example_program(path))
}
