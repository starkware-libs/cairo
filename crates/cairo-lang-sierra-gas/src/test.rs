use std::fs;
use std::path::PathBuf;

use cairo_lang_sierra::program::Program;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::{calc_gas_postcost_info, calc_gas_precost_info};

cairo_lang_test_utils::test_file_test!(
    test_solve_gas,
    "src/test_data",
    {
        fib_jumps :"fib_jumps",
    },
    test_solve_gas
);

/// Returns a parsed example program from the example directory.
fn get_example_program(name: &str) -> Program {
    // Pop the "/sierra_gas" suffix.
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();
    path.extend(["cairo-lang-sierra", "examples", &format!("{name}.sierra")]);
    cairo_lang_sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

fn test_solve_gas(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let path = &inputs["test_file_name"];
    let program = get_example_program(path);

    let gas_info0 = calc_gas_precost_info(&program, Default::default()).unwrap();
    let gas_info1 =
        calc_gas_postcost_info(&program, Default::default(), &gas_info0, |_| 0).unwrap();
    let gas_info = gas_info0.combine(gas_info1);

    TestRunnerResult::success(OrderedHashMap::from([(
        "gas_solution".into(),
        format!("{gas_info}"),
    )]))
}
