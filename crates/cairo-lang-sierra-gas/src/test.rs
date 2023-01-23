use std::fs;
use std::path::PathBuf;

use cairo_lang_sierra::program::Program;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::calc_gas_info;

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
    path.extend(["cairo-lang-sierra", "examples", &format!("{name}.sierra")].into_iter());
    cairo_lang_sierra::ProgramParser::new().parse(&fs::read_to_string(path).unwrap()).unwrap()
}

fn test_solve_gas(inputs: &OrderedHashMap<String, String>) -> OrderedHashMap<String, String> {
    let path = &inputs["test_file_name"];

    OrderedHashMap::from([(
        "gas_solution".into(),
        format!("{}", calc_gas_info(&get_example_program(path)).unwrap()),
    )])
}
