use std::path::{Path, PathBuf};

use cairo_lang_sierra::program::VersionedProgram;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use test_case::test_case;

cairo_lang_test_utils::test_file_test!(
    serde_sierra_json,
    "src/test_data",
    {
        fib_jumps: "fib_jumps",
        fib_no_gas: "fib_no_gas",
    },
    test_sierra_serde_json
);

// Ensuring Sierra Program is not changing the structure.
fn test_sierra_serde_json(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let prog: VersionedProgram = serde_json::from_str(&inputs["pretty_json"].clone())
        .expect("Could not deserialize VersionedProgram.");
    let json = serde_json::to_string_pretty(&prog).expect("Could not serialize VersionedProgram.");
    TestRunnerResult::success(OrderedHashMap::from([("pretty_json".into(), json)]))
}

fn get_path(file_name: &str, ext: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join(Path::new(&format!("examples/{file_name}.{ext}")))
}

fn get_test_program_from_sierra(example_file_name: &str) -> VersionedProgram {
    let path = get_path(example_file_name, "sierra");
    let parser = cairo_lang_sierra::ProgramParser::new();
    parser
        .parse(&std::fs::read_to_string(path).expect("Could not read example program."))
        .expect("Could not parse example program.")
        .into_artifact()
}

// Parse code, serialize, and then deserialize it, ensuring the original parsed code is retained.
#[test_case("fib_jumps")]
#[test_case("fib_no_gas")]
fn serde_json_from_parser_test(example_name: &str) {
    let json = serde_json::to_string(&get_test_program_from_sierra(example_name))
        .expect("Could not serialize VersionedProgram to json.");
    let prog = serde_json::from_str::<VersionedProgram>(&json)
        .expect("Could not deserialize VersionedProgram from json.");
    assert_eq!(
        json,
        serde_json::to_string(&prog).unwrap(),
        "Could not serialize and deserialize VersionedProgram."
    );
}
