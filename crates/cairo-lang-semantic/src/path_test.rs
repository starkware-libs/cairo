use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module_ex};

cairo_lang_test_utils::test_file_test!(
    path,
    "src/path_test_data",
    {
        tests: "tests",
    },
    test_path_diagnostics
);

pub fn test_path_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SemanticDatabaseForTesting::default();
    let setup = setup_test_module_ex(
        db,
        &inputs["module_code"],
        inputs.get("crate_settings").map(|s| s.as_str()),
        None,
    );
    let diagnostics = setup.get_diagnostics();
    let error = verify_diagnostics_expectation(args, &diagnostics);

    TestRunnerResult {
        outputs: OrderedHashMap::from([("expected_diagnostics".into(), diagnostics)]),
        error,
    }
}
