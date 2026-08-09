use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_test_utils::verify_diagnostics_expectation;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::path::ContextualizePath;
use crate::test_utils::{
    SemanticDatabaseForTesting, setup_test_function_from_content, setup_test_module_ex,
};

cairo_lang_test_utils::test_file_test!(
    path,
    "src/path_test_data",
    {
        tests: "tests",
    },
    test_path_diagnostics,
    ["expect_diagnostics"]
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

/// Verifies that types, generic args, and concrete functions contextualize recursively - each
/// path component shortened to the shortest form valid in the context module.
#[test]
fn test_contextualized_types_and_functions() {
    let db = &SemanticDatabaseForTesting::default();
    let test_function = setup_test_function_from_content(
        db,
        indoc::indoc! {"
            #[target_function]
            fn foo(
                a: u32,
                b: Option<(felt252, @u64)>,
                c: [u8; 3],
                d: Pedersen,
            ) {}
        "},
        None,
        None,
    )
    .unwrap();
    let module_id = test_function.module_id;
    let signature = &test_function.signature;

    let contextualized_param_tys = signature
        .params
        .iter()
        .map(|param| param.ty.contextualized_path(db, module_id).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(
        contextualized_param_tys,
        ["u32", "Option::<(felt252, @u64)>", "[u8; 3]", "Pedersen"]
    );

    assert_eq!(
        test_function.concrete_function_id.contextualized_path(db, module_id).unwrap(),
        "foo"
    );
}
