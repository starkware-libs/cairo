use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::items::module::ModuleSemantic;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::try_extract_matches;
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use test_case::test_case;

use super::get_dummy_program_for_size_estimation;
use crate::db::SierraGenGroup;
use crate::program_generator::SierraProgramWithDebug;
use crate::replace_ids::replace_sierra_ids_in_program;
use crate::test_utils::{
    SierraGenDatabaseForTesting, checked_compile_to_sierra, setup_db_and_get_crate_id,
};

cairo_lang_test_utils::test_file_test!(
    program_generator,
    "src/program_generator_test_data",
    {
        coupon: "coupon",
        function_call: "function_call",
        type_dependency: "type_dependency",
    },
    test_program_generator
);

cairo_lang_test_utils::test_file_test!(
    dummy_program_generator,
    "src/dummy_program_generator_test_data",
    {
        simple: "simple",
    },
    test_dummy_program_generator
);

fn test_dummy_program_generator(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SierraGenDatabaseForTesting::default();

    // Parse code and create semantic model.
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    // Compile the function.
    let function_id =
        ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

    db.module_lowering_diagnostics(test_function.module_id)
        .unwrap()
        .expect_with_db(db, "Unexpected lowering diagnostics.");

    let program = get_dummy_program_for_size_estimation(db, function_id)
        .expect("`get_sierra_program` failed. run with RUST_LOG=warn (or less) to see diagnostics");

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("sierra_code".into(), replace_sierra_ids_in_program(db, &program).to_string()),
    ]))
}

fn test_program_generator(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let program = checked_compile_to_sierra(inputs["cairo_code"].as_str());
    TestRunnerResult::success(OrderedHashMap::from([("sierra_code".into(), program.to_string())]))
}

#[test_case("f1", &["f1", "f2", "f3", "f4", "f5", "f6"]; "finds all")]
#[test_case("f2", &["f2", "f3", "f4", "f5", "f6"]; "all but first")]
#[test_case("f3", &["f3", "f5", "f6"]; "f3 -> f5 -> f6")]
#[test_case("f4", &["f4", "f5", "f6"]; "f4 -> (f5 -> f6, f6)")]
#[test_case("f5", &["f5", "f6"]; "f5 -> f6")]
#[test_case("f6", &["f6"]; "self loop")]
fn test_only_include_dependencies(func_name: &str, sierra_used_funcs: &[&str]) {
    let db = SierraGenDatabaseForTesting::default();
    let crate_id = setup_db_and_get_crate_id(
        &db,
        indoc! {"
        #[inline(never)]
        fn f1() { f2(); f3(); }
        #[inline(never)]
        fn f2() { f3(); f4(); f5(); }
        #[inline(never)]
        fn f3() { f5(); }
        #[inline(never)]
        fn f4() { f5(); f6(); }
        #[inline(never)]
        fn f5() { f6(); }
        #[inline(never)]
        fn f6() { f6(); }
    "},
    );
    let func_id = ConcreteFunctionWithBodyId::from_no_generics_free(
        &db,
        db.crate_modules(crate_id)
            .iter()
            .find_map(|module_id| {
                try_extract_matches!(
                    db.module_item_by_name(*module_id, SmolStrId::from(&db, func_name))
                        .unwrap()
                        .unwrap(),
                    ModuleItemId::FreeFunction
                )
            })
            .unwrap(),
    )
    .unwrap();
    let SierraProgramWithDebug { program, .. } =
        db.get_sierra_program_for_functions(vec![func_id]).unwrap();
    assert_eq!(
        replace_sierra_ids_in_program(&db, program)
            .funcs
            .into_iter()
            .filter_map(|f| f.id.to_string().strip_prefix("test::").map(|s| s.to_string()))
            .collect_vec(),
        sierra_used_funcs
    );
}
