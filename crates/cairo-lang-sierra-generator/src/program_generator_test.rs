use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::try_extract_matches;
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use test_case::test_case;

use crate::db::SierraGenGroup;
use crate::program_generator::SierraProgramWithDebug;
use crate::replace_ids::replace_sierra_ids_in_program;
use crate::test_utils::{checked_compile_to_sierra, setup_db_and_get_crate_id};

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

fn test_program_generator(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let program = checked_compile_to_sierra(inputs["cairo_code"].as_str());
    TestRunnerResult::success(OrderedHashMap::from([("sierra_code".into(), program.to_string())]))
}

#[test_case(
    "f1",
    &[
        "test::f1", "test::f2", "test::f3",
        "test::f4", "test::f5", "test::f6",
    ];
    "finds all"
)]
#[test_case(
    "f2",
    &[
        "test::f2", "test::f3", "test::f4", "test::f5", "test::f6",
    ];
    "all but first"
)]
#[test_case("f3", &["test::f3", "test::f5", "test::f6"]; "f3 -> f5 -> f6")]
#[test_case("f4", &["test::f4", "test::f5", "test::f6"]; "f4 -> (f5 -> f6, f6)")]
#[test_case("f5", &["test::f5", "test::f6"]; "f5 -> f6")]
#[test_case("f6", &["test::f6"]; "self loop")]
fn test_only_include_dependencies(func_name: &str, sierra_used_funcs: &[&str]) {
    let (db, crate_id) = setup_db_and_get_crate_id(indoc! {"
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
    "});
    let func_id = ConcreteFunctionWithBodyId::from_no_generics_free(
        &db,
        db.crate_modules(crate_id)
            .iter()
            .find_map(|module_id| {
                try_extract_matches!(
                    db.module_item_by_name(*module_id, func_name.into()).unwrap().unwrap(),
                    ModuleItemId::FreeFunction
                )
            })
            .unwrap(),
    )
    .unwrap();
    let SierraProgramWithDebug { program, .. } =
        Arc::unwrap_or_clone(db.get_sierra_program_for_functions(vec![func_id]).unwrap());
    assert_eq!(
        replace_sierra_ids_in_program(&db, &program)
            .funcs
            .into_iter()
            .map(|f| f.id.to_string())
            .collect_vec(),
        sierra_used_funcs
    );
}
