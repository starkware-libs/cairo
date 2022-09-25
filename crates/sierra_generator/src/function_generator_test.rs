use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;
use utils::extract_matches;

use crate::db::SierraGenGroup;
use crate::test_utils::{replace_libfunc_ids, SierraGenDatabaseForTesting};

#[test]
fn test_function_generator() {
    let mut db = SierraGenDatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db,
        indoc! {"
                func foo(a: felt, b: felt) -> felt {
                    let b = felt_add(a, 5);
                    bar(b, b, b)
                }

                func bar(x: felt, y: felt, z: felt) -> felt {
                    0
                }
            "},
    )
    .unwrap()
    .module_id;
    let foo = extract_matches!(
        db.module_items(module_id).unwrap().items["foo"],
        ModuleItemId::FreeFunction,
        "Unexpected item type."
    );

    db.free_function_sierra_diagnostics(foo).expect("");
    let function = db.free_function_sierra(foo).unwrap();
    assert_eq!(
        function
            .body
            .iter()
            .map(|x| replace_libfunc_ids(&db, x).to_string())
            .collect::<Vec<String>>(),
        vec![
            "label0:",
            "felt_drop([1]) -> ()",
            "revoke_ap_tracking() -> ()",
            "felt_const<5>() -> ([2])",
            "felt_add([0], [2]) -> ([3])",
            "store_temp<[0]>([3]) -> ([3])",
            "felt_dup([3]) -> ([3], [9])",
            "rename<[0]>([9]) -> ([4])",
            "felt_dup([3]) -> ([3], [10])",
            "store_temp<[0]>([10]) -> ([5])",
            "store_temp<[0]>([3]) -> ([6])",
            "function_call<user@[0]>([4], [5], [6]) -> ([7])",
            "store_temp<[0]>([7]) -> ([8])",
            "return([8])",
        ]
    );
}
