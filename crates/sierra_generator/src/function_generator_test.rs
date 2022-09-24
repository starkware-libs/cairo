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

    let function = db.get_function_code(foo).expect("").unwrap();
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
            "store_temp<[0]>([3]) -> ([4])",
            "felt_dup([4]) -> ([4], [10])",
            "store_temp<[0]>([10]) -> ([5])",
            "felt_dup([4]) -> ([4], [11])",
            "store_temp<[0]>([11]) -> ([6])",
            "store_temp<[0]>([4]) -> ([7])",
            "function_call<user@[0]>([5], [6], [7]) -> ([8])",
            "store_temp<[0]>([8]) -> ([9])",
            "return([9])",
        ]
    );
}
