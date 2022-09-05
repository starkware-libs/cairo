use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;

use crate::db::SierraGenGroup;
use crate::test_utils::{replace_libfunc_ids, DatabaseImpl};

#[test]
fn test_function_generator() {
    let mut db = DatabaseImpl::default();
    let module_id = setup_test_module(
        &mut db,
        indoc! {"
                func foo(a: felt) -> felt {
                    5
                }
            "},
    );
    let foo = match db.module_items(module_id).expect("").unwrap().items["foo"] {
        ModuleItemId::FreeFunction(foo) => foo,
        _ => panic!("Unexpected item type."),
    };

    let function = db.get_function_code(foo).expect("").unwrap();
    assert_eq!(
        function
            .body
            .iter()
            .map(|x| replace_libfunc_ids(&db, x).to_string())
            .collect::<Vec<String>>(),
        vec!["label0:", "felt_const<5>() -> ([1])", "store_temp<[0]>([1]) -> ([2])", "return([2])",]
    );
}
