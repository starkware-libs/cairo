use test_utils_macros::test;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use pretty_assertions::assert_eq;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_impl() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            #[ABI]
            trait IContract {
                func foo(a: felt);
            }


            #[Contract]
            impl Contract of IContract {
                func foo(a: felt) {
                    return 1;
                }
            }
        "},
    )
    .unwrap();

    let impl_id = extract_matches!(
        db.module_item_by_name(test_module.module_id, "Contract".into()).unwrap(),
        ModuleItemId::Impl
    );

    assert_eq!(format!("{:?}", db.impl_generic_params(impl_id).unwrap()), "[]");

    assert_eq!(format!("{:?}", db.impl_functions(impl_id).unwrap()), "[ImplFunctionId(0)]");
}
