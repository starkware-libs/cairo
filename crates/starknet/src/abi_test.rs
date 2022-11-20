use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use indoc::indoc;
use semantic::test_utils::{setup_test_module, SemanticDatabaseForTesting};
use utils::extract_matches;

use crate::abi::Contract;

#[test]
fn test_abi() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            trait MyAbi {
                func foo(a: felt, b: uint128) -> Option::<()>;
            }
        "},
    )
    .unwrap()
    .module_id;

    let db = &db_val;
    let trait_id = extract_matches!(
        db.module_item_by_name(module_id, "MyAbi".into()).unwrap(),
        ModuleItemId::Trait
    );
    let abi = Contract::from_trait(db, trait_id);
    // TODO(spapini): Replace with json.
    assert_eq!(
        format!("{:?}", abi),
        "Ok(Contract { functions: [Function { name: \"foo\", inputs: [Input { name: \"a\", ty: \
         \"core::felt\" }, Input { name: \"b\", ty: \"core::integer::uint128\" }], output_ty: \
         \"core::option::Option::<()>\" }] })"
    );
}
