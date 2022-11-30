use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::{setup_test_crate, SemanticDatabaseForTesting};

use crate::contract::{find_contract_structs, resolve_contract_impls, starknet_keccak};

#[test]
fn test_contract_resolving() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let _crate_id = setup_test_crate(
        db,
        indoc! {"
            struct NotAContract {}

            trait IERC20 {}
            impl ERC20Impl of IERC20 {}

            #[contract(ERC20Impl)]
            struct ERC20 {}

            #[contract(ERC20)]
            struct StructInsteadOfImpl {}

            #[contract(1+2)]
            struct BadImplExpr {}

            #[contract(NoSuchPath)]
            struct BadImplPath {}
        "},
    );

    let contracts = find_contract_structs(db);
    assert_eq!(contracts.len(), 4);

    assert_eq!(
        format!("{:?}", resolve_contract_impls(db, &contracts[0]).unwrap()),
        "[ConcreteImplId(0)]"
    );

    assert_eq!(
        resolve_contract_impls(db, &contracts[1]).expect_err("").to_string(),
        "`ERC20` is not an `impl`."
    );

    assert_eq!(
        resolve_contract_impls(db, &contracts[2]).expect_err("").to_string(),
        "Expected a path, Got `1+2`."
    );

    assert_eq!(
        resolve_contract_impls(db, &contracts[3]).expect_err("").to_string(),
        "Failed to resolve `NoSuchPath`."
    );
}

#[test]
fn test_starknet_keccak() {
    assert_eq!(
        format!("0x{:x}", starknet_keccak("__execute__".as_bytes())),
        "0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad",
    )
}
