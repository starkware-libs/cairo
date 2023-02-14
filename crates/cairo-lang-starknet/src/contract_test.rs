use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_semantic::test_utils::setup_test_crate;
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;

use crate::contract::{find_contracts, get_module_functions, starknet_keccak};
use crate::db::StarknetRootDatabaseBuilderEx;
use crate::plugin::consts::EXTERNAL_MODULE;

#[test]
fn test_contract_resolving() {
    let db = &mut RootDatabase::builder().detect_corelib().with_starknet().build().unwrap();
    let _crate_id = setup_test_crate(
        db,
        indoc! {"
            mod NotAContract {}

            #[contract]
            mod ERC20 {
                fn internal_func(ref system: System) -> felt {
                    1
                }

                #[external]
                fn ep1() {}

                #[external]
                fn ep2() {}
            }
        "},
    );

    let contracts = find_contracts(db, &db.crates());
    assert_eq!(contracts.len(), 1);

    assert_eq!(
        get_module_functions(db, &contracts[0], EXTERNAL_MODULE)
            .unwrap()
            .into_iter()
            .map(|func_id| func_id.name(db))
            .collect_vec(),
        vec!["ep1", "ep2"]
    );
}

#[test]
fn test_starknet_keccak() {
    assert_eq!(
        format!("0x{:x}", starknet_keccak("__execute__".as_bytes())),
        "0x15d40a3d6ca2ac30f4031e42be28da9b056fef9bb7357ac5e85627ee876e5ad",
    )
}
