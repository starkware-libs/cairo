use filesystem::db::FilesGroup;
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use semantic::test_utils::{setup_test_crate, SemanticDatabaseForTesting};

use crate::contract::{find_contracts, get_external_functions, starknet_keccak};

#[test]
fn test_contract_resolving() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let _crate_id = setup_test_crate(
        db,
        indoc! {"
            mod NotAContract {}

            #[contract]
            mod ERC20 {
                func internal_func(ref system: System) -> felt {
                    1
                }
            }

            mod __external {
                func ep1() {}
                func ep2() {}
            }
        "},
    );

    let contracts = find_contracts(db, &db.crates());
    assert_eq!(contracts.len(), 1);

    assert_eq!(
        get_external_functions(db, &contracts[0])
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
