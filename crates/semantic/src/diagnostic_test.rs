use defs::ids::ModuleId;
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::semantic_test;
use crate::test_utils::{setup_test_crate, test_expr_diagnostics, SemanticDatabaseForTesting};

semantic_test!(diagnostics_tests, ["src/diagnostic_test_data/tests"], test_expr_diagnostics);

#[test]
fn test_missing_module_file() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = setup_test_crate(db, "mod abc;");

    assert_eq!(
        db.module_semantic_diagnostics(ModuleId::CrateRoot(crate_id)).unwrap().format(db),
        indoc! {"
            error: File not found.
             --> lib.cairo:1:1
            mod abc;
            ^******^

            "
        },
    );
}
