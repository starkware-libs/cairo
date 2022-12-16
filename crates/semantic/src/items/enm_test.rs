use debug::DebugWithDb;
use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;
use utils::extract_matches;

use crate::db::SemanticGroup;
use crate::test_utils::{setup_test_module, SemanticDatabaseForTesting};

#[test]
fn test_enum() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let (test_module, diagnostics) = setup_test_module(
        db,
        indoc::indoc! {"
            enum A {
                a: felt,
                b: (felt, felt),
                c: (),
                a: (),
                a: ()
            }

            func foo(a: A) {
                5;
            }
        "},
    )
    .split();
    assert_eq!(
        diagnostics,
        indoc! {r#"
        error: Redefinition of variant "a" on enum "test::A".
         --> lib.cairo:5:5
            a: (),
            ^***^

        error: Redefinition of variant "a" on enum "test::A".
         --> lib.cairo:6:5
            a: ()
            ^***^

        "#}
    );
    let module_id = test_module.module_id;

    let enum_id = extract_matches!(
        db.module_item_by_name(module_id, "A".into()).unwrap().unwrap(),
        ModuleItemId::Enum
    );
    let actual = db
        .enum_variants(enum_id)
        .unwrap()
        .iter()
        .map(|(name, variant_id)| {
            format!(
                "{name}: {:?}, ty: {:?}",
                variant_id.debug(db),
                db.variant_semantic(enum_id, *variant_id).unwrap().ty.debug(db)
            )
        })
        .collect::<Vec<_>>()
        .join(",\n");
    assert_eq!(
        actual,
        indoc! {"
            a: VariantId(test::a), ty: (),
            b: VariantId(test::b), ty: (core::felt, core::felt),
            c: VariantId(test::c), ty: ()"}
    );
}
