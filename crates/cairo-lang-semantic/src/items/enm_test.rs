use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_test_utils::test;
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::items::enm::EnumSemantic;
use crate::items::module::ModuleSemantic;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_enum() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let (test_module, diagnostics) = setup_test_module(
        db,
        indoc::indoc! {"
            enum A {
                a: felt252,
                b: (felt252, felt252),
                c: (),
                a: (),
                a: ()
            }

            fn foo(a: A) {
                5;
            }
        "},
    )
    .split();
    assert_eq!(
        diagnostics,
        indoc! {r#"
        error[E2051]: Redefinition of variant "a" on enum "A".
         --> lib.cairo:5:5
            a: (),
            ^^^^^

        error[E2051]: Redefinition of variant "a" on enum "A".
         --> lib.cairo:6:5
            a: ()
            ^^^^^

        "#}
    );
    let module_id = test_module.module_id;

    let enum_id = extract_matches!(
        db.module_item_by_name(module_id, SmolStrId::from(db, "A")).unwrap().unwrap(),
        ModuleItemId::Enum
    );
    let actual = db
        .enum_variants(enum_id)
        .unwrap()
        .iter()
        .map(|(name, variant_id)| {
            format!(
                "{}: {:?}, ty: {:?}",
                name.long(db),
                variant_id.debug(db),
                db.variant_semantic(enum_id, *variant_id).unwrap().ty.debug(db)
            )
        })
        .collect::<Vec<_>>()
        .join(",\n");
    assert_eq!(
        actual,
        indoc! {"
            a: VariantId(test::A::a), ty: core::felt252,
            b: VariantId(test::A::b), ty: (core::felt252, core::felt252),
            c: VariantId(test::A::c), ty: ()"}
    );
}
