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
fn test_struct() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let (test_module, diagnostics) = setup_test_module(
        db,
        indoc::indoc! {"
            #[contract(MyImpl1, MyImpl2)]
            struct A {
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
        error: Redefinition of member "a" on struct "test::A".
         --> lib.cairo:6:5
            a: (),
            ^***^

        error: Redefinition of member "a" on struct "test::A".
         --> lib.cairo:7:5
            a: ()
            ^***^

        "#}
    );
    let module_id = test_module.module_id;

    let struct_id = extract_matches!(
        db.module_item_by_name(module_id, "A".into()).unwrap().unwrap(),
        ModuleItemId::Struct
    );
    let actual = db
        .struct_members(struct_id)
        .unwrap()
        .iter()
        .map(|(name, member)| format!("{name}: {:?}", member.debug(db)))
        .collect::<Vec<_>>()
        .join(",\n");
    assert_eq!(
        actual,
        indoc! {"
            a: Member { id: MemberId(test::a), ty: () },
            b: Member { id: MemberId(test::b), ty: (core::felt, core::felt) },
            c: Member { id: MemberId(test::c), ty: () }"}
    );

    assert_eq!(
        db.struct_attributes(struct_id)
            .unwrap()
            .iter()
            .map(|attr| format!("{:?}", attr.debug(db)))
            .collect::<Vec<_>>()
            .join(",\n"),
        r#"Attribute { id: "contract", args: ["MyImpl1", "MyImpl2", ] }"#
    );
}
