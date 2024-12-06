use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_utils::extract_matches;
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::{PluginSuiteInput, SemanticGroup};
use crate::inline_macros::get_default_plugin_suite;
use crate::test_utils::{SemanticDatabaseForTesting, TestModule};

#[test]
fn test_struct() {
    let db = &mut SemanticDatabaseForTesting::default();

    let test_module_builder = TestModule::builder(
        db,
        indoc::indoc! {"
            #[inline(MyImpl1, MyImpl2)]
            struct A {
                a: felt252,
                pub b: (felt252, felt252),
                pub(crate) c: (),
                a: (),
                a: ()
            }

            fn foo(a: A) {
                5;
            }
        "},
        None,
    );

    let crate_id = unsafe { test_module_builder.get_crate_id() };
    db.set_crate_plugins_from_suite(crate_id, get_default_plugin_suite());

    let (test_module, diagnostics) =
        test_module_builder.build_and_check_for_diagnostics(db).split();

    assert_eq!(diagnostics, indoc! {r#"
        error: Redefinition of member "a" on struct "test::A".
         --> lib.cairo:6:5
            a: (),
            ^***^

        error: Redefinition of member "a" on struct "test::A".
         --> lib.cairo:7:5
            a: ()
            ^***^

        "#});
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
    assert_eq!(actual, indoc! {"
            a: Member { id: MemberId(test::a), ty: (), visibility: Private },
            b: Member { id: MemberId(test::b), ty: (core::felt252, core::felt252), visibility: Public },
            c: Member { id: MemberId(test::c), ty: (), visibility: PublicInCrate }"});

    assert_eq!(
        db.struct_attributes(struct_id)
            .unwrap()
            .iter()
            .map(|attr| format!("{:?}", attr.debug(db)))
            .collect::<Vec<_>>()
            .join(",\n"),
        r#"Attribute { id: "inline", args: ["MyImpl1", "MyImpl2", ] }"#
    );
}
