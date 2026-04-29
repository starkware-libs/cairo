use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_test_utils::test;
use cairo_lang_utils::{Intern, extract_matches};
use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::TypeLongId;
use crate::items::module::ModuleSemantic;
use crate::items::structure::{StructSemantic, TypeMemberKind};
use crate::items::visibility::Visibility;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};
use crate::types::{ConcreteStructLongId, ConcreteTypeId};

#[test]
fn test_struct() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let (test_module, diagnostics) = setup_test_module(
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
    )
    .split();
    assert_eq!(
        diagnostics,
        indoc! {r#"
        error[E2050]: Redefinition of member "a" on struct "A".
         --> lib.cairo:6:5
            a: (),
            ^^^^^

        error[E2050]: Redefinition of member "a" on struct "A".
         --> lib.cairo:7:5
            a: ()
            ^^^^^

        "#}
    );
    let module_id = test_module.module_id;

    let struct_id = extract_matches!(
        db.module_item_by_name(module_id, SmolStrId::from(db, "A")).unwrap().unwrap(),
        ModuleItemId::Struct
    );
    let actual = db
        .struct_members(struct_id)
        .unwrap()
        .iter()
        .map(|(name, member)| format!("{}: {:?}", name.long(db), member.debug(db)))
        .collect::<Vec<_>>()
        .join(",\n");
    assert_eq!(
        actual,
        indoc! {"
            a: Member { id: MemberId(test::A::a), ty: (), visibility: Private },
            b: Member { id: MemberId(test::A::b), ty: (core::felt252, core::felt252), visibility: Public },
            c: Member { id: MemberId(test::A::c), ty: (), visibility: PublicInCrate }"}
    );

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

#[test]
fn test_tuple_struct() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let (test_module, diagnostics) = setup_test_module(
        db,
        indoc! {"
            struct Point(pub u32, u64)
        "},
    )
    .split();
    assert_eq!(diagnostics, "");
    let module_id = test_module.module_id;

    let struct_id = extract_matches!(
        db.module_item_by_name(module_id, SmolStrId::from(db, "Point")).unwrap().unwrap(),
        ModuleItemId::Struct
    );

    // Tuple structs have no named members.
    assert!(db.struct_members(struct_id).unwrap().is_empty());

    // Positional types are correctly extracted.
    let positional_tys = db.struct_positional_types(struct_id).unwrap().unwrap();
    assert_eq!(positional_tys.len(), 2);
    assert_eq!(positional_tys[0].1, Visibility::Public);
    assert_eq!(positional_tys[1].1, Visibility::Private);
    assert_eq!(format!("{:?}", positional_tys[0].0.debug(db)), "core::integer::u32");
    assert_eq!(format!("{:?}", positional_tys[1].0.debug(db)), "core::integer::u64");

    // type_members returns TupleElement kinds for tuple structs.
    let concrete_struct_id = ConcreteStructLongId { struct_id, generic_args: vec![] }.intern(db);
    let ty = TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)).intern(db);
    let members = db.type_members(ty).unwrap().unwrap();
    assert_eq!(members.len(), 2);
    let m0 = members.get(&SmolStrId::from(db, "0")).unwrap();
    let m1 = members.get(&SmolStrId::from(db, "1")).unwrap();
    assert!(matches!(m0.kind, TypeMemberKind::TupleElement(0)));
    assert!(matches!(m1.kind, TypeMemberKind::TupleElement(1)));
    assert_eq!(m0.visibility, Visibility::Public);
    assert_eq!(m1.visibility, Visibility::Private);
}
