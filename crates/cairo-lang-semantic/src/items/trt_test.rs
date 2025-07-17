use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_utils::Intern;
use cairo_lang_utils::extract_matches;
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use test_log::test;

use crate::db::SemanticGroup;
use crate::test_utils::{SemanticDatabaseForTesting, setup_test_module};

#[test]
fn test_trait() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let test_module = setup_test_module(
        db,
        indoc::indoc! {"
            // `inline` is used just to have an allowed attribute.
            #[inline]
            trait MyContract {
                fn foo(a: felt252);
            }
        "},
    )
    .unwrap();

    let trait_id = extract_matches!(
        db.module_item_by_name(test_module.module_id, SmolStr::from("MyContract").intern(db))
            .unwrap()
            .unwrap(),
        ModuleItemId::Trait
    );

    let generic_params = db.trait_generic_params(trait_id).unwrap();
    let debug = generic_params.debug(db);
    assert_eq!(format!("{:?}", debug), "[]");
    let trait_atts = db.trait_attributes(trait_id).unwrap();
    test_trait_atts(db, &trait_atts);

    let trait_functions = db.trait_functions(trait_id).unwrap();
    let trait_function_id = trait_functions.get(&SmolStr::from("foo").intern(db)).unwrap();
    let signature = db.trait_function_signature(*trait_function_id).unwrap();
    assert_eq!(
        format!("{:?}", signature.debug(db)),
        "Signature { params: [Parameter { id: ParamId(test::a), name: \"a\", ty: core::felt252, \
         mutability: Immutable }], return_type: (), implicits: [], panicable: true, is_const: \
         false }"
    );
}

fn test_trait_atts<'me, 'db>(db: &'db dyn SyntaxGroup, trait_atts: &'me Vec<Attribute<'db>>) {
    let debug: cairo_lang_debug::debug::DebugWith<'me, 'db, dyn SyntaxGroup> = trait_atts.debug(db);
    assert_eq!(format!("{:?}", debug), "[Attribute { id: \"inline\" }]");
}
