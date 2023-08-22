use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{MacroPlugin, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::test_utils::{
    get_crate_semantic_diagnostics, setup_test_crate, test_expr_diagnostics,
    SemanticDatabaseForTesting,
};

cairo_lang_test_utils::test_file_test!(
    diagnostics,
    "src/diagnostic_test_data",
    {
        tests: "tests",
        not_found: "not_found",
        missing: "missing",
        plus_eq: "plus_eq",
        inline: "inline",
    },
    test_expr_diagnostics
);

#[test]
fn test_missing_module_file() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let crate_id = setup_test_crate(
        db,
        "
    mod a {
        mod abc;
    }",
    );

    let submodule_id =
        *db.module_submodules_ids(ModuleId::CrateRoot(crate_id)).unwrap().first().unwrap();

    assert_eq!(
        db.module_semantic_diagnostics(ModuleId::Submodule(submodule_id)).unwrap().format(db),
        indoc! {"
            error: Module file not found. Expected path: abc.cairo
             --> lib.cairo:3:9
                    mod abc;
                    ^******^

            "
        },
    );
}

// A dummy plugin that adds an inline module with a semantic error (per function
// in the original module).
// Used to test error location inside plugin generated inline modules.
#[derive(Debug)]
struct AddInlineModuleDummyPlugin;

impl MacroPlugin for AddInlineModuleDummyPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::FreeFunction(func) if func.has_attr(db, "test_change_return_type") => {
                let mut builder = PatchBuilder::new(db);
                let mut new_func = RewriteNode::from_ast(&func);
                if matches!(
                    func.declaration(db).signature(db).ret_ty(db),
                    ast::OptionReturnTypeClause::ReturnTypeClause(_)
                ) {
                    // Change the return type.
                    new_func
                        .modify_child(db, ast::FunctionWithBody::INDEX_DECLARATION)
                        .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
                        .modify_child(db, ast::FunctionSignature::INDEX_RET_TY)
                        .modify_child(db, ast::ReturnTypeClause::INDEX_TY)
                        .set_str("NewType".into());
                    // Remove the attribute.
                    new_func
                        .modify_child(db, ast::FunctionWithBody::INDEX_ATTRIBUTES)
                        .modify(db)
                        .children
                        .as_mut()
                        .unwrap()
                        .remove(0);
                }
                builder.add_modified(RewriteNode::interpolate_patched(
                    indoc! {"
                        mod inner_mod {{
                            extern type NewType;
                            // Comment 1.
                            // Comment $$.
                            $func$
                        }}
                    "},
                    [("func".to_string(), new_func)].into(),
                ));

                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "virt2".into(),
                        content: builder.code,
                        diagnostics_mappings: builder.diagnostics_mappings,
                        aux_data: None,
                    }),
                    diagnostics: vec![],
                    remove_original_item: false,
                }
            }
            _ => PluginResult::default(),
        }
    }
}

#[test]
fn test_inline_module_diagnostics() {
    let mut db_val = SemanticDatabaseForTesting::new_empty();
    let db = &mut db_val;
    db.set_macro_plugins(vec![Arc::new(AddInlineModuleDummyPlugin)]);
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod a {
                #[test_change_return_type]
                fn bad() -> u128 {
                    return 5_felt252;
                }
            }
       "},
    );

    // Verify we get diagnostics both for the original and the generated code.
    assert_eq!(
        get_crate_semantic_diagnostics(db, crate_id).format(db),
        indoc! {r#"
            error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt252".
             --> lib.cairo:4:16
                    return 5_felt252;
                           ^*******^

            error: Plugin diagnostic: Unexpected return type. Expected: "test::a::inner_mod::NewType", found: "core::felt252".
             --> lib.cairo:4:16
                    return 5_felt252;
                           ^*******^

            "#},
    );
}

#[test]
fn test_inline_inline_module_diagnostics() {
    let db_val = SemanticDatabaseForTesting::default();
    let db = &db_val;
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod a {
                fn bad_a() -> u128 {
                    return 1_felt252;
                }
            }
            mod b {
                mod c {
                    fn bad_c() -> u128 {
                        return 2_felt252;
                    }
                }
                mod d {
                    fn foo_d() {
                    }
                }
            }
            fn foo() {
                b::c::bad_c();
            }
       "},
    );

    assert_eq!(
        get_crate_semantic_diagnostics(db, crate_id).format(db),
        indoc! {r#"error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt252".
             --> lib.cairo:3:16
                    return 1_felt252;
                           ^*******^

            error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt252".
             --> lib.cairo:9:20
                        return 2_felt252;
                               ^*******^

    "#},
    );
}
