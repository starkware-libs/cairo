use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{GenericTypeId, MacroPluginLongId, ModuleId, TopLevelLanguageElementId};
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::ids::AnalyzerPluginLongId;
use crate::items::us::SemanticUseEx;
use crate::plugin::AnalyzerPlugin;
use crate::resolve::ResolvedGenericItem;
use crate::test_utils::{
    SemanticDatabaseForTesting, get_crate_semantic_diagnostics, setup_test_crate,
    test_expr_diagnostics,
};

cairo_lang_test_utils::test_file_test!(
    diagnostics,
    "src/diagnostic_test_data",
    {
        allow_attr: "allow_attr",
        deref: "deref",
        tests: "tests",
        not_found: "not_found",
        missing: "missing",
        neg_impl: "neg_impl",
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
            error[E0005]: Module file not found. Expected path: abc.cairo
             --> lib.cairo:3:9
                    mod abc;
                    ^^^^^^^^

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
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        match item_ast {
            ast::ModuleItem::FreeFunction(func) if func.has_attr(db, "test_change_return_type") => {
                let mut builder = PatchBuilder::new(db, &func);
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
                    &[("func".to_string(), new_func)].into(),
                ));

                let (content, code_mappings) = builder.build();
                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "virt2".into(),
                        content,
                        code_mappings,
                        aux_data: None,
                        diagnostics_note: Default::default(),
                    }),
                    diagnostics: vec![],
                    remove_original_item: false,
                }
            }
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec!["test_change_return_type".to_string()]
    }
}

#[test]
fn test_inline_module_diagnostics() {
    let mut db_val = SemanticDatabaseForTesting::new_empty();
    let db = &mut db_val;
    db.set_default_macro_plugins(Arc::new([
        db.intern_macro_plugin(MacroPluginLongId(Arc::new(AddInlineModuleDummyPlugin)))
    ]));
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
                           ^^^^^^^^^

            error: Unexpected return type. Expected: "test::a::inner_mod::NewType", found: "core::felt252".
             --> lib.cairo:4:16
                    return 5_felt252;
                           ^^^^^^^^^

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
                           ^^^^^^^^^

            error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt252".
             --> lib.cairo:9:20
                        return 2_felt252;
                               ^^^^^^^^^

    "#},
    );
}

#[derive(Debug)]
struct NoU128RenameAnalyzerPlugin;
impl AnalyzerPlugin for NoU128RenameAnalyzerPlugin {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        let Ok(uses) = db.module_uses_ids(module_id) else {
            return diagnostics;
        };
        for use_id in uses.iter() {
            let Ok(ResolvedGenericItem::GenericType(GenericTypeId::Extern(ty))) =
                db.use_resolved_item(*use_id)
            else {
                continue;
            };
            if ty.full_path(db.upcast()) == "core::integer::u128" {
                diagnostics.push(PluginDiagnostic::error(
                    use_id.stable_ptr(db.upcast()).untyped(),
                    "Use items for u128 disallowed.".to_string(),
                ));
            }
        }
        diagnostics
    }

    fn declared_allows(&self) -> Vec<String> {
        vec!["u128_rename".to_string()]
    }
}

#[test]
fn test_analyzer_diagnostics() {
    let mut db_val = SemanticDatabaseForTesting::new_empty();
    let db = &mut db_val;
    db.set_default_analyzer_plugins(Arc::new([
        db.intern_analyzer_plugin(AnalyzerPluginLongId(Arc::new(NoU128RenameAnalyzerPlugin)))
    ]));
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod inner {
                use core::integer::u128 as long_u128_rename;
                use u128 as short_u128_rename;
                use core::integer::u64 as long_u64_rename;
                use u64 as short_u64_rename;
            }
            use core::integer::u128 as long_u128_rename;
            use u128 as short_u128_rename;
            use inner::long_u128_rename as additional_u128_rename;
            #[allow(u128_rename)]
            use core::integer::u64 as long_u64_rename;
            use u64 as short_u64_rename;
            use inner::long_u64_rename as additional_u64_rename;
       "},
    );

    assert_eq!(
        get_crate_semantic_diagnostics(db, crate_id).format(db),
        indoc! {r#"
        error: Plugin diagnostic: Use items for u128 disallowed.
         --> lib.cairo:7:20
        use core::integer::u128 as long_u128_rename;
                           ^^^^^^^^^^^^^^^^^^^^^^^^

        error: Plugin diagnostic: Use items for u128 disallowed.
         --> lib.cairo:8:5
        use u128 as short_u128_rename;
            ^^^^^^^^^^^^^^^^^^^^^^^^^

        error: Plugin diagnostic: Use items for u128 disallowed.
         --> lib.cairo:9:12
        use inner::long_u128_rename as additional_u128_rename;
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        error: Plugin diagnostic: Use items for u128 disallowed.
         --> lib.cairo:2:24
            use core::integer::u128 as long_u128_rename;
                               ^^^^^^^^^^^^^^^^^^^^^^^^

        error: Plugin diagnostic: Use items for u128 disallowed.
         --> lib.cairo:3:9
            use u128 as short_u128_rename;
                ^^^^^^^^^^^^^^^^^^^^^^^^^

    "#},
    );
}
