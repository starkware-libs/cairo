use std::sync::Arc;

use defs::db::DefsGroup;
use defs::ids::ModuleId;
use defs::plugin::{
    DiagnosticMapper, DynDiagnosticMapper, MacroPlugin, PluginGeneratedFile,
    PluginMappedDiagnostic, PluginResult,
};
use indoc::indoc;
use pretty_assertions::assert_eq;
use syntax::node::ast;
use syntax::node::db::SyntaxGroup;
use test_log::test;

use crate::db::SemanticGroup;
use crate::patcher::{PatchBuilder, Patches, RewriteNode};
use crate::test_utils::{setup_test_crate, test_expr_diagnostics, SemanticDatabaseForTesting};
use crate::{semantic_test, SemanticDiagnostic};

semantic_test!(
    diagnostics_tests,
    [
        "src/diagnostic_test_data/tests",
        "src/diagnostic_test_data/not_found",
        "src/diagnostic_test_data/missing"
    ],
    test_expr_diagnostics
);

#[test]
fn test_missing_module_file() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = setup_test_crate(
        db,
        "
    mod a {
        mod abc;
    }",
    );

    let submodule_id =
        *db.module_submodules(ModuleId::CrateRoot(crate_id)).unwrap().first().unwrap();

    assert_eq!(
        db.module_semantic_diagnostics(submodule_id).unwrap().format(db),
        indoc! {"
            error: Module file not found. Expected path: src/a/abc.cairo
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
struct AddInlineModuleDummyPlugin {}

impl MacroPlugin for AddInlineModuleDummyPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: syntax::node::ast::Item,
    ) -> PluginResult {
        match item_ast {
            ast::Item::FreeFunction(func) => {
                let mut builder = PatchBuilder::new(db);
                let mut new_func = RewriteNode::from_ast(&func);
                if matches!(
                    func.signature(db).ret_ty(db),
                    ast::OptionReturnTypeClause::ReturnTypeClause(_)
                ) {
                    new_func
                        .modify_child(db, ast::ItemFreeFunction::INDEX_SIGNATURE)
                        .modify_child(db, ast::FunctionSignature::INDEX_RET_TY)
                        .modify_child(db, ast::ReturnTypeClause::INDEX_TY)
                        .set_str("NewType".into());
                }
                builder.interpolate_patched(
                    indoc! {"
                        extern type NewType;
                        mod inner_mod {{
                            // Comment.
                            // Comment.
                            // Comment.
                            $func$
                        }}
                    "},
                    [("func".to_string(), new_func)].into(),
                );

                PluginResult {
                    code: Some(PluginGeneratedFile {
                        name: "virt2".into(),
                        content: builder.code,
                        diagnostic_mapper: DynDiagnosticMapper::new(PatchMapper {
                            patches: builder.patches,
                        }),
                    }),
                    diagnostics: vec![],
                }
            }
            _ => PluginResult { code: None, diagnostics: vec![] },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatchMapper {
    patches: Patches,
}
impl DiagnosticMapper for PatchMapper {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn map_diag(
        &self,
        db: &dyn DefsGroup,
        diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        let Some(diag) = diag.downcast_ref::<SemanticDiagnostic>() else {return None;};
        let span = self.patches.translate(db, diag.stable_location.diagnostic_location(db).span)?;
        Some(PluginMappedDiagnostic { span, message: "Mapped error.".into() })
    }

    fn eq(&self, other: &dyn DiagnosticMapper) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}

#[test]
fn test_inline_module_diagnostics() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    db.set_macro_plugins(vec![Arc::new(AddInlineModuleDummyPlugin {})]);
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod a {
                fn bad() -> u128 {
                    return 5;
                }
            }
       "},
    );

    let submodules = db.module_submodules(ModuleId::CrateRoot(crate_id)).unwrap();
    let submodule_id = submodules.first().unwrap();

    assert_eq!(
        db.module_semantic_diagnostics(*submodule_id).unwrap().format(db),
        indoc! {r#"
            error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt".
             --> lib.cairo:3:16
                    return 5;
                           ^

            "#},
    );

    // Test diagnostics within a generated inline module.
    let submodule_submodules = db.module_submodules(*submodule_id).unwrap();
    let subsubmodule_id = submodule_submodules.first().unwrap();

    assert_eq!(
        db.module_semantic_diagnostics(*subsubmodule_id).unwrap().format(db),
        indoc! {r#"
            error: Plugin diagnostic: Mapped error.
             --> lib.cairo:3:16
                    return 5;
                           ^

            "#},
    );
}
