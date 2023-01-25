use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, GeneratedFileAuxData, MacroPlugin, PluginGeneratedFile, PluginResult,
};
use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal};
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::db::SemanticGroup;
use crate::patcher::{PatchBuilder, Patches, RewriteNode};
use crate::plugin::{
    AsDynGeneratedFileAuxData, AsDynMacroPlugin, DiagnosticMapper, DynDiagnosticMapper,
    PluginMappedDiagnostic, SemanticPlugin,
};
use crate::test_utils::{
    get_crate_semantic_diagnostics, setup_test_crate, test_expr_diagnostics,
    SemanticDatabaseForTesting,
};
use crate::SemanticDiagnostic;

cairo_lang_test_utils::test_file_test!(
    diagnostics,
    "src/diagnostic_test_data",
    {
        tests: "tests",
        not_found: "not_found",
        missing: "missing",
    },
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
        *db.module_submodules_ids(ModuleId::CrateRoot(crate_id)).unwrap().first().unwrap();

    assert_eq!(
        db.module_semantic_diagnostics(ModuleId::Submodule(submodule_id)).unwrap().format(db),
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
            ast::Item::FreeFunction(func)
                if func
                    .attributes(db)
                    .elements(db)
                    .iter()
                    .any(|attr| attr.attr(db).text(db) == "test_change_return_type") =>
            {
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
                        aux_data: DynGeneratedFileAuxData::new(DynDiagnosticMapper::new(
                            PatchMapper { patches: builder.patches },
                        )),
                    }),
                    diagnostics: vec![],
                    remove_original_item: false,
                }
            }
            _ => PluginResult::default(),
        }
    }
}
impl AsDynMacroPlugin for AddInlineModuleDummyPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for AddInlineModuleDummyPlugin {}

#[derive(Debug, PartialEq, Eq)]
pub struct PatchMapper {
    patches: Patches,
}
impl GeneratedFileAuxData for PatchMapper {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
impl AsDynGeneratedFileAuxData for PatchMapper {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static) {
        self
    }
}
impl DiagnosticMapper for PatchMapper {
    fn map_diag(
        &self,
        db: &(dyn SemanticGroup + 'static),
        diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        let Some(diag) = diag.downcast_ref::<SemanticDiagnostic>() else {return None;};
        let span = self
            .patches
            .translate(db.upcast(), diag.stable_location.diagnostic_location(db.upcast()).span)?;
        Some(PluginMappedDiagnostic { span, message: format!("Mapped error. {}", diag.format(db)) })
    }
}

#[test]
fn test_inline_module_diagnostics() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    db.set_semantic_plugins(vec![Arc::new(AddInlineModuleDummyPlugin {})]);
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod a {
                #[test_change_return_type]
                fn bad() -> u128 {
                    return 5;
                }
            }
       "},
    );

    // Verify we get diagnostics both for the original and the generated code.
    assert_eq!(
        get_crate_semantic_diagnostics(db, crate_id).format(db),
        indoc! {r#"
            error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt".
             --> lib.cairo:4:16
                    return 5;
                           ^

            error: Plugin diagnostic: Mapped error. Unexpected return type. Expected: "test::a::inner_mod::NewType", found: "core::felt".
             --> lib.cairo:4:16
                    return 5;
                           ^

            "#},
    );
}

#[test]
fn test_inline_inline_module_diagnostics() {
    let mut db_val = SemanticDatabaseForTesting::default();
    let db = &mut db_val;
    let crate_id = setup_test_crate(
        db,
        indoc! {"
            mod a {
                fn bad_a() -> u128 {
                    return 1;
                }
            }
            mod b {
                mod c {
                    fn bad_c() -> u128 {
                        return 2;
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
        indoc! {r#"error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt".
             --> lib.cairo:3:16
                    return 1;
                           ^

            error: Unexpected return type. Expected: "core::integer::u128", found: "core::felt".
             --> lib.cairo:9:20
                        return 2;
                               ^

    "#},
    );
}
