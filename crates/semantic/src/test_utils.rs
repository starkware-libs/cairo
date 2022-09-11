use std::sync::Arc;

use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{GenericFunctionId, ModuleId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup, FilesGroupEx};
use filesystem::ids::{CrateLongId, Directory};
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};
use utils::extract_matches;

use crate::corelib::core_config;
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::{semantic, ExprBlock, ExprId, SemanticDiagnostic};

#[salsa::database(SemanticDatabase, DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct SemanticDatabaseForTesting {
    storage: salsa::Storage<SemanticDatabaseForTesting>,
}
impl salsa::Database for SemanticDatabaseForTesting {}
impl Default for SemanticDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}
impl AsFilesGroup for SemanticDatabaseForTesting {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
    fn as_files_group_mut(&mut self) -> &mut (dyn FilesGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for SemanticDatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl AsDefsGroup for SemanticDatabaseForTesting {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}

/// Sets up a module with given content, and returns its module id.
pub fn setup_test_module(
    db: &mut (dyn SemanticGroup + 'static),
    content: &str,
) -> (ModuleId, String) {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let directory = Directory("src".into());
    db.set_project_config(core_config(db).with_crate(crate_id, directory));
    let file_id = db.module_file(ModuleId::CrateRoot(crate_id)).unwrap();
    db.as_files_group_mut().override_file_content(file_id, Some(Arc::new(content.to_string())));
    let syntax_diagnostics = db.file_syntax_diagnostics(file_id).format(db.as_files_group());

    (ModuleId::CrateRoot(crate_id), syntax_diagnostics)
}
/// Returns the semantic model of a given function.
/// function_name - name of the function.
/// module_code - extra setup code in the module context.
#[with_diagnostics]
pub fn setup_test_function(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &mut (dyn SemanticGroup + 'static),
    function_code: &str,
    function_name: &str,
    module_code: &str,
) -> (ModuleId, semantic::FreeFunction, String) {
    let content = format!("{module_code} {function_code}");
    let (module_id, syntax_diagnostics) = setup_test_module(db, &content);
    let generic_function_id = db
        .module_item_by_name(module_id, function_name.into())
        .and_then(GenericFunctionId::from)
        .unwrap();
    let function_id = extract_matches!(generic_function_id, GenericFunctionId::Free);
    (
        module_id,
        db.free_function_semantic(function_id).propagate(diagnostics).unwrap(),
        syntax_diagnostics,
    )
}

/// Returns the semantic model of a given expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
#[with_diagnostics]
pub fn setup_test_expr(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &mut (dyn SemanticGroup + 'static),
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> (ModuleId, ExprId, String) {
    let function_code = format!("func test_func() {{ {function_body} {{\n{expr_code}\n}} }}");
    let (module_id, function_semantic, syntax_diagnostics) =
        setup_test_function(db, &function_code, "test_func", module_code).propagate(diagnostics);
    let ExprBlock { tail: function_body_tail, .. } =
        extract_matches!(db.lookup_intern_expr(function_semantic.body), semantic::Expr::ExprBlock);
    let ExprBlock { statements, tail, .. } = extract_matches!(
        db.lookup_intern_expr(function_body_tail.unwrap()),
        semantic::Expr::ExprBlock
    );
    assert!(
        statements.is_empty(),
        "expr_code is not a valid expression. Consider using setup_test_block()."
    );
    (module_id, tail.unwrap(), syntax_diagnostics)
}

/// Returns the semantic model of a given block expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
#[with_diagnostics]
pub fn setup_test_block(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    db: &mut (dyn SemanticGroup + 'static),
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> (ModuleId, ExprId, String) {
    setup_test_expr(db, &format!("{{ {expr_code} }}"), module_code, function_body)
        .propagate(diagnostics)
}
