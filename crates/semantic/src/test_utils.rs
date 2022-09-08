use std::collections::HashMap;
use std::sync::Arc;

use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{GenericFunctionId, ModuleId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::{init_files_group, AsFilesGroup, FilesDatabase, FilesGroup};
use filesystem::ids::{CrateLongId, FileLongId};
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::corelib::core_config;
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::{semantic, Diagnostic, ExprId};

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
pub fn setup_test_module(db: &mut dyn SemanticGroup, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::OnDisk("test.cairo".into()));
    db.set_file_overrides(Arc::new(HashMap::from_iter([(file_id, Arc::new(content.to_string()))])));
    db.set_project_config(core_config(db).with_crate(crate_id, file_id));

    ModuleId::CrateRoot(crate_id)
}

/// Returns the semantic model of a given function.
/// function_name - name of the function.
/// module_code - extra setup code in the module context.
#[with_diagnostics]
pub fn setup_test_function(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &mut dyn SemanticGroup,
    function_code: &str,
    function_name: &str,
    module_code: &str,
) -> (ModuleId, semantic::FreeFunction) {
    let content = format!("{module_code} {function_code}");
    let module_id = setup_test_module(db, &content);
    let generic_function_id = db
        .module_item_by_name(module_id, function_name.into())
        .propagate(diagnostics)
        .and_then(GenericFunctionId::from)
        .unwrap();
    let function_id = match generic_function_id {
        GenericFunctionId::Free(function_id) => function_id,
        _ => panic!(),
    };
    (module_id, db.free_function_semantic(function_id).propagate(diagnostics).unwrap())
}

/// Returns the semantic model of a given expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
#[with_diagnostics]
pub fn setup_test_expr(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &mut dyn SemanticGroup,
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> (ModuleId, ExprId) {
    let function_code = format!("func test_func() {{ {function_body} {expr_code} }}");
    let (module_id, function_semantic) =
        setup_test_function(db, &function_code, "test_func", module_code).propagate(diagnostics);
    let expr = match db.lookup_intern_expr(function_semantic.body) {
        semantic::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };
    (module_id, expr)
}

/// Returns the semantic model of a given block expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
#[with_diagnostics]
pub fn setup_test_block(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &mut dyn SemanticGroup,
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> (ModuleId, ExprId) {
    setup_test_expr(db, &format!("{{ {expr_code} }}"), module_code, function_body)
        .propagate(diagnostics)
}
