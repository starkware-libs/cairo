use std::sync::Arc;

use defs::ids::{GenericFunctionId, ModuleId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::ids::{CrateLongId, FileLongId, VirtualFile};

use crate::corelib::core_config;
use crate::db::SemanticGroup;
use crate::{semantic, Diagnostic, ExprId};

/// Sets up a module with given content, and returns its module id.
pub fn setup_test_module(db: &mut dyn SemanticGroup, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: Arc::new(content.to_string()),
    }));
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
    let function_code = format!("func test_func() {{ {function_body} {{\n{expr_code}\n}} }}");
    let (module_id, function_semantic) =
        setup_test_function(db, &function_code, "test_func", module_code).propagate(diagnostics);
    // Fetch the last block, which contains expr_code.
    let expr_block = match db.lookup_intern_expr(function_semantic.body) {
        semantic::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };
    let expr = match db.lookup_intern_expr(expr_block) {
        semantic::Expr::ExprBlock(block) => {
            assert!(
                block.statements.is_empty(),
                "expr_code is not a valid expression. Consider using setup_test_block()."
            );
            block.tail.unwrap()
        }
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
