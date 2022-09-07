use std::sync::Arc;

use defs::ids::GenericFunctionId;
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};

use crate::corelib::core_config;
use crate::db::SemanticGroup;
use crate::{semantic, ExprId};

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
pub fn setup_test_function(
    db: &mut dyn SemanticGroup,
    function_code: &str,
    function_name: &str,
    module_code: &str,
) -> (ModuleId, semantic::FreeFunction) {
    let content = format!("{module_code} {function_code}");
    let module_id = setup_test_module(db, &content);
    let generic_function_id =
        db.module_resolve_generic_function(module_id, function_name.into()).expect("").unwrap();
    let function_id = match generic_function_id {
        GenericFunctionId::Free(function_id) => function_id,
        _ => panic!(),
    };
    (module_id, db.free_function_semantic(function_id).expect("").unwrap())
}

/// Returns the semantic model of a given expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
pub fn setup_test_expr(
    db: &mut dyn SemanticGroup,
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> (ModuleId, ExprId) {
    let function_code = format!("func test_func() {{ {function_body} {expr_code} }}");
    let (module_id, function_semantic) =
        setup_test_function(db, &function_code, "test_func", module_code);
    let expr = match db.lookup_intern_expr(function_semantic.body) {
        semantic::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };
    (module_id, expr)
}

/// Returns the semantic model of a given block expression.
/// module_code - extra setup code in the module context.
/// function_body - extra setup code in the function context.
pub fn setup_test_block(
    db: &mut dyn SemanticGroup,
    expr_code: &str,
    module_code: &str,
    function_body: &str,
) -> (ModuleId, ExprId) {
    setup_test_expr(db, &format!("{{ {expr_code} }}"), module_code, function_body)
}
