use std::sync::Arc;

use defs::ids::GenericFunctionId;
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};

use crate::corelib::core_config;
use crate::db::SemanticGroup;
use crate::{semantic, ExprId};

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

pub fn setup_test_function(
    db: &mut dyn SemanticGroup,
    module_code: &str,
    function_name: &str,
    function_code: &str,
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

pub fn setup_test_expr(
    db: &mut dyn SemanticGroup,
    module_code: &str,
    function_body: &str,
    expr_code: &str,
) -> (ModuleId, ExprId) {
    let function_code = format!("func test_func() {{ {function_body} {expr_code} }}");
    let (module_id, function_semantic) =
        setup_test_function(db, module_code, "test_func", &function_code);
    let expr = match db.lookup_intern_expr(function_semantic.body) {
        semantic::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };
    (module_id, expr)
}
