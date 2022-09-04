use std::sync::Arc;

use defs::ids::{GenericFunctionId, HasName};
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};
use smol_str::SmolStr;

use crate::corelib::{core_config, core_module};
use crate::db::SemanticGroup;
use crate::{semantic, ExprId, StatementId, TypeId};

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

pub fn assert_unit_type(db: &dyn SemanticGroup, ty: TypeId) {
    match db.lookup_intern_type(ty) {
        crate::TypeLongId::Tuple(items) if items.is_empty() => {}
        variant => panic!("Expected unit type, got {variant:?}"),
    }
}
pub fn assert_felt_type(db: &dyn SemanticGroup, ty: TypeId) {
    match db.lookup_intern_type(ty) {
        crate::TypeLongId::Concrete(ty) => {
            assert!(ty.generic_args.is_empty());
            if let defs::ids::GenericTypeId::Extern(extern_type_id) = ty.generic_type {
                assert_eq!(extern_type_id.name(db.as_defs_group()), "felt");
                assert_eq!(extern_type_id.module(db.as_defs_group()), core_module(db));
            } else {
                panic!("Expected extern type");
            }
        }
        variant => panic!("Expected concrete type, got {variant:?}"),
    }
}

pub fn assert_let_statement_with_literal(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
    literal_value: usize,
) {
    let rhs = assert_let_statement_lhs_and_get_rhs(db, statement_id, module_id, var_name);
    if let semantic::Expr::ExprLiteral(literal) = db.lookup_intern_expr(rhs) {
        assert_eq!(literal.value, literal_value);
        assert_felt_type(db, literal.ty);
    } else {
        panic!("Expected a literal expression");
    }
}

pub fn assert_let_statement_with_var(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
    expr_var_name: SmolStr,
) {
    let rhs = assert_let_statement_lhs_and_get_rhs(db, statement_id, module_id, var_name);
    if let semantic::Expr::ExprVar(var) = db.lookup_intern_expr(rhs) {
        assert_eq!(var.var.name(db.as_defs_group()), expr_var_name);
        // TODO(yuval): assert type after real types are returned.
        assert_unit_type(db, var.ty);
    } else {
        panic!("Expected a var expression");
    }
}

fn assert_let_statement_lhs_and_get_rhs(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
) -> ExprId {
    let stmt = db.lookup_intern_statement(statement_id);
    let let_stmt = if let semantic::Statement::Let(let_stmt) = stmt {
        let_stmt
    } else {
        panic!("Expected a let statement")
    };
    assert_eq!(let_stmt.var.id.module(db.as_defs_group()), module_id);
    assert_eq!(let_stmt.var.id.name(db.as_defs_group()), var_name);
    assert_felt_type(db, let_stmt.var.ty);

    let_stmt.expr
}
