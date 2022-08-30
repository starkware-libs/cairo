use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{
    BlockLongId, CodeElementLongId, FreeFunctionLongId, FunctionWithBodyId, LocalVarId, ModuleId,
    VarId,
};
use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::CrateLongId;
use parser::db::ParserDatabase;
use pretty_assertions::assert_eq;
use salsa::{InternId, InternKey};
use semantic::db::{SemanticDatabase, SemanticGroup};
use semantic::ids::{ConcreteFunctionId, TypeId};
use semantic::ExprData;
use syntax::node::db::{AsGreenInterner, GreenDatabase, GreenInterner};

use crate::db::{SierraGenDatabase, SierraGenGroup};
use crate::expr_generator::generate_expression_code;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::pre_sierra;

#[salsa::database(
    DefsDatabase,
    SemanticDatabase,
    SierraGenDatabase,
    ParserDatabase,
    GreenDatabase,
    FilesDatabase
)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}
impl AsGreenInterner for DatabaseImpl {
    fn as_green_interner(&self) -> &(dyn GreenInterner + 'static) {
        self
    }
}
impl AsDefsGroup for DatabaseImpl {
    fn as_defs_group(&self) -> &(dyn defs::db::DefsGroup + 'static) {
        self
    }
}

/// Replaces `ConcreteLibFuncId` with a dummy `ConcreteLibFuncId` whose debug string is the string
/// representing the original `ConcreteLibFuncLongId`.
/// For example, while the original debug string may be `[6]`, the resulting debug string may be
/// `felt_const<2>`.
fn replace_libfunc_ids(
    db: &dyn SierraGenGroup,
    statement: &pre_sierra::Statement,
) -> pre_sierra::Statement {
    match statement {
        pre_sierra::Statement::SierraStatement(sierra::program::GenStatement::Invocation(p)) => {
            pre_sierra::Statement::SierraStatement(sierra::program::GenStatement::Invocation(
                sierra::program::GenInvocation {
                    libfunc_id: db
                        .lookup_intern_concrete_lib_func(p.libfunc_id.clone())
                        .to_string()
                        .into(),
                    ..p.clone()
                },
            ))
        }
        _ => statement.clone(),
    }
}

#[test]
fn test_expr_generator() {
    let (db, code_element) = prepare_test_data();

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprLiteral(semantic::ExprLiteral { value: 7, ty }),
        code_element,
    });
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprVar(semantic::ExprVar { var: VarId::Local(var_x), ty }),
        code_element,
    });

    // "let x = 7;" statement.
    let statement_let = semantic::StatementLet { var: var_x, expr: literal7 };

    // "foo(x, 7)" expression.
    let expr = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
            function: ConcreteFunctionId::from_intern_id(InternId::from(1u32)),
            args: vec![var_x_expr, literal7],
            ty,
        }),
        code_element,
    });

    // "foo(foo(x, 7), foo(x, 7))" expression.
    let expr2 = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
            function: ConcreteFunctionId::from_intern_id(InternId::from(2u32)),
            args: vec![expr, expr],
            ty,
        }),
        code_element,
    });

    // "let x = 7; foo(x, 7); foo(foo(x, 7), foo(x, 7))" block.
    let block = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprBlock(semantic::ExprBlock {
            statements: vec![
                semantic::Statement::Let(statement_let),
                semantic::Statement::Expr(expr),
            ],
            tail: Some(expr2),
            ty,
        }),
        code_element,
    });

    let mut expr_generator_context = ExprGeneratorContext::new(&db);
    let (statements, res) = generate_expression_code(&mut expr_generator_context, block);
    assert_eq!(
        statements.iter().map(|x| replace_libfunc_ids(&db, x).to_string()).collect::<Vec<String>>(),
        vec![
            // let x = 7;
            "felt_const<7>() -> ([0])",
            // foo(x, 7);
            "felt_const<7>() -> ([1])",
            "store_temp([0]) -> ([2])",
            "store_temp([1]) -> ([3])",
            "function_call([2], [3]) -> ([4])",
            // foo(foo(x, 7), foo(x, 7))
            "felt_const<7>() -> ([5])",
            "store_temp([0]) -> ([6])",
            "store_temp([5]) -> ([7])",
            "function_call([6], [7]) -> ([8])",
            "felt_const<7>() -> ([9])",
            "store_temp([0]) -> ([10])",
            "store_temp([9]) -> ([11])",
            "function_call([10], [11]) -> ([12])",
            "store_temp([8]) -> ([13])",
            "store_temp([12]) -> ([14])",
            "function_call([13], [14]) -> ([15])",
        ]
    );

    assert_eq!(res, sierra::ids::VarId::from(15));
}

#[test]
fn test_match() {
    let (db, code_element) = prepare_test_data();

    let ty = TypeId::from_intern_id(InternId::from(0u32));
    let literal7 = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprLiteral(semantic::ExprLiteral { value: 7, ty }),
        code_element,
    });
    let var_x = LocalVarId::from_intern_id(InternId::from(3u32));
    let var_x_expr = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprVar(semantic::ExprVar { var: VarId::Local(var_x), ty }),
        code_element,
    });

    let statement_let = semantic::StatementLet { var: var_x, expr: literal7 };

    let branch0 = semantic::MatchBranch {
        pattern: semantic::Pattern::Literal(semantic::ExprLiteral { value: 0, ty }),
        block: var_x_expr,
    };
    let branch_otherwise =
        semantic::MatchBranch { pattern: semantic::Pattern::Otherwise, block: literal7 };
    let match_statement = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprMatch(semantic::ExprMatch {
            matched_expr: var_x_expr,
            arms: vec![branch0, branch_otherwise],
            ty,
        }),
        code_element,
    });

    let block = db.intern_expr(ExprData {
        expr: semantic::Expr::ExprBlock(semantic::ExprBlock {
            statements: vec![semantic::Statement::Let(statement_let)],
            tail: Some(match_statement),
            ty,
        }),
        code_element,
    });

    let mut expr_generator_context = ExprGeneratorContext::new(&db);
    let (statements, res) = generate_expression_code(&mut expr_generator_context, block);
    assert_eq!(
        statements.iter().map(|x| replace_libfunc_ids(&db, x).to_string()).collect::<Vec<String>>(),
        vec![
            // let x = 7;
            "felt_const<7>() -> ([0])",
            // match {
            "jump_nz([0]) { label0() fallthrough() }",
            // Branch 0.
            "store_temp([0]) -> ([1])",
            "jump() { label1() }",
            // Branch otherwise.
            "label0:",
            "felt_const<7>() -> ([2])",
            "store_temp([2]) -> ([1])",
            // Post match.
            "label1:",
        ]
    );

    assert_eq!(res, sierra::ids::VarId::new(1));
}

fn prepare_test_data() -> (DatabaseImpl, defs::ids::CodeElementId) {
    let db = DatabaseImpl::default();
    let crate_id = db.intern_crate(CrateLongId("my_crate".into()));
    let module_id = ModuleId::CrateRoot(crate_id);
    let function_id =
        db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "my_func".into() });
    let block_id =
        db.intern_block(BlockLongId::FunctionBody(FunctionWithBodyId::FreeFunction(function_id)));
    // TODO(spapini): Remove this fake id once we get this data from the semantic model.
    let code_element = db.intern_code_element(CodeElementLongId::Block(block_id));
    (db, code_element)
}
