#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

use std::collections::HashMap;
use std::rc::Rc;

use defs::ids::VarId;
use filesystem::ids::ModuleId;
use smol_str::SmolStr;
use syntax::node::ast;

use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::{
    semantic, ConcreteFunctionId, ConcreteFunctionLongId, ExprId, GenericFunctionId, StatementId,
};

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'db> {
    db: &'db dyn SemanticGroup,
    module_id: ModuleId,
    environment: Rc<Environment>,
}

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current scope until now, and a pointer
/// to the parent environment.
pub struct Environment {
    parent: Option<Rc<Environment>>,
    variables: HashMap<SmolStr, VarId>,
}

/// Computes the semantic model of an expression.
pub fn compute_expr_semantic(ctx: &mut ComputationContext<'_>, syntax: ast::Expr) -> ExprId {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    // TODO: When semantic::Expr holds the syntax pointer, add it here as well.
    let expr = match syntax {
        ast::Expr::Path(path) => {
            let var = resolve_variable(ctx, path);
            // TODO(spapini): Return the correct variable type, instead of the unit type.
            semantic::Expr::ExprVar(semantic::ExprVar { var, ty: unit_ty(db) })
        }
        ast::Expr::Literal(literal_syntax) => {
            let text = literal_syntax.terminal(syntax_db).text(syntax_db);
            // TODO(spapini): Diagnostics.
            let value = text.parse::<usize>().unwrap();
            let ty = db.core_felt_ty();
            semantic::Expr::ExprLiteral(semantic::ExprLiteral { value, ty })
        }
        ast::Expr::Parenthesized(_) => todo!(),
        ast::Expr::Unary(_) => todo!(),
        ast::Expr::Binary(_) => todo!(),
        ast::Expr::Tuple(_) => todo!(),
        ast::Expr::FunctionCall(call_syntax) => {
            let path = call_syntax.path(syntax_db);
            let (generic_function, concrete_function) = resolve_concrete_function(ctx, path);
            let signature = db
                .generic_function_signature_semantic(generic_function)
                .expect("Diagnostics not supported yet")
                .expect("No signature");

            let args = call_syntax
                .arguments(syntax_db)
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .zip(signature.params)
                .map(|(arg_syntax, _param_id)| {
                    // TODO(spapini): Type check arguments.
                    compute_expr_semantic(ctx, arg_syntax)
                })
                .collect();
            semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
                function: concrete_function,
                args,
                ty: signature.return_type,
            })
        }
        ast::Expr::StructCtorCall(_) => todo!(),
        ast::Expr::Block(block_syntax) => {
            let environment = Rc::new(Environment {
                parent: Some(ctx.environment.clone()),
                variables: HashMap::new(),
            });
            let mut new_ctx = ComputationContext { environment, ..*ctx };
            semantic::Expr::ExprBlock(semantic::ExprBlock {
                statements: block_syntax
                    .statements(syntax_db)
                    .elements(syntax_db)
                    .into_iter()
                    .map(|statement_syntax| {
                        compute_statement_semantic(&mut new_ctx, statement_syntax)
                    })
                    .collect(),
                // TODO(spapini): Handle tail when it exists in ast.
                tail: None,
                ty: unit_ty(db),
            })
        }
        ast::Expr::Match(_) => todo!(),
        ast::Expr::ExprMissing(_) => todo!(),
    };
    db.intern_expr(expr)
}

/// Resolves a variable given a context and a path expression.
fn resolve_variable(ctx: &mut ComputationContext<'_>, path: ast::ExprPath) -> VarId {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    let elements = path.elements(syntax_db);
    if elements.len() != 1 {
        panic!("Expected a single identifier");
    }
    let last_element = &elements[0];
    match last_element.generic_args(syntax_db) {
        ast::OptionGenericArgs::Empty(_) => {}
        ast::OptionGenericArgs::Some(_) => todo!("Generics are not supported yet"),
    };
    let variable_name = last_element.ident(syntax_db).text(syntax_db);
    let mut maybe_env = Some(&*ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(&variable_name) {
            return *var;
        }
        maybe_env = env.parent.as_deref();
    }
    panic!("Not found");
}

/// Resolves a concrete function given a context and a path expression.
/// Returns the generic function and the concrete function.
fn resolve_concrete_function(
    ctx: &mut ComputationContext<'_>,
    path: ast::ExprPath,
) -> (GenericFunctionId, ConcreteFunctionId) {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    let elements = path.elements(syntax_db);
    if elements.len() != 1 {
        todo!("Qualified paths are not supported yet");
    }
    let last_element = &elements[0];
    match last_element.generic_args(syntax_db) {
        ast::OptionGenericArgs::Empty(_) => {}
        ast::OptionGenericArgs::Some(_) => todo!("Generics are not supported yet"),
    };
    let function_name = last_element.ident(syntax_db).text(syntax_db);
    let generic_function = match db
        .resolve_module_identifier(ctx.module_id, function_name)
        .expect("Diagnostics not supported yet")
        .expect("Unresolved identifier")
    {
        defs::ids::ModuleItemId::FreeFunction(free_function) => {
            GenericFunctionId::Free(free_function)
        }
        defs::ids::ModuleItemId::ExternFunction(extern_function) => {
            GenericFunctionId::Extern(extern_function)
        }
        defs::ids::ModuleItemId::Struct(_) => panic!("Unexpected struct"),
        defs::ids::ModuleItemId::ExternType(_) => panic!("Unexpected extern type"),
    };
    let concrete_function = db.intern_concrete_function(ConcreteFunctionLongId {
        generic_function,
        generic_args: vec![],
    });
    (generic_function, concrete_function)
}

/// Computes the semantic model of a statement.
pub fn compute_statement_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Statement,
) -> StatementId {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    let statement = match syntax {
        ast::Statement::Let(_) => todo!(),
        ast::Statement::Expr(expr_syntax) => {
            semantic::Statement::Expr(compute_expr_semantic(ctx, expr_syntax.expr(syntax_db)))
        }
        ast::Statement::Return(_) => todo!(),
        ast::Statement::StatementMissing(_) => todo!(),
    };
    db.intern_statement(statement)
}
