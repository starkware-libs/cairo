#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

use std::collections::HashMap;

use defs::ids::{GenericFunctionId, VarId};
use filesystem::ids::ModuleId;
use itertools::zip_eq;
use smol_str::SmolStr;
use syntax::node::ast;
use syntax::node::db::SyntaxGroup;

use crate::corelib::{core_module, unit_ty};
use crate::db::SemanticGroup;
use crate::{
    semantic, ConcreteFunctionId, ConcreteFunctionLongId, ExprId, MatchArm, StatementId, TypeId,
};

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    db: &'ctx dyn SemanticGroup,
    module_id: ModuleId,
    environment: Environment<'ctx>,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(
        db: &'ctx dyn SemanticGroup,
        module_id: ModuleId,
        variables: HashMap<SmolStr, VarId>,
    ) -> Self {
        let environment = Environment { parent: None, variables };
        Self { db, module_id, environment }
    }
}

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current scope until now, and a pointer
/// to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment<'penv> {
    parent: Option<&'penv Environment<'penv>>,
    variables: HashMap<SmolStr, VarId>,
}

/// Returns the tail expression of the given list of statements, if exists.
/// A tail expression is the last statement in the list, if it is an expression and
/// it does not end with a semicolon.
fn get_tail_expression(
    syntax_db: &dyn SyntaxGroup,
    statements: &[ast::Statement],
) -> Option<ast::Expr> {
    if statements.is_empty() {
        return None;
    }

    if let ast::Statement::Expr(statement_expr) = &statements[statements.len() - 1] {
        if let ast::OptionSemicolon::Empty(_) = statement_expr.semicolon(syntax_db) {
            return Some(statement_expr.expr(syntax_db));
        }
    }
    None
}

fn literal_to_semantic(
    ctx: &mut ComputationContext<'_>,
    literal_syntax: ast::ExprLiteral,
) -> semantic::ExprLiteral {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    let text = literal_syntax.terminal(syntax_db).text(syntax_db);
    // TODO(spapini): Diagnostics.
    let value = text.parse::<usize>().unwrap();
    let ty = db.core_felt_ty();
    semantic::ExprLiteral { value, ty }
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
            semantic::Expr::ExprLiteral(literal_to_semantic(ctx, literal_syntax))
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

            let args = zip_eq(
                call_syntax
                    .arguments(syntax_db)
                    .expressions(syntax_db)
                    .elements(syntax_db)
                    .into_iter(),
                signature.params,
            )
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
            let environment =
                Environment { parent: Some(&ctx.environment), variables: HashMap::new() };
            let mut new_ctx = ComputationContext { environment, ..*ctx };
            let mut statements = block_syntax.statements(syntax_db).elements(syntax_db);

            // Remove the tail expression, if exists.
            // TODO(spapini): Consider splitting tail expression in the parser.
            let tail = get_tail_expression(syntax_db, statements.as_slice());
            if tail.is_some() {
                statements.pop();
            }

            // Convert statements to semantic model.
            let statements_semantic = statements
                .into_iter()
                .map(|statement_syntax| compute_statement_semantic(&mut new_ctx, statement_syntax))
                .collect();

            // Convert tail expression (if exists) to semantic model.
            let tail_semantic =
                tail.map(|tail_expr| compute_expr_semantic(&mut new_ctx, tail_expr));

            semantic::Expr::ExprBlock(semantic::ExprBlock {
                statements: statements_semantic,
                tail: tail_semantic,
                ty: unit_ty(db),
            })
        }
        // TODO(yuval): verify exhaustiveness.
        ast::Expr::Match(expr_match) => {
            let syntax_arms = expr_match.arms(syntax_db).elements(syntax_db);
            let mut semantic_arms = Vec::new();
            let mut match_type: Option<TypeId> = None;
            for syntax_arm in syntax_arms {
                let pattern = match syntax_arm.pattern(syntax_db) {
                    ast::Pattern::Underscore(_) => semantic::Pattern::Otherwise,
                    ast::Pattern::Literal(literal) => {
                        let semantic_literal = literal_to_semantic(ctx, literal);
                        semantic::Pattern::Literal(semantic_literal)
                    }
                };
                let expr_semantic = compute_expr_semantic(ctx, syntax_arm.expression(syntax_db));
                let arm_type = db.lookup_intern_expr(expr_semantic).ty();
                semantic_arms.push(MatchArm { pattern, expression: expr_semantic });
                match match_type {
                    Some(t) if t == arm_type => {}
                    Some(t) => {
                        panic!("Match arms have incompatible types: {t:?} and {arm_type:?}")
                    }
                    None => match_type = Some(arm_type),
                }
            }
            semantic::Expr::ExprMatch(semantic::ExprMatch {
                matched_expr: compute_expr_semantic(ctx, expr_match.expr(syntax_db)),
                arms: semantic_arms,
                ty: match match_type {
                    Some(t) => t,
                    None => todo!("Return never-type"),
                },
            })
        }
        ast::Expr::ExprMissing(_) => todo!(),
    };
    db.intern_expr(expr)
}

/// Resolves a variable given a context and a path expression.
fn resolve_variable(ctx: &mut ComputationContext<'_>, path: ast::ExprPath) -> VarId {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    let segments = path.elements(syntax_db);
    if segments.len() != 1 {
        // TODO(spapini): Diagnostic.
        panic!("Expected a single identifier");
    }
    let last_segment = &segments[0];
    if let ast::OptionGenericArgs::Some(_) = last_segment.generic_args(syntax_db) {
        todo!("Generics are not supported")
    };
    let variable_name = last_segment.ident(syntax_db).text(syntax_db);
    resolve_variable_by_name(ctx, &variable_name)
}

/// Resolves a variable given a context and a simple name.
pub fn resolve_variable_by_name(
    ctx: &mut ComputationContext<'_>,
    variable_name: &SmolStr,
) -> VarId {
    let mut maybe_env = Some(&ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(variable_name) {
            return *var;
        }
        maybe_env = env.parent;
    }
    // TODO(spapini): Diagnostic and return option.
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
    // TODO(spapini): Support qualified paths.
    if elements.len() != 1 {
        todo!("Qualified paths are not supported yet");
    }
    let last_element = &elements[0];
    // TODO(spapini): Support generics.
    if let ast::OptionGenericArgs::Some(_) = last_element.generic_args(syntax_db) {
        todo!("Generics are not supported yet")
    };
    let function_name = last_element.ident(syntax_db).text(syntax_db);
    let generic_function = resolve_function_in_module(db, ctx.module_id, function_name.clone())
        .or_else(|| resolve_function_in_module(db, core_module(db), function_name))
        .expect("Unresolved identifier");
    let concrete_function = db.intern_concrete_function(ConcreteFunctionLongId {
        generic_function,
        generic_args: vec![],
    });
    (generic_function, concrete_function)
}

// TODO(spapini): Unite / be consistent with generic type resolution.
/// Resolves a generic function in a module, by name.
fn resolve_function_in_module(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    function_name: SmolStr,
) -> Option<GenericFunctionId> {
    let generic_function = match db
        .module_item_by_name(module_id, function_name)
        .expect("Diagnostics not supported yet")?
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
    Some(generic_function)
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
