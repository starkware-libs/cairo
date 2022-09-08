#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

use std::collections::HashMap;

use defs::ids::{GenericFunctionId, LocalVarLongId, ModuleId, VarId};
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::TerminalEx;
use syntax::node::{ast, TypedSyntaxNode};

use crate::corelib::{core_binary_operator, unit_ty};
use crate::db::{resolve_type, SemanticGroup};
use crate::diagnostic::SemanticDiagnosticKind;
use crate::resolve_item::resolve_item;
use crate::{
    semantic, ConcreteFunction, Diagnostic, FunctionId, FunctionLongId, MatchArm,
    SemanticDiagnostic, StatementId, TypeId, Variable,
};

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    db: &'ctx dyn SemanticGroup,
    module_id: ModuleId,
    environment: Environment<'ctx>,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(db: &'ctx dyn SemanticGroup, module_id: ModuleId, variables: EnvVariables) -> Self {
        let environment = Environment { parent: None, variables };
        Self { db, module_id, environment }
    }
}

pub type EnvVariables = HashMap<SmolStr, Variable>;

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current scope until now, and a pointer
/// to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment<'penv> {
    parent: Option<&'penv Environment<'penv>>,
    variables: EnvVariables,
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
#[with_diagnostics]
pub fn compute_expr_semantic(
    diagnostics: &mut Diagnostics<Diagnostic>,
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Expr,
) -> semantic::Expr {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    // TODO: When semantic::Expr holds the syntax pointer, add it here as well.
    let expr = match syntax {
        ast::Expr::Path(path) => {
            let var = resolve_variable(ctx, path);
            // TODO(spapini): Return the correct variable type, instead of the unit type.
            semantic::Expr::ExprVar(semantic::ExprVar { var: var.id, ty: var.ty })
        }
        ast::Expr::Literal(literal_syntax) => {
            semantic::Expr::ExprLiteral(literal_to_semantic(ctx, literal_syntax))
        }
        ast::Expr::Parenthesized(_) => todo!(),
        ast::Expr::Unary(_) => todo!(),
        ast::Expr::Binary(binary_op_syntax) => {
            let operator_kind = binary_op_syntax.op(syntax_db).kind(syntax_db);
            let lexpr =
                compute_expr_semantic(ctx, binary_op_syntax.lhs(syntax_db)).propagte(diagnostics);
            let rexpr =
                compute_expr_semantic(ctx, binary_op_syntax.rhs(syntax_db)).propagte(diagnostics);
            let function = match core_binary_operator(db, operator_kind) {
                Some(function) => function,
                None => {
                    diagnostics.add(SemanticDiagnostic {
                        module_id: ctx.module_id,
                        stable_ptr: binary_op_syntax.as_syntax_node().stable_ptr(),
                        kind: SemanticDiagnosticKind::UnknownBinaryOperator,
                    });
                    return semantic::Expr::Missing(TypeId::missing(db));
                }
            };
            semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
                function,
                args: vec![db.intern_expr(lexpr), db.intern_expr(rexpr)],
                ty: function.return_type(db),
            })
        }
        ast::Expr::Tuple(_) => todo!(),
        ast::Expr::FunctionCall(call_syntax) => {
            let path = call_syntax.path(syntax_db);
            let arg_exprs: Vec<_> = call_syntax
                .arguments(syntax_db)
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|arg_syntax| compute_expr_semantic(ctx, arg_syntax).propagte(diagnostics))
                .collect();
            let arg_types: Vec<_> = arg_exprs.iter().map(|expr| expr.ty()).collect();
            let args = arg_exprs.into_iter().map(|expr| db.intern_expr(expr)).collect();
            let function = resolve_function(ctx, path, &arg_types).propagte(diagnostics);
            semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
                function,
                args,
                ty: function.return_type(db),
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
                .map(|statement_syntax| {
                    compute_statement_semantic(&mut new_ctx, statement_syntax).propagte(diagnostics)
                })
                .collect();

            // Convert tail expression (if exists) to semantic model.
            let tail_semantic_expr = tail.map(|tail_expr| {
                compute_expr_semantic(&mut new_ctx, tail_expr).propagte(diagnostics)
            });

            let ty = match &tail_semantic_expr {
                Some(t) => t.ty(),
                None => unit_ty(db),
            };
            semantic::Expr::ExprBlock(semantic::ExprBlock {
                statements: statements_semantic,
                tail: tail_semantic_expr.map(|expr| db.intern_expr(expr)),
                ty,
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
                let expr_semantic = compute_expr_semantic(ctx, syntax_arm.expression(syntax_db))
                    .propagte(diagnostics);
                let arm_type = expr_semantic.ty();
                semantic_arms.push(MatchArm { pattern, expression: db.intern_expr(expr_semantic) });
                match match_type {
                    Some(t) if t == arm_type => {}
                    Some(t) => {
                        panic!("Match arms have incompatible types: {t:?} and {arm_type:?}")
                    }
                    None => match_type = Some(arm_type),
                }
            }
            semantic::Expr::ExprMatch(semantic::ExprMatch {
                matched_expr: db.intern_expr(
                    compute_expr_semantic(ctx, expr_match.expr(syntax_db)).propagte(diagnostics),
                ),
                arms: semantic_arms,
                ty: match match_type {
                    Some(t) => t,
                    None => todo!("Return never-type"),
                },
            })
        }
        ast::Expr::ExprMissing(_) => todo!(),
    };
    expr
}

/// Resolves a variable given a context and a path expression.
fn resolve_variable(ctx: &mut ComputationContext<'_>, path: ast::ExprPath) -> Variable {
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
) -> Variable {
    let mut maybe_env = Some(&ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(variable_name) {
            return var.clone();
        }
        maybe_env = env.parent;
    }
    // TODO(spapini): Diagnostic and return option.
    panic!("Not found");
}

/// Resolves a concrete function given a context and a path expression.
/// Returns the generic function and the concrete function.
#[with_diagnostics]
fn resolve_function(
    diagnostics: &mut Diagnostics<Diagnostic>,
    ctx: &mut ComputationContext<'_>,
    path: ast::ExprPath,
    arg_types: &[TypeId],
) -> FunctionId {
    // TODO(spapini): Try to find function in multiple places (e.g. impls, or other modules for
    //   suggestions)
    resolve_item(ctx.db, ctx.module_id, &path)
        .propagte(diagnostics)
        .and_then(GenericFunctionId::from)
        .and_then(|generic_function| {
            specialize_function(ctx, generic_function, arg_types).propagte(diagnostics)
        })
        .unwrap_or_else(|| {
            diagnostics.add(SemanticDiagnostic {
                module_id: ctx.module_id,
                stable_ptr: path.node.stable_ptr(),
                kind: SemanticDiagnosticKind::UnknownFunction,
            });
            FunctionId::missing(ctx.db)
        })
}

/// Tries to specializes a generic function.
#[with_diagnostics]
fn specialize_function(
    diagnostics: &mut Diagnostics<Diagnostic>,
    ctx: &mut ComputationContext<'_>,
    generic_function: GenericFunctionId,
    _arg_types: &[TypeId],
) -> Option<FunctionId> {
    // TODO(spapini): Type check arguments.
    let signature =
        ctx.db.generic_function_signature_semantic(generic_function).propagte(diagnostics)?;
    Some(ctx.db.intern_function(FunctionLongId::Concrete(ConcreteFunction {
        generic_function,
        generic_args: vec![],
        return_type: signature.return_type,
    })))
}

/// Computes the semantic model of a statement.
#[with_diagnostics]
pub fn compute_statement_semantic(
    diagnostics: &mut Diagnostics<Diagnostic>,
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Statement,
) -> StatementId {
    let db = ctx.db;
    let syntax_db = db.as_syntax_group();
    let statement = match syntax {
        ast::Statement::Let(let_syntax) => {
            let var_id =
                db.intern_local_var(LocalVarLongId(ctx.module_id, let_syntax.stable_ptr()));

            let rhs_expr_id = db.intern_expr(
                compute_expr_semantic(ctx, let_syntax.rhs(syntax_db)).propagte(diagnostics),
            );
            let inferred_type = db.lookup_intern_expr(rhs_expr_id).ty();

            let ty = match let_syntax.type_clause(syntax_db) {
                ast::OptionTypeClause::Empty(_) => inferred_type,
                ast::OptionTypeClause::TypeClause(type_clause) => {
                    let var_type_path = type_clause.ty(syntax_db);
                    let explicit_type =
                        resolve_type(db, ctx.module_id, var_type_path).propagte(diagnostics);
                    assert_eq!(
                        explicit_type, inferred_type,
                        "inferred type ({explicit_type:?}) and explicit type ({inferred_type:?}) \
                         are different"
                    );
                    explicit_type
                }
            };
            ctx.environment.variables.insert(
                let_syntax.name(syntax_db).text(syntax_db),
                Variable { id: VarId::Local(var_id), ty },
            );
            semantic::Statement::Let(semantic::StatementLet {
                var: crate::LocalVariable { id: var_id, ty },
                expr: rhs_expr_id,
            })
        }
        ast::Statement::Expr(expr_syntax) => semantic::Statement::Expr(db.intern_expr(
            compute_expr_semantic(ctx, expr_syntax.expr(syntax_db)).propagte(diagnostics),
        )),
        ast::Statement::Return(_) => todo!(),
        ast::Statement::StatementMissing(_) => todo!(),
    };
    db.intern_statement(statement)
}
