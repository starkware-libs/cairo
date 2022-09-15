//! This module is responsible of computing the semantic model of expressions and statements in
//! the code, while type checking.
//! It is invoked by queries for function bodies and other code blocks.

use std::collections::HashMap;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{GenericFunctionId, LocalVarLongId, ModuleId, VarId};
use diagnostics::Diagnostics;
use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::TerminalEx;
use syntax::node::{ast, TypedSyntaxNode};

use super::objects::*;
use crate::corelib::{core_binary_operator, unit_ty};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::items::functions::{ConcreteFunction, FunctionLongId};
use crate::resolve_item::resolve_item;
use crate::types::resolve_type;
use crate::{semantic, FunctionId, SemanticDiagnostic, TypeId, Variable};

/// Context for computing the semantic model of expression trees.
pub struct ComputationContext<'ctx> {
    diagnostics: &'ctx mut Diagnostics<SemanticDiagnostic>,
    db: &'ctx dyn SemanticGroup,
    module_id: ModuleId,
    return_ty: TypeId,
    environment: Box<Environment>,
}
impl<'ctx> ComputationContext<'ctx> {
    pub fn new(
        diagnostics: &'ctx mut Diagnostics<SemanticDiagnostic>,
        db: &'ctx dyn SemanticGroup,
        module_id: ModuleId,
        return_ty: TypeId,
        environment: Environment,
    ) -> Self {
        Self { diagnostics, db, module_id, return_ty, environment: Box::new(environment) }
    }

    /// Runs a function with a modified context, with a new environment for a subscope.
    /// This environment holds no variable of its own, but points to the current environment as a
    /// parent.
    /// Used for block expressions.
    fn run_in_subscope<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        // Push an environment to the stack.
        let new_environment = Box::new(Environment { parent: None, variables: HashMap::new() });
        let old_environment = std::mem::replace(&mut self.environment, new_environment);
        self.environment.parent = Some(old_environment);

        let res = f(self);

        // Pop the environment from the stack.
        let parent = self.environment.parent.take();
        self.environment = parent.unwrap();
        res
    }
}

pub type EnvVariables = HashMap<SmolStr, Variable>;

// TODO(spapini): Consider using identifiers instead of SmolStr everywhere in the code.
/// A state which contains all the variables defined at the current scope until now, and a pointer
/// to the parent environment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: EnvVariables,
}
impl Environment {
    pub fn new(variables: EnvVariables) -> Self {
        Self { parent: None, variables }
    }
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
    let syntax_db = db.upcast();
    let text = literal_syntax.terminal(syntax_db).text(syntax_db);
    // TODO(spapini): Diagnostics.
    let value = text.parse::<usize>().unwrap();
    let ty = db.core_felt_ty();
    semantic::ExprLiteral { value, ty, stable_ptr: literal_syntax.stable_ptr().untyped() }
}

/// Computes the semantic model of an expression.
pub fn compute_expr_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Expr,
) -> semantic::Expr {
    let db = ctx.db;
    let syntax_db = db.upcast();
    // TODO: When semantic::Expr holds the syntax pointer, add it here as well.
    match syntax {
        ast::Expr::Path(path) => {
            let stable_ptr = path.stable_ptr().untyped();
            match resolve_variable(ctx, &path) {
                Ok(var) => semantic::Expr::ExprVar(semantic::ExprVar {
                    var: var.id,
                    ty: var.ty,
                    stable_ptr,
                }),
                Err(diagnostic_kind) => {
                    ctx.diagnostics.add(SemanticDiagnostic {
                        stable_location: StableLocation::from_ast(ctx.module_id, &path),
                        kind: diagnostic_kind,
                    });
                    semantic::Expr::Missing { ty: TypeId::missing(db), stable_ptr }
                }
            }
            // TODO(spapini): Return the correct variable type, instead of the unit type.
        }
        ast::Expr::Literal(literal_syntax) => {
            semantic::Expr::ExprLiteral(literal_to_semantic(ctx, literal_syntax))
        }
        ast::Expr::Parenthesized(_) => todo!(),
        ast::Expr::Unary(_) => todo!(),
        ast::Expr::Binary(binary_op_syntax) => {
            let stable_ptr = binary_op_syntax.stable_ptr().untyped();
            let operator_kind = binary_op_syntax.op(syntax_db).kind(syntax_db);
            let lexpr = compute_expr_semantic(ctx, binary_op_syntax.lhs(syntax_db));
            let rexpr = compute_expr_semantic(ctx, binary_op_syntax.rhs(syntax_db));
            let function =
                match core_binary_operator(db, operator_kind).and_then(|generic_function| {
                    // TODO(lior): Can we avoid the clone() below?
                    specialize_function(ctx, generic_function, &[lexpr.clone(), rexpr.clone()])
                }) {
                    Some(generic_function) => generic_function,
                    None => {
                        ctx.diagnostics.add(SemanticDiagnostic {
                            stable_location: StableLocation::from_ast(
                                ctx.module_id,
                                &binary_op_syntax,
                            ),
                            kind: SemanticDiagnosticKind::UnknownBinaryOperator,
                        });
                        return semantic::Expr::Missing { ty: TypeId::missing(db), stable_ptr };
                    }
                };
            semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
                function,
                args: vec![db.intern_expr(lexpr), db.intern_expr(rexpr)],
                ty: function.return_type(db),
                stable_ptr,
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
                .map(|arg_syntax| compute_expr_semantic(ctx, arg_syntax))
                .collect();
            let function = resolve_function(ctx, path, &arg_exprs);
            let args = arg_exprs.into_iter().map(|expr| db.intern_expr(expr)).collect();
            semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall {
                function,
                args,
                ty: function.return_type(db),
                stable_ptr: call_syntax.stable_ptr().untyped(),
            })
        }
        ast::Expr::StructCtorCall(_) => todo!(),
        ast::Expr::Block(block_syntax) => {
            ctx.run_in_subscope(|new_ctx| {
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
                    .map(|statement_syntax| compute_statement_semantic(new_ctx, statement_syntax))
                    .collect();

                // Convert tail expression (if exists) to semantic model.
                let tail_semantic_expr =
                    tail.map(|tail_expr| compute_expr_semantic(new_ctx, tail_expr));

                let ty = match &tail_semantic_expr {
                    Some(t) => t.ty(),
                    None => unit_ty(db),
                };
                semantic::Expr::ExprBlock(semantic::ExprBlock {
                    statements: statements_semantic,
                    tail: tail_semantic_expr.map(|expr| db.intern_expr(expr)),
                    ty,
                    stable_ptr: block_syntax.stable_ptr().untyped(),
                })
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
                matched_expr: db
                    .intern_expr(compute_expr_semantic(ctx, expr_match.expr(syntax_db))),
                arms: semantic_arms,
                ty: match match_type {
                    Some(t) => t,
                    None => todo!("Return never-type"),
                },
                stable_ptr: expr_match.stable_ptr().untyped(),
            })
        }
        ast::Expr::ExprMissing(_) => todo!(),
    }
}

/// Resolves a variable given a context and a path expression.
fn resolve_variable(
    ctx: &mut ComputationContext<'_>,
    path: &ast::ExprPath,
) -> Result<Variable, SemanticDiagnosticKind> {
    let db = ctx.db;
    let syntax_db = db.upcast();
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
) -> Result<Variable, SemanticDiagnosticKind> {
    let mut maybe_env = Some(&*ctx.environment);
    while let Some(env) = maybe_env {
        if let Some(var) = env.variables.get(variable_name) {
            return Ok(var.clone());
        }
        maybe_env = env.parent.as_deref();
    }
    Err(SemanticDiagnosticKind::VariableNotFound { name: variable_name.clone() })
}

/// Resolves a concrete function given a context and a path expression.
/// Returns the generic function and the concrete function.
fn resolve_function(
    ctx: &mut ComputationContext<'_>,
    path: ast::ExprPath,
    args: &[semantic::Expr],
) -> FunctionId {
    // TODO(spapini): Try to find function in multiple places (e.g. impls, or other modules for
    //   suggestions)
    resolve_item(ctx.db, ctx.module_id, &path)
        .and_then(GenericFunctionId::from)
        .and_then(|generic_function| specialize_function(ctx, generic_function, args))
        .unwrap_or_else(|| {
            ctx.diagnostics.add(SemanticDiagnostic {
                stable_location: StableLocation::from_ast(ctx.module_id, &path),
                kind: SemanticDiagnosticKind::UnknownFunction,
            });
            FunctionId::missing(ctx.db)
        })
}

/// Tries to specializes a generic function.
fn specialize_function(
    ctx: &mut ComputationContext<'_>,
    generic_function: GenericFunctionId,
    args: &[semantic::Expr],
) -> Option<FunctionId> {
    // TODO(spapini): Type check arguments.
    let signature = ctx.db.generic_function_signature(generic_function)?;

    // TODO(lior): Replace with diagnostic and replace zip_eq below.
    assert_eq!(args.len(), signature.params.len());

    // Check argument types.
    for (arg, param) in itertools::zip_eq(args, signature.params) {
        let arg_typ = arg.ty();
        let param_typ = param.ty;
        // Don't add diagnostic if the type is missing (a diagnostic should have already been
        // added).
        // TODO(lior): Add a test to missing type once possible.
        if arg_typ != param_typ && arg_typ != TypeId::missing(ctx.db) {
            ctx.diagnostics.add(SemanticDiagnostic {
                stable_location: StableLocation::new(ctx.module_id, arg.stable_ptr()),
                kind: SemanticDiagnosticKind::WrongArgumentType {
                    expected_ty: param_typ,
                    actual_ty: arg_typ,
                },
            });
        }
    }

    Some(ctx.db.intern_function(FunctionLongId::Concrete(ConcreteFunction {
        generic_function,
        generic_args: vec![],
        return_type: signature.return_type,
    })))
}

/// Computes the semantic model of a statement.
pub fn compute_statement_semantic(
    ctx: &mut ComputationContext<'_>,
    syntax: ast::Statement,
) -> StatementId {
    let db = ctx.db;
    let syntax_db = db.upcast();
    let statement = match syntax {
        ast::Statement::Let(let_syntax) => {
            let var_id =
                db.intern_local_var(LocalVarLongId(ctx.module_id, let_syntax.stable_ptr()));

            let rhs_expr_id = db.intern_expr(compute_expr_semantic(ctx, let_syntax.rhs(syntax_db)));
            let inferred_type = db.lookup_intern_expr(rhs_expr_id).ty();

            let ty = match let_syntax.type_clause(syntax_db) {
                ast::OptionTypeClause::Empty(_) => inferred_type,
                ast::OptionTypeClause::TypeClause(type_clause) => {
                    let var_type_path = type_clause.ty(syntax_db);
                    let explicit_type =
                        resolve_type(ctx.diagnostics, db, ctx.module_id, var_type_path);
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
        ast::Statement::Expr(expr_syntax) => semantic::Statement::Expr(
            db.intern_expr(compute_expr_semantic(ctx, expr_syntax.expr(syntax_db))),
        ),
        ast::Statement::Return(return_syntax) => {
            let expr_syntax = return_syntax.expr(syntax_db);
            let expr = compute_expr_semantic(ctx, expr_syntax.clone());
            if expr.ty() != ctx.return_ty {
                ctx.diagnostics.add(SemanticDiagnostic {
                    stable_location: StableLocation::new(
                        ctx.module_id,
                        expr_syntax.stable_ptr().untyped(),
                    ),
                    kind: SemanticDiagnosticKind::WrongReturnType {
                        expected_ty: ctx.return_ty,
                        actual_ty: expr.ty(),
                    },
                })
            }
            semantic::Statement::Return(db.intern_expr(expr))
        }
        ast::Statement::StatementMissing(_) => todo!(),
    };
    db.intern_statement(statement)
}
