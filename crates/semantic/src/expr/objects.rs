use db_utils::define_short_id;
use defs::ids::{LocalVarId, VarId};
use diagnostics_proc_macros::DebugWithDb;
use syntax::node::ids::SyntaxStablePtrId;

use crate::db::SemanticGroup;
use crate::{semantic, FunctionId};

define_short_id!(ExprId, Expr, SemanticGroup, lookup_intern_expr);
define_short_id!(StatementId, Statement, SemanticGroup, lookup_intern_statement);

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum Statement {
    Expr(ExprId),
    Let(StatementLet),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct StatementLet {
    pub var: LocalVariable,
    pub expr: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct LocalVariable {
    pub id: LocalVarId,
    pub ty: semantic::TypeId,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum Expr {
    ExprBlock(ExprBlock),
    ExprFunctionCall(ExprFunctionCall),
    ExprMatch(ExprMatch),
    ExprVar(ExprVar),
    ExprLiteral(ExprLiteral),
    Missing { ty: semantic::TypeId, stable_ptr: SyntaxStablePtrId },
}
impl Expr {
    pub fn ty(&self) -> semantic::TypeId {
        match self {
            Expr::ExprBlock(expr) => expr.ty,
            Expr::ExprFunctionCall(expr) => expr.ty,
            Expr::ExprMatch(expr) => expr.ty,
            Expr::ExprVar(expr) => expr.ty,
            Expr::ExprLiteral(expr) => expr.ty,
            Expr::Missing { ty, stable_ptr: _ } => *ty,
        }
    }
    pub fn stable_ptr(&self) -> SyntaxStablePtrId {
        match self {
            Expr::ExprBlock(expr) => expr.stable_ptr,
            Expr::ExprFunctionCall(expr) => expr.stable_ptr,
            Expr::ExprMatch(expr) => expr.stable_ptr,
            Expr::ExprVar(expr) => expr.stable_ptr,
            Expr::ExprLiteral(expr) => expr.stable_ptr,
            Expr::Missing { ty: _, stable_ptr } => *stable_ptr,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct ExprBlock {
    pub statements: Vec<StatementId>,
    /// Blocks may end with an expression, without a trailing `;`.
    /// In this case, `tail` will be Some(expr) with that expression.
    /// The block expression will evaluate to this tail expression.
    /// Otherwise, this will be None.
    pub tail: Option<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: SyntaxStablePtrId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct ExprFunctionCall {
    pub function: FunctionId,
    pub args: Vec<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: SyntaxStablePtrId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct ExprMatch {
    pub matched_expr: ExprId,
    pub arms: Vec<MatchArm>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: SyntaxStablePtrId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum Pattern {
    Otherwise,
    Literal(ExprLiteral),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct ExprVar {
    pub var: VarId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: SyntaxStablePtrId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub struct ExprLiteral {
    // TODO(spapini): Fix the type of `value`.
    pub value: usize,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: SyntaxStablePtrId,
}
