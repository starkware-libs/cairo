use debug::DebugWithDb;
use defs::ids::{MemberId, StructId, VarId, VariantId};
use diagnostics_proc_macros::DebugWithDb;
use id_arena::Id;
use syntax::node::ast::{self};

use super::fmt::ExprFormatter;
use super::pattern::Pattern;
use crate::{semantic, FunctionId};

pub type ExprId = Id<Expr>;
pub type StatementId = Id<Statement>;

impl DebugWithDb<ExprFormatter<'_>> for ExprId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        expr_formatter: &ExprFormatter<'_>,
    ) -> std::fmt::Result {
        expr_formatter
            .db
            .expr_semantic(expr_formatter.free_function_id, *self)
            .fmt(f, expr_formatter)
    }
}
impl DebugWithDb<ExprFormatter<'_>> for StatementId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        expr_formatter: &ExprFormatter<'_>,
    ) -> std::fmt::Result {
        expr_formatter
            .db
            .statement_semantic(expr_formatter.free_function_id, *self)
            .fmt(f, expr_formatter)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub enum Statement {
    Expr(ExprId),
    Let(StatementLet),
    Return(ExprId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct StatementLet {
    pub pattern: Pattern,
    pub expr: ExprId,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub enum Expr {
    ExprTuple(ExprTuple),
    ExprAssignment(ExprAssignment),
    ExprBlock(ExprBlock),
    ExprFunctionCall(ExprFunctionCall),
    ExprMatch(ExprMatch),
    ExprVar(ExprVar),
    ExprLiteral(ExprLiteral),
    ExprMemberAccess(ExprMemberAccess),
    ExprStructCtor(ExprStructCtor),
    ExprEnumVariantCtor(ExprEnumVariantCtor),
    Missing { ty: semantic::TypeId, stable_ptr: ast::ExprPtr },
}
impl Expr {
    pub fn ty(&self) -> semantic::TypeId {
        match self {
            Expr::ExprTuple(expr) => expr.ty,
            Expr::ExprAssignment(expr) => expr.ty,
            Expr::ExprBlock(expr) => expr.ty,
            Expr::ExprFunctionCall(expr) => expr.ty,
            Expr::ExprMatch(expr) => expr.ty,
            Expr::ExprVar(expr) => expr.ty,
            Expr::ExprLiteral(expr) => expr.ty,
            Expr::ExprMemberAccess(expr) => expr.ty,
            Expr::ExprStructCtor(expr) => expr.ty,
            Expr::ExprEnumVariantCtor(expr) => expr.ty,
            Expr::Missing { ty, stable_ptr: _ } => *ty,
        }
    }
    pub fn stable_ptr(&self) -> ast::ExprPtr {
        match self {
            Expr::ExprAssignment(expr) => expr.stable_ptr,
            Expr::ExprTuple(expr) => expr.stable_ptr,
            Expr::ExprBlock(expr) => expr.stable_ptr,
            Expr::ExprFunctionCall(expr) => expr.stable_ptr,
            Expr::ExprMatch(expr) => expr.stable_ptr,
            Expr::ExprVar(expr) => expr.stable_ptr,
            Expr::ExprLiteral(expr) => expr.stable_ptr,
            Expr::ExprMemberAccess(expr) => expr.stable_ptr,
            Expr::ExprStructCtor(expr) => expr.stable_ptr,
            Expr::ExprEnumVariantCtor(expr) => expr.stable_ptr,
            Expr::Missing { ty: _, stable_ptr } => *stable_ptr,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprTuple {
    pub items: Vec<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprBlock {
    pub statements: Vec<StatementId>,
    /// Blocks may end with an expression, without a trailing `;`.
    /// In this case, `tail` will be Some(expr) with that expression.
    /// The block expression will evaluate to this tail expression.
    /// Otherwise, this will be None.
    pub tail: Option<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprFunctionCall {
    pub function: FunctionId,
    pub args: Vec<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprMatch {
    pub matched_expr: ExprId,
    pub arms: Vec<MatchArm>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprAssignment {
    pub var: VarId,
    pub rhs: semantic::ExprId,
    // ExprAssignment is always of unit type.
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprVar {
    pub var: VarId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprLiteral {
    // TODO(spapini): Fix the type of `value`.
    pub value: usize,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprMemberAccess {
    pub expr: semantic::ExprId,
    pub member: MemberId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprStructCtor {
    pub struct_id: StructId,
    pub members: Vec<(MemberId, ExprId)>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'_>)]
pub struct ExprEnumVariantCtor {
    pub enum_variant_id: VariantId,
    pub value_expr: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}
