use debug::DebugWithDb;
use defs::ids::{MemberId, StructId, VarId};
use diagnostics::DiagnosticAdded;
use diagnostics_proc_macros::DebugWithDb;
use id_arena::Id;
use num_bigint::BigInt;
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
#[debug_db(ExprFormatter<'a>)]
pub enum Statement {
    Expr(StatementExpr),
    Let(StatementLet),
    Return(StatementReturn),
}
impl Statement {
    pub fn stable_ptr(&self) -> ast::StatementPtr {
        match self {
            Statement::Expr(stmt) => stmt.stable_ptr,
            Statement::Let(stmt) => stmt.stable_ptr,
            Statement::Return(stmt) => stmt.stable_ptr,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementExpr {
    pub expr: ExprId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::StatementPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementLet {
    pub pattern: Pattern,
    pub expr: ExprId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::StatementPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementReturn {
    pub expr: ExprId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::StatementPtr,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub enum Expr {
    Tuple(ExprTuple),
    Assignment(ExprAssignment),
    Block(ExprBlock),
    FunctionCall(ExprFunctionCall),
    Match(ExprMatch),
    If(ExprIf),
    Var(ExprVar),
    Literal(ExprLiteral),
    MemberAccess(ExprMemberAccess),
    StructCtor(ExprStructCtor),
    EnumVariantCtor(ExprEnumVariantCtor),
    PropagateError(ExprPropagateError),
    Missing(ExprMissing),
}
impl Expr {
    pub fn ty(&self) -> semantic::TypeId {
        match self {
            Expr::Tuple(expr) => expr.ty,
            Expr::Assignment(expr) => expr.ty,
            Expr::Block(expr) => expr.ty,
            Expr::FunctionCall(expr) => expr.ty,
            Expr::Match(expr) => expr.ty,
            Expr::If(expr) => expr.ty,
            Expr::Var(expr) => expr.ty,
            Expr::Literal(expr) => expr.ty,
            Expr::MemberAccess(expr) => expr.ty,
            Expr::StructCtor(expr) => expr.ty,
            Expr::EnumVariantCtor(expr) => expr.ty,
            Expr::PropagateError(expr) => expr.ok_variant.ty,
            Expr::Missing(expr) => expr.ty,
        }
    }
    pub fn stable_ptr(&self) -> ast::ExprPtr {
        match self {
            Expr::Assignment(expr) => expr.stable_ptr,
            Expr::Tuple(expr) => expr.stable_ptr,
            Expr::Block(expr) => expr.stable_ptr,
            Expr::FunctionCall(expr) => expr.stable_ptr,
            Expr::Match(expr) => expr.stable_ptr,
            Expr::If(expr) => expr.stable_ptr,
            Expr::Var(expr) => expr.stable_ptr,
            Expr::Literal(expr) => expr.stable_ptr,
            Expr::MemberAccess(expr) => expr.stable_ptr,
            Expr::StructCtor(expr) => expr.stable_ptr,
            Expr::EnumVariantCtor(expr) => expr.stable_ptr,
            Expr::PropagateError(expr) => expr.stable_ptr,
            Expr::Missing(expr) => expr.stable_ptr,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprTuple {
    pub items: Vec<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
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
#[debug_db(ExprFormatter<'a>)]
pub struct ExprFunctionCall {
    pub function: FunctionId,
    pub ref_args: Vec<VarId>,
    pub args: Vec<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprMatch {
    pub matched_expr: ExprId,
    pub arms: Vec<MatchArm>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprIf {
    pub condition: ExprId,
    pub if_block: ExprId,
    pub else_block: Option<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprAssignment {
    pub var: VarId,
    pub rhs: semantic::ExprId,
    // ExprAssignment is always of unit type.
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprVar {
    pub var: VarId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprLiteral {
    pub value: BigInt,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprMemberAccess {
    pub expr: semantic::ExprId,
    pub struct_id: StructId,
    pub member: MemberId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprStructCtor {
    pub struct_id: StructId,
    pub members: Vec<(MemberId, ExprId)>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprEnumVariantCtor {
    pub variant: semantic::ConcreteVariant,
    pub value_expr: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprPropagateError {
    pub inner: ExprId,
    pub ok_variant: semantic::ConcreteVariant,
    pub err_variant: semantic::ConcreteVariant,
    pub func_err_variant: semantic::ConcreteVariant,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprMissing {
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
    #[hide_field_debug_with_db]
    pub diag_added: DiagnosticAdded,
}
