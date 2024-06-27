use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{MemberId, NamedLanguageElementId, VarId};
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use id_arena::Id;
use num_bigint::BigInt;

use super::fmt::ExprFormatter;
use super::pattern::Pattern;
use crate::items::constant::ConstValueId;
use crate::{semantic, ConcreteStructId, FunctionId, TypeId};

pub type PatternId = Id<Pattern>;
pub type ExprId = Id<Expr>;
pub type StatementId = Id<Statement>;

impl DebugWithDb<ExprFormatter<'_>> for PatternId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        expr_formatter: &ExprFormatter<'_>,
    ) -> std::fmt::Result {
        expr_formatter.db.pattern_semantic(expr_formatter.function_id, *self).fmt(f, expr_formatter)
    }
}
impl DebugWithDb<ExprFormatter<'_>> for ExprId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        expr_formatter: &ExprFormatter<'_>,
    ) -> std::fmt::Result {
        expr_formatter.db.expr_semantic(expr_formatter.function_id, *self).fmt(f, expr_formatter)
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
            .statement_semantic(expr_formatter.function_id, *self)
            .fmt(f, expr_formatter)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub enum Statement {
    Expr(StatementExpr),
    Let(StatementLet),
    Continue(StatementContinue),
    Return(StatementReturn),
    Break(StatementBreak),
}
impl Statement {
    pub fn stable_ptr(&self) -> ast::StatementPtr {
        match self {
            Statement::Expr(stmt) => stmt.stable_ptr,
            Statement::Let(stmt) => stmt.stable_ptr,
            Statement::Continue(stmt) => stmt.stable_ptr,
            Statement::Return(stmt) => stmt.stable_ptr,
            Statement::Break(stmt) => stmt.stable_ptr,
        }
    }
}

impl From<&Statement> for SyntaxStablePtrId {
    fn from(statement: &Statement) -> Self {
        statement.stable_ptr().into()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementExpr {
    pub expr: ExprId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementLet {
    pub pattern: PatternId,
    pub expr: ExprId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementContinue {
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementReturn {
    pub expr_option: Option<ExprId>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct StatementBreak {
    pub expr_option: Option<ExprId>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub enum Expr {
    Tuple(ExprTuple),
    Snapshot(ExprSnapshot),
    Desnap(ExprDesnap),
    Assignment(ExprAssignment),
    LogicalOperator(ExprLogicalOperator),
    Block(ExprBlock),
    Loop(ExprLoop),
    While(ExprWhile),
    For(ExprFor),
    FunctionCall(ExprFunctionCall),
    Match(ExprMatch),
    If(ExprIf),
    Var(ExprVar),
    Literal(ExprLiteral),
    StringLiteral(ExprStringLiteral),
    MemberAccess(ExprMemberAccess),
    StructCtor(ExprStructCtor),
    EnumVariantCtor(ExprEnumVariantCtor),
    PropagateError(ExprPropagateError),
    Constant(ExprConstant),
    FixedSizeArray(ExprFixedSizeArray),
    Missing(ExprMissing),
}
impl Expr {
    pub fn ty(&self) -> semantic::TypeId {
        match self {
            Expr::Assignment(expr) => expr.ty,
            Expr::Tuple(expr) => expr.ty,
            Expr::Snapshot(expr) => expr.ty,
            Expr::Desnap(expr) => expr.ty,
            Expr::LogicalOperator(expr) => expr.ty,
            Expr::Block(expr) => expr.ty,
            Expr::Loop(expr) => expr.ty,
            Expr::While(expr) => expr.ty,
            Expr::For(expr) => expr.ty,
            Expr::FunctionCall(expr) => expr.ty,
            Expr::Match(expr) => expr.ty,
            Expr::If(expr) => expr.ty,
            Expr::Var(expr) => expr.ty,
            Expr::Literal(expr) => expr.ty,
            Expr::StringLiteral(expr) => expr.ty,
            Expr::MemberAccess(expr) => expr.ty,
            Expr::StructCtor(expr) => expr.ty,
            Expr::EnumVariantCtor(expr) => expr.ty,
            Expr::PropagateError(expr) => expr.ok_variant.ty,
            Expr::Constant(expr) => expr.ty,
            Expr::Missing(expr) => expr.ty,
            Expr::FixedSizeArray(expr) => expr.ty,
        }
    }
    pub fn stable_ptr(&self) -> ast::ExprPtr {
        match self {
            Expr::Assignment(expr) => expr.stable_ptr,
            Expr::Tuple(expr) => expr.stable_ptr,
            Expr::Snapshot(expr) => expr.stable_ptr,
            Expr::Desnap(expr) => expr.stable_ptr,
            Expr::LogicalOperator(expr) => expr.stable_ptr,
            Expr::Block(expr) => expr.stable_ptr,
            Expr::Loop(expr) => expr.stable_ptr,
            Expr::While(expr) => expr.stable_ptr,
            Expr::For(expr) => expr.stable_ptr,
            Expr::FunctionCall(expr) => expr.stable_ptr,
            Expr::Match(expr) => expr.stable_ptr,
            Expr::If(expr) => expr.stable_ptr,
            Expr::Var(expr) => expr.stable_ptr,
            Expr::Literal(expr) => expr.stable_ptr,
            Expr::StringLiteral(expr) => expr.stable_ptr,
            Expr::MemberAccess(expr) => expr.stable_ptr,
            Expr::StructCtor(expr) => expr.stable_ptr,
            Expr::EnumVariantCtor(expr) => expr.stable_ptr,
            Expr::PropagateError(expr) => expr.stable_ptr,
            Expr::Constant(expr) => expr.stable_ptr,
            Expr::Missing(expr) => expr.stable_ptr,
            Expr::FixedSizeArray(expr) => expr.stable_ptr,
        }
    }

    pub fn as_member_path(&self) -> Option<ExprVarMemberPath> {
        match self {
            Expr::Var(expr) => Some(ExprVarMemberPath::Var(expr.clone())),
            Expr::MemberAccess(expr) => expr.member_path.clone(),
            _ => None,
        }
    }
}

impl From<&Expr> for SyntaxStablePtrId {
    fn from(expr: &Expr) -> Self {
        expr.stable_ptr().into()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprTuple {
    pub items: Vec<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprFixedSizeArray {
    pub items: FixedSizeArrayItems,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

/// Either a vector of items, if all was written in the code i.e. ([10, 11, 12] or [10, 10, 10]), or
/// a value and a size, if the array was written as ([10; 3]).
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub enum FixedSizeArrayItems {
    Items(Vec<ExprId>),
    ValueAndSize(ExprId, ConstValueId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprSnapshot {
    pub inner: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprDesnap {
    pub inner: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
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
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprLoop {
    pub body: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprWhile {
    pub condition: Condition,
    pub body: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprFor {
    pub into_iter: FunctionId,
    pub into_iter_member_path: ExprVarMemberPath,
    pub next_function_id: FunctionId,
    pub expr_id: ExprId,
    pub pattern: PatternId,
    pub body: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

/// A sequence of member accesses of a variable. For example: a, a.b, a.b.c, ...
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ExprVarMemberPath {
    Var(ExprVar),
    Member {
        parent: Box<ExprVarMemberPath>,
        member_id: MemberId,
        #[dont_rewrite]
        stable_ptr: ast::ExprPtr,
        concrete_struct_id: ConcreteStructId,
        // Type of the member.
        ty: TypeId,
    },
}
impl ExprVarMemberPath {
    pub fn base_var(&self) -> VarId {
        match self {
            ExprVarMemberPath::Var(expr) => expr.var,
            ExprVarMemberPath::Member { parent, .. } => parent.base_var(),
        }
    }
    pub fn ty(&self) -> TypeId {
        match self {
            ExprVarMemberPath::Var(expr) => expr.ty,
            ExprVarMemberPath::Member { ty, .. } => *ty,
        }
    }
    pub fn stable_ptr(&self) -> ast::ExprPtr {
        match self {
            ExprVarMemberPath::Var(var) => var.stable_ptr,
            ExprVarMemberPath::Member { stable_ptr, .. } => *stable_ptr,
        }
    }
}
impl<'a> DebugWithDb<ExprFormatter<'a>> for ExprVarMemberPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &ExprFormatter<'a>) -> std::fmt::Result {
        match self {
            ExprVarMemberPath::Var(var) => var.fmt(f, db),
            ExprVarMemberPath::Member { parent, member_id, .. } => {
                write!(f, "{:?}::{}", parent.debug(db), member_id.name(db.db.upcast()))
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub enum ExprFunctionCallArg {
    Reference(ExprVarMemberPath),
    Value(ExprId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprFunctionCall {
    pub function: FunctionId,
    pub args: Vec<ExprFunctionCallArg>,
    /// The `__coupon__` argument of the function call, if used. Attaching a coupon to a function
    /// means that the coupon is used instead of reducing the cost of the called function from the
    /// gas wallet. In particular, the cost of such a call is constant.
    pub coupon_arg: Option<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprMatch {
    pub matched_expr: ExprId,
    pub arms: Vec<MatchArm>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprIf {
    pub condition: Condition,
    pub if_block: ExprId,
    pub else_block: Option<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub enum Condition {
    BoolExpr(ExprId),
    Let(ExprId, Vec<PatternId>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct MatchArm {
    pub patterns: Vec<PatternId>,
    pub expression: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprAssignment {
    pub ref_arg: ExprVarMemberPath,
    pub rhs: semantic::ExprId,
    // ExprAssignment is always of unit type.
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalOperator {
    AndAnd,
    OrOr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprLogicalOperator {
    pub lhs: semantic::ExprId,
    #[dont_rewrite]
    pub op: LogicalOperator,
    pub rhs: semantic::ExprId,
    // ExprLogicalOperator is always of bool type.
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ExprVar {
    pub var: VarId,
    pub ty: semantic::TypeId,
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}
impl<'a> DebugWithDb<ExprFormatter<'a>> for ExprVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &ExprFormatter<'a>) -> std::fmt::Result {
        self.var.fmt(f, db)
    }
}

// TODO(yuval): rename to ExprNumericLiteral.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprLiteral {
    #[dont_rewrite]
    pub value: BigInt,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprStringLiteral {
    #[dont_rewrite]
    pub value: String,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprMemberAccess {
    pub expr: semantic::ExprId,
    pub concrete_struct_id: ConcreteStructId,
    pub member: MemberId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    pub member_path: Option<ExprVarMemberPath>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub n_snapshots: usize,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprStructCtor {
    pub concrete_struct_id: ConcreteStructId,
    pub members: Vec<(MemberId, ExprId)>,
    /// The base struct to copy missing members from if provided.
    /// For example `let x = MyStruct { a: 1, ..base }`.
    pub base_struct: Option<ExprId>,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprEnumVariantCtor {
    pub variant: semantic::ConcreteVariant,
    pub value_expr: ExprId,
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprPropagateError {
    pub inner: ExprId,
    pub ok_variant: semantic::ConcreteVariant,
    pub err_variant: semantic::ConcreteVariant,
    pub func_err_variant: semantic::ConcreteVariant,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprConstant {
    pub const_value_id: ConstValueId,
    pub ty: semantic::TypeId,
    #[dont_rewrite]
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'a>)]
pub struct ExprMissing {
    pub ty: semantic::TypeId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub diag_added: DiagnosticAdded,
}
