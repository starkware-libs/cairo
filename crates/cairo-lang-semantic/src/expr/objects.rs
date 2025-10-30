use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{MemberId, NamedLanguageElementId, StatementUseId, VarId};
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use id_arena::{Arena, ArenaBehavior};
use num_bigint::BigInt;
use salsa::Database;

use super::fmt::ExprFormatter;
use crate::items::constant::ConstValueId;
use crate::{ConcreteStructId, FunctionId, TypeId, semantic};

/// Defines an arena id type and its behavior for usage in an arena.
macro_rules! define_arena_id {
    ($id:ident, $behaviour:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $id(u32, usize);

        impl core::fmt::Debug for $id {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.debug_tuple(stringify!($id)).field(&self.1).finish()
            }
        }

        #[derive(Clone, Debug, PartialEq, Eq)]
        pub struct $behaviour;
        impl ArenaBehavior for $behaviour {
            type Id = $id;

            fn new_id(arena_id: u32, index: usize) -> Self::Id {
                $id(arena_id, index)
            }

            fn arena_id(id: Self::Id) -> u32 {
                id.0
            }

            fn index(id: Self::Id) -> usize {
                id.1
            }
        }
    };
}

define_arena_id!(PatternId, PatternArenaBehavior);
pub type PatternArena<'db> = Arena<semantic::Pattern<'db>, PatternArenaBehavior>;
define_arena_id!(ExprId, ExprArenaBehavior);
pub type ExprArena<'db> = Arena<semantic::Expr<'db>, ExprArenaBehavior>;
define_arena_id!(StatementId, StatementArenaBehavior);
pub type StatementArena<'db> = Arena<semantic::Statement<'db>, StatementArenaBehavior>;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub enum Statement<'db> {
    Expr(StatementExpr<'db>),
    Let(StatementLet<'db>),
    Continue(StatementContinue<'db>),
    Return(StatementReturn<'db>),
    Break(StatementBreak<'db>),
    Item(StatementItem<'db>),
}
impl<'db> Statement<'db> {
    pub fn stable_ptr(&self) -> ast::StatementPtr<'db> {
        match self {
            Statement::Expr(stmt) => stmt.stable_ptr,
            Statement::Let(stmt) => stmt.stable_ptr,
            Statement::Continue(stmt) => stmt.stable_ptr,
            Statement::Return(stmt) => stmt.stable_ptr,
            Statement::Break(stmt) => stmt.stable_ptr,
            Statement::Item(stmt) => stmt.stable_ptr,
        }
    }
}

impl<'db> From<&Statement<'db>> for SyntaxStablePtrId<'db> {
    fn from(statement: &Statement<'db>) -> Self {
        statement.stable_ptr().into()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct StatementExpr<'db> {
    pub expr: ExprId,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct StatementLet<'db> {
    pub pattern: PatternId,
    pub expr: ExprId,
    pub else_clause: Option<ExprId>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct StatementContinue<'db> {
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct StatementReturn<'db> {
    pub expr_option: Option<ExprId>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct StatementBreak<'db> {
    pub expr_option: Option<ExprId>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct StatementItem<'db> {
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::StatementPtr<'db>,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub enum Expr<'db> {
    Tuple(ExprTuple<'db>),
    Snapshot(ExprSnapshot<'db>),
    Desnap(ExprDesnap<'db>),
    Assignment(ExprAssignment<'db>),
    LogicalOperator(ExprLogicalOperator<'db>),
    Block(ExprBlock<'db>),
    Loop(ExprLoop<'db>),
    While(ExprWhile<'db>),
    For(ExprFor<'db>),
    FunctionCall(ExprFunctionCall<'db>),
    Match(ExprMatch<'db>),
    If(ExprIf<'db>),
    Var(ExprVar<'db>),
    Literal(ExprLiteral<'db>),
    StringLiteral(ExprStringLiteral<'db>),
    MemberAccess(ExprMemberAccess<'db>),
    StructCtor(ExprStructCtor<'db>),
    EnumVariantCtor(ExprEnumVariantCtor<'db>),
    PropagateError(ExprPropagateError<'db>),
    Constant(ExprConstant<'db>),
    FixedSizeArray(ExprFixedSizeArray<'db>),
    ExprClosure(ExprClosure<'db>),
    Missing(ExprMissing<'db>),
}
impl<'db> Expr<'db> {
    pub fn ty(&self) -> semantic::TypeId<'db> {
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
            Expr::ExprClosure(expr) => expr.ty,
        }
    }
    pub fn stable_ptr(&self) -> ast::ExprPtr<'db> {
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
            Expr::ExprClosure(expr) => expr.stable_ptr,
        }
    }

    /// Returns the member path of the expression, if it is a variable or a member access.
    pub fn as_member_path(&self) -> Option<ExprVarMemberPath<'db>> {
        match self {
            Expr::Var(expr) => Some(ExprVarMemberPath::Var(expr.clone())),
            Expr::MemberAccess(expr) => expr.member_path.clone(),
            _ => None,
        }
    }
}

impl<'db> From<&Expr<'db>> for SyntaxStablePtrId<'db> {
    fn from(expr: &Expr<'db>) -> Self {
        expr.stable_ptr().into()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprTuple<'db> {
    pub items: Vec<ExprId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprFixedSizeArray<'db> {
    pub items: FixedSizeArrayItems<'db>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

/// Either a vector of items, if all was written in the code i.e. ([10, 11, 12] or [10, 10, 10]), or
/// a value and a size, if the array was written as ([10; 3]).
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub enum FixedSizeArrayItems<'db> {
    Items(Vec<ExprId>),
    ValueAndSize(ExprId, ConstValueId<'db>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprSnapshot<'db> {
    pub inner: ExprId,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprDesnap<'db> {
    pub inner: ExprId,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprBlock<'db> {
    pub statements: Vec<StatementId>,
    /// Blocks may end with an expression, without a trailing `;`.
    /// In this case, `tail` will be Some(expr) with that expression.
    /// The block expression will evaluate to this tail expression.
    /// Otherwise, this will be None.
    pub tail: Option<ExprId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprLoop<'db> {
    pub body: ExprId,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprWhile<'db> {
    pub condition: Condition,
    pub body: ExprId,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprFor<'db> {
    pub into_iter: FunctionId<'db>,
    pub into_iter_member_path: ExprVarMemberPath<'db>,
    pub next_function_id: FunctionId<'db>,
    pub expr_id: ExprId,
    pub pattern: PatternId,
    pub body: ExprId,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

/// A sequence of member accesses of a variable. For example: a, a.b, a.b.c, ...
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub enum ExprVarMemberPath<'db> {
    Var(ExprVar<'db>),
    Member {
        parent: Box<ExprVarMemberPath<'db>>,
        member_id: MemberId<'db>,
        #[dont_rewrite]
        stable_ptr: ast::ExprPtr<'db>,
        concrete_struct_id: ConcreteStructId<'db>,
        // Type of the member.
        ty: TypeId<'db>,
    },
}
impl<'db> ExprVarMemberPath<'db> {
    pub fn base_var(&self) -> VarId<'db> {
        match self {
            ExprVarMemberPath::Var(expr) => expr.var,
            ExprVarMemberPath::Member { parent, .. } => parent.base_var(),
        }
    }
    pub fn ty(&self) -> TypeId<'db> {
        match self {
            ExprVarMemberPath::Var(expr) => expr.ty,
            ExprVarMemberPath::Member { ty, .. } => *ty,
        }
    }
    pub fn stable_ptr(&self) -> ast::ExprPtr<'db> {
        match self {
            ExprVarMemberPath::Var(var) => var.stable_ptr,
            ExprVarMemberPath::Member { stable_ptr, .. } => *stable_ptr,
        }
    }
}
impl<'db> DebugWithDb<'db> for ExprVarMemberPath<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        match self {
            ExprVarMemberPath::Var(var) => var.fmt(f, db),
            ExprVarMemberPath::Member { parent, member_id, .. } => {
                write!(f, "{:?}::{}", parent.debug(db), member_id.name(db).long(db))
            }
        }
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprClosure<'db> {
    pub body: ExprId,
    pub params: Vec<semantic::Parameter<'db>>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
    pub ty: TypeId<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub enum ExprFunctionCallArg<'db> {
    Reference(ExprVarMemberPath<'db>),
    Value(ExprId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprFunctionCall<'db> {
    pub function: FunctionId<'db>,
    pub args: Vec<ExprFunctionCallArg<'db>>,
    /// The `__coupon__` argument of the function call, if used. Attaching a coupon to a function
    /// means that the coupon is used instead of reducing the cost of the called function from the
    /// gas wallet. In particular, the cost of such a call is constant.
    pub coupon_arg: Option<ExprId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprMatch<'db> {
    pub matched_expr: ExprId,
    pub arms: Vec<MatchArm>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprIf<'db> {
    pub conditions: Vec<Condition>,
    pub if_block: ExprId,
    pub else_block: Option<ExprId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub enum Condition {
    BoolExpr(ExprId),
    Let(ExprId, Vec<PatternId>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct MatchArm {
    pub patterns: Vec<PatternId>,
    pub expression: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprAssignment<'db> {
    pub ref_arg: ExprVarMemberPath<'db>,
    pub rhs: semantic::ExprId,
    // ExprAssignment is always of unit type.
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalOperator {
    AndAnd,
    OrOr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprLogicalOperator<'db> {
    pub lhs: semantic::ExprId,
    #[dont_rewrite]
    pub op: LogicalOperator,
    pub rhs: semantic::ExprId,
    // ExprLogicalOperator is always of bool type.
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub struct ExprVar<'db> {
    pub var: VarId<'db>,
    pub ty: semantic::TypeId<'db>,
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}
impl<'db> DebugWithDb<'db> for ExprVar<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        self.var.fmt(f, db)
    }
}

// TODO(yuval): rename to ExprNumericLiteral.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprLiteral<'db> {
    #[dont_rewrite]
    pub value: BigInt,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprStringLiteral<'db> {
    #[dont_rewrite]
    pub value: String,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprMemberAccess<'db> {
    pub expr: semantic::ExprId,
    pub concrete_struct_id: ConcreteStructId<'db>,
    pub member: MemberId<'db>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    pub member_path: Option<ExprVarMemberPath<'db>>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub n_snapshots: usize,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprStructCtor<'db> {
    pub concrete_struct_id: ConcreteStructId<'db>,
    pub members: Vec<(ExprId, MemberId<'db>)>,
    /// The base struct to copy missing members from if provided.
    /// For example `let x = MyStruct { a: 1, ..base }`.
    pub base_struct: Option<ExprId>,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprEnumVariantCtor<'db> {
    pub variant: semantic::ConcreteVariant<'db>,
    pub value_expr: ExprId,
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprPropagateError<'db> {
    pub inner: ExprId,
    pub ok_variant: semantic::ConcreteVariant<'db>,
    pub err_variant: semantic::ConcreteVariant<'db>,
    pub func_err_variant: semantic::ConcreteVariant<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprConstant<'db> {
    pub const_value_id: ConstValueId<'db>,
    pub ty: semantic::TypeId<'db>,
    #[dont_rewrite]
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprUse<'db> {
    pub const_value_id: StatementUseId<'db>,
    pub ty: semantic::TypeId<'db>,
    #[dont_rewrite]
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::ExprPtr<'db>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(ExprFormatter<'db>)]
pub struct ExprMissing<'db> {
    pub ty: semantic::TypeId<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::ExprPtr<'db>,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub diag_added: DiagnosticAdded,
}

/// Arena for semantic expressions, patterns, and statements.
#[derive(Clone, Debug, Default, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn Database)]
pub struct Arenas<'db> {
    pub exprs: ExprArena<'db>,
    pub patterns: PatternArena<'db>,
    pub statements: StatementArena<'db>,
}

unsafe impl<'db> salsa::Update for Arenas<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_arenas: &mut Arenas<'db> = unsafe { &mut *old_pointer };

        // Next id includes both arena length and arena id.
        if old_arenas.exprs.next_id() != new_value.exprs.next_id()
            || old_arenas.patterns.next_id() != new_value.patterns.next_id()
            || old_arenas.statements.next_id() != new_value.statements.next_id()
        {
            *old_arenas = new_value;
            return true;
        }
        false
    }
}
