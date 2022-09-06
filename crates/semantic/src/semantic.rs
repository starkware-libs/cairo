use defs::ids::{LocalVarId, MemberId, ParamId, VarId};

use crate::ids::{ConcreteFunctionId, ExprId, TypeId};
use crate::StatementId;

// Statements.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Statement {
    Expr(ExprId),
    Let(StatementLet),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StatementLet {
    pub var: LocalVariable,
    pub expr: ExprId,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    ExprBlock(ExprBlock),
    ExprFunctionCall(ExprFunctionCall),
    ExprMatch(ExprMatch),
    ExprVar(ExprVar),
    ExprLiteral(ExprLiteral),
}
impl Expr {
    pub fn ty(&self) -> TypeId {
        match self {
            Expr::ExprBlock(expr) => expr.ty,
            Expr::ExprFunctionCall(expr) => expr.ty,
            Expr::ExprMatch(expr) => expr.ty,
            Expr::ExprVar(expr) => expr.ty,
            Expr::ExprLiteral(expr) => expr.ty,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprBlock {
    pub statements: Vec<StatementId>,
    /// Blocks may end with an expression, without a trailing `;`.
    /// In this case, `tail` will be Some(expr) with that expression.
    /// The block expression will evaluate to this tail expression.
    /// Otherwise, this will be None.
    pub tail: Option<ExprId>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprFunctionCall {
    pub function: ConcreteFunctionId,
    pub args: Vec<ExprId>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprMatch {
    pub matched_expr: ExprId,
    pub arms: Vec<MatchArm>,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: ExprId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Pattern {
    Otherwise,
    Literal(ExprLiteral),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprVar {
    pub var: VarId,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprLiteral {
    // TODO(spapini): Fix the type of `value`.
    pub value: usize,
    pub ty: TypeId,
}

// Items.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FreeFunction {
    pub signature: Signature,
    pub body: ExprId,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signature {
    // TODO(spapini): Generics parameters.
    pub params: Vec<Parameter>,
    pub return_type: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parameter {
    pub id: ParamId,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LocalVariable {
    pub id: LocalVarId,
    pub ty: TypeId,
}

// TODO(yuval): consider making this an enum or the id an enum of ParamId/LocalVarId
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Variable {
    pub id: VarId,
    pub ty: TypeId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    pub members: Vec<MemberId>,
}
// TODO(spapini): Add semantic representation for Params and Members.
//   This will include their types.
