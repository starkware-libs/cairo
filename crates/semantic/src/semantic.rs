use defs::ids::{LocalVarId, MemberId, ModuleItemId, ParamId, VarId};

use crate::ids::{ExprId, FunctionInstanceId, TypeId};

// Statements.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Statement {
    Expr(ExprId),
    Let(Let),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Let {
    var: LocalVarId,
    expr: ExprId,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    ExprBlock(ExprBlock),
    ExprCall(ExprCall),
    ExprMatch(ExprMatch),
    ExprVar(ExprVar),
    ExprLiteral(ExprLiteral),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprBlock {
    statements: Vec<Statement>,
    tail: Option<ExprId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprCall {
    function: FunctionInstanceId,
    args: Vec<ExprId>,
    ty: TypeId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprMatch {
    expr: ExprId,
    branches: Vec<Branch>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Branch {
    pattern: Pattern,
    block: ExprBlock,
}
// TODO(spapini): Implement pattern.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Pattern {}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprVar {
    var: VarId,
    ty: TypeId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprLiteral {
    // TODO(spapini): Literal value.
    ty: TypeId,
}

// Items.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FreeFunction {
    signature: Signature,
    body: ExprBlock,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    // TODO(spapini): Generics parameters.
    args: Vec<(ParamId, TypeId)>,
    ret: TypeId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    members: Vec<(MemberId, TypeId)>,
}

// Module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    items: Vec<ModuleItemId>,
}
