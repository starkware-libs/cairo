use defs::ids::{FunctionArgId, GenericArgId, LocalVarId, ModuleItemId, VarId};

use crate::ids::{ExprId, FunctionInstanceId, TypeInstanceId};

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
    ty: TypeInstanceId,
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
// TODO:
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Pattern {}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprVar {
    var: VarId,
    ty: TypeInstanceId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprLiteral {
    // TODO(spapini): Literal value.
    ty: TypeInstanceId,
}

// Items.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Function {
    signature: Signature,
    body: ExprBlock,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    generic_args: Vec<GenericArgId>,
    args: Vec<(FunctionArgId, TypeInstanceId)>,
    ret: TypeInstanceId,
}

// Module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    items: Vec<ModuleItemId>,
}
