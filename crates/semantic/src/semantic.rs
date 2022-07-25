use defs::ids::{
    FunctionArgId, FunctionId, GenericArgId, ImplAssociatedTypeId, LetVarId, MemberId,
    ModuleItemId, StructId, TraitFunctionId, TraitId, VarId, VariantId,
};

use crate::ids::{ExprId, FunctionInstanceId, GenericValueId, TypeInstanceId};

// Statements.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Statement {
    Expr(ExprId),
    Let(Let),
    Assign(Assign),
    Return(Return),
    Break,
    Loop(Loop),
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Let {
    var: LetVarId,
    expr: ExprId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Assign {
    var: VarId,
    expr: ExprId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Return {
    expr: ExprId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Loop {
    body: ExprBlock,
}

// Expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    ExprBlock(ExprBlock),
    ExprCall(ExprCall),
    ExprMatch(ExprMatch),
    ExprVar(ExprVar),
    ExprStructCtor(ExprStructCtor),
    ExprEnumCtor(ExprEnumCtor),
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
pub struct ExprDot {
    left: ExprId,
    right: MemberId,
    ty: TypeInstanceId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprStructCtor {
    strct: StructId,
    generic_values: Vec<GenericValueId>,
    args: Vec<ExprId>,
    ty: TypeInstanceId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprEnumCtor {
    variant: VariantId,
    generic_values: Vec<GenericValueId>,
    args: Vec<ExprId>,
    ty: TypeInstanceId,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprLiteral {
    // TODO: Literal value.
    ty: TypeInstanceId,
}

// Items.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Trait(Trait),
    Impl(Impl),
}
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    generic_args: Vec<GenericArgId>,
    members: Vec<MemberId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Enum {
    generic_args: Vec<GenericArgId>,
    variants: Vec<VariantId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Trait {
    generic_args: Vec<GenericArgId>,
    associated_types: Vec<ImplAssociatedTypeId>,
    functions: Vec<TraitFunctionId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Impl {
    generic_args: Vec<GenericArgId>,
    trt: TraitId,
    associated_types: Vec<(ImplAssociatedTypeId, TypeInstanceId)>,
    functions: Vec<(TraitFunctionId, FunctionId)>,
}

// Module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    items: Vec<ModuleItemId>,
}
