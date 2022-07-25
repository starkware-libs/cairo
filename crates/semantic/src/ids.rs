use db_utils::define_short_id;
use defs::ids::{ExternFunctionId, ExternTypeId, FreeFunctionId, GenericTypeArgId};

// Ids in this file represent semantic representations.

// Generics.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericValueId {
    Type(TypeInstanceId),
    // TODO(spapini): impls and constants as generic values.
}

// Function instance.
// For example: ImplA::foo<A, B>, or bar<A>.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionInstanceLongId {
    kind: FunctionInstanceKind,
    generic_values: Vec<GenericValueId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionInstanceKind {
    Top(FreeFunctionId),
    // TODO(spapini): impl functions.
    Extern(ExternFunctionId),
}
define_short_id!(FunctionInstanceId);

// Type instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeDefId {
    Generic(GenericTypeArgId),
    External(ExternTypeId),
    // TODO(spapini): structs, enums, associated types in impls.
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeInstanceLongId {
    TypeDef(TypeDefId, Vec<GenericValueId>),
    Missing,
}
define_short_id!(TypeInstanceId);

// CodeElements.
define_short_id!(ExprId);
define_short_id!(StatementId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CodeElementId {
    Statement(StatementId),
    Expr(ExprId),
}
