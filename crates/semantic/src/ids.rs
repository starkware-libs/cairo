use db_utils::define_short_id;
use defs::ids::{ExternFunctionId, ExternTypeId, FreeFunctionId, StructId};

// Ids in this file represent semantic representations.

// Generics.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericValueId {
    Type(TypeId),
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
    Free(FreeFunctionId),
    // TODO(spapini): impl functions.
    Extern(ExternFunctionId),
}
define_short_id!(FunctionInstanceId);

// Type instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericType {
    Struct(StructId),
    External(ExternTypeId),
    // TODO(spapini): enums, associated types in impls.
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Concrete(GenericType, Vec<GenericValueId>),
    // TODO(spapini): tuple, generic type parameters.
    Missing,
}
define_short_id!(TypeId);

// CodeElements.
define_short_id!(ExprId);
define_short_id!(StatementId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CodeElementId {
    Statement(StatementId),
    Expr(ExprId),
}
