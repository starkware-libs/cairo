use db_utils::define_short_id;
use defs::ids::{ExternFunctionId, ExternTypeId, FreeFunctionId, StructId};

// Ids in this file represent semantic representations.

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArgumentId {
    Type(TypeId),
    // TODO(spapini): impls and constants as generic values.
}

/// Function instance.
/// For example: ImplA::foo<A, B>, or bar<A>.
// TODO(spapini): Add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionInstanceLongId {
    generic_function: GenericFunctionId,
    generic_args: Vec<GenericArgumentId>,
}
define_short_id!(FunctionInstanceId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericFunctionId {
    Free(FreeFunctionId),
    Extern(ExternFunctionId),
    // TODO(spapini): impl functions.
}

// Type instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericType {
    Struct(StructId),
    External(ExternTypeId),
    // TODO(spapini): enums, associated types in impls.
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeLongId {
    Concrete(GenericType, Vec<GenericArgumentId>),
    // Some expressions might have invalid types during processing, either due to errors or
    // durin inference.
    Missing,
    // TODO(spapini): tuple, generic type parameters.
}
define_short_id!(TypeId);

pub struct ConcreteType {
    generic_type: GenericType,
    generic_args: Vec<GenericArgumentId>,
}

// CodeElements.
// Expressions and statements are defined at semantic.rs .
define_short_id!(ExprId);
define_short_id!(StatementId);
