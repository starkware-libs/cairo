use db_utils::define_short_id;
use defs::ids::{self, ExternFunctionId, ExternTypeId, FreeFunctionId, StructId};

use crate::semantic;

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
pub struct ConcreteFunctionLongId {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteFunctionId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericFunctionId {
    Free(FreeFunctionId),
    Extern(ExternFunctionId),
    // TODO(spapini): impl functions.
}

/// Type instance.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericType {
    Struct(StructId),
    External(ExternTypeId),
    // TODO(spapini): enums, associated types in impls.
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeLongId {
    Concrete(ConcreteType),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Missing,
    // TODO(spapini): tuple, generic type parameters.
}
define_short_id!(TypeId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteType {
    pub generic_type: GenericType,
    pub generic_args: Vec<GenericArgumentId>,
}

// Code elements - expressions and statements.
// Semantic expressions and statements are defined at 'semantic.rs'.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ExprData {
    pub expr: semantic::Expr,
    pub code_element: ids::CodeElementId,
}
define_short_id!(ExprId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StatementData {
    pub statement: semantic::Statement,
    pub code_element: ids::CodeElementId,
}
define_short_id!(StatementId);
