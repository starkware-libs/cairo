use db_utils::define_short_id;
use defs::ids::{GenericFunctionId, GenericTypeId};
use diagnostics_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::semantic::{Expr, Statement};

// Ids in this file represent semantic representations.

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum GenericArgumentId {
    Type(TypeId),
    // TODO(spapini): impls and constants as generic values.
}

/// Function instance.
/// For example: ImplA::foo<A, B>, or bar<A>.
// TODO(spapini): Add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionLongId {
    Concrete(ConcreteFunction),
    Missing,
}
define_short_id!(FunctionId, FunctionLongId, SemanticGroup, lookup_intern_function);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteFunction {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<GenericArgumentId>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum TypeLongId {
    Concrete(ConcreteType),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    Missing,
    // TODO(spapini): tuple, generic type parameters.
}
define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteType {
    pub generic_type: GenericTypeId,
    pub generic_args: Vec<GenericArgumentId>,
}

// CodeElements.
// Expressions and statements are defined at 'semantic.rs'.
define_short_id!(ExprId, Expr, SemanticGroup, lookup_intern_expr);
define_short_id!(StatementId, Statement, SemanticGroup, lookup_intern_statement);
