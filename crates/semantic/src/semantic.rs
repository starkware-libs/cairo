use defs::ids::{ParamId, VarId};
use diagnostics_proc_macros::DebugWithDb;

// Reexport objects
pub use super::expr::objects::*;
use crate::db::SemanticGroup;
pub use crate::items::enm::Variant;
pub use crate::items::functions::{ConcreteFunction, FunctionId, FunctionLongId, Signature};
pub use crate::items::strct::Member;
pub use crate::types::{ConcreteType, TypeId, TypeLongId};

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Parameter {
    pub id: ParamId,
    pub ty: TypeId,
}

// TODO(yuval): consider making this an enum or the id an enum of ParamId/LocalVarId
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Variable {
    pub id: VarId,
    pub ty: TypeId,
}

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum GenericArgumentId {
    Type(TypeId),
    // TODO(spapini): impls and constants as generic values.
}
