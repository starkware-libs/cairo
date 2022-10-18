// Reexport objects
pub use defs::ids::{ParamId, VarId};
use diagnostics_proc_macros::DebugWithDb;

pub use super::expr::objects::*;
use crate::db::SemanticGroup;
pub use crate::expr::pattern::{
    LocalVariable, Pattern, PatternEnum, PatternLiteral, PatternOtherwise, PatternStruct,
    PatternTuple, PatternVariable,
};
pub use crate::items::enm::{ConcreteVariant, Variant};
pub use crate::items::free_function::FreeFunctionDefinition;
pub use crate::items::functions::{ConcreteFunction, FunctionId, FunctionLongId, Signature};
pub use crate::items::strct::Member;
pub use crate::types::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteStructId, ConcreteTypeId, TypeId, TypeLongId,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Parameter {
    pub id: ParamId,
    pub ty: TypeId,
    pub modifiers: Modifiers,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Modifiers {
    pub is_ref: bool,
}

// TODO(yuval): consider making this an enum or the id an enum of ParamId/LocalVarId
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum Variable {
    Local(LocalVariable),
    Param(Parameter),
}
impl Variable {
    pub fn id(&self) -> VarId {
        match self {
            Variable::Local(local) => VarId::Local(local.id),
            Variable::Param(param) => VarId::Param(param.id),
        }
    }
    pub fn ty(&self) -> TypeId {
        match self {
            Variable::Local(local) => local.ty,
            Variable::Param(param) => param.ty,
        }
    }
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
