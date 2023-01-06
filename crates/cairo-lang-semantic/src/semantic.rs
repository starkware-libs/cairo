use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LocalVarId;
// Reexport objects
pub use cairo_lang_defs::ids::{ParamId, VarId};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::ast;
use smol_str::SmolStr;

pub use super::expr::objects::*;
use crate::db::SemanticGroup;
pub use crate::expr::pattern::{
    Pattern, PatternEnumVariant, PatternLiteral, PatternOtherwise, PatternStruct, PatternTuple,
    PatternVariable,
};
pub use crate::items::enm::{ConcreteVariant, Variant};
pub use crate::items::free_function::FreeFunctionDefinition;
pub use crate::items::functions::{ConcreteFunction, FunctionId, FunctionLongId, Signature};
pub use crate::items::imp::{ConcreteImplId, ConcreteImplLongId};
pub use crate::items::strct::Member;
pub use crate::items::trt::{ConcreteTraitId, ConcreteTraitLongId};
use crate::literals::LiteralId;
pub use crate::types::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteStructId, ConcreteTypeId, TypeId, TypeLongId,
};

const UNUSED_PARAMETER_PREFIX: &str = "_";

/// Semantic model of a variable.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct LocalVariable {
    pub id: LocalVarId,
    pub ty: TypeId,
    pub is_mut: bool,
}
impl LocalVariable {
    pub fn stable_ptr(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifierPtr {
        self.id.stable_ptr(db)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Parameter {
    pub id: ParamId,
    /// The name of the parameter. `None` means a nameless parameter (`_`).
    pub name: Option<SmolStr>,
    pub ty: TypeId,
    pub mutability: Mutability,
}
impl Parameter {
    /// Returns the expected name of the corresponding argument.
    /// If the parameter name starts with `_`, it is removed.
    pub fn expected_arg_name(&self) -> Option<SmolStr> {
        let name = self.name.as_ref()?;
        Some(name.strip_prefix(UNUSED_PARAMETER_PREFIX).unwrap_or(name.as_str()).into())
    }
}

/// The mutability attribute of a variable.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Mutability {
    /// The variable can't be changed.
    Immutable,
    /// The variable can be changed locally.
    Mutable,
    /// Only relevant for a parameter.
    /// The parameter is an in-out parameter and a change in it affects the outer scope.
    Reference,
}

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
    pub fn is_mut(&self) -> bool {
        match self {
            Variable::Local(local) => local.is_mut,
            Variable::Param(param) => param.mutability != Mutability::Immutable,
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
    Literal(LiteralId),
    // TODO(spapini): impls and constants as generic values.
}
