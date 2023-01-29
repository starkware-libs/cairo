use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{GenericKind, LocalVarId};
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
pub use crate::items::function_with_body::FunctionBody;
pub use crate::items::functions::{
    ConcreteFunction, ConcreteFunctionWithBodyId, FunctionId, FunctionLongId, Signature,
};
pub use crate::items::imp::{ConcreteImplId, ConcreteImplLongId};
pub use crate::items::strct::Member;
pub use crate::items::trt::{ConcreteTraitId, ConcreteTraitLongId};
use crate::literals::LiteralId;
pub use crate::types::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteStructId, ConcreteTypeId, TypeId, TypeLongId,
};

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
    pub name: SmolStr,
    pub ty: TypeId,
    pub mutability: Mutability,
    #[hide_field_debug_with_db]
    pub stable_ptr: ast::TerminalIdentifierPtr,
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
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArgumentId {
    Type(TypeId),
    Literal(LiteralId),
    Impl(ConcreteImplId), // TODO(spapini): impls and constants as generic values.
}
impl GenericArgumentId {
    pub fn kind(&self) -> GenericKind {
        match self {
            GenericArgumentId::Type(_) => GenericKind::Type,
            GenericArgumentId::Literal(_) => GenericKind::Const,
            GenericArgumentId::Impl(_) => GenericKind::Impl,
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            GenericArgumentId::Type(ty) => ty.format(db),
            GenericArgumentId::Literal(lit) => lit.format(db),
            GenericArgumentId::Impl(imp) => format!("{:?}", imp.debug(db.elongate())),
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for GenericArgumentId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        match self {
            GenericArgumentId::Type(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::Literal(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::Impl(id) => write!(f, "{:?}", id.debug(db)),
        }
    }
}
