use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::LocalVarId;
// Reexport objects
pub use cairo_lang_defs::ids::{ParamId, VarId};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ast;
use smol_str::SmolStr;

pub use super::expr::objects::*;
use crate::db::SemanticGroup;
pub use crate::expr::pattern::{
    Pattern, PatternEnumVariant, PatternLiteral, PatternOtherwise, PatternStringLiteral,
    PatternStruct, PatternTuple, PatternVariable,
};
pub use crate::items::enm::{ConcreteVariant, Variant};
pub use crate::items::function_with_body::FunctionBody;
pub use crate::items::functions::{
    ConcreteFunction, ConcreteFunctionWithBodyId, FunctionId, FunctionLongId, Signature,
};
pub use crate::items::generics::{GenericArgumentId, GenericParam};
pub use crate::items::imp::{ConcreteImplId, ConcreteImplLongId};
pub use crate::items::structure::Member;
pub use crate::items::trt::{ConcreteTraitId, ConcreteTraitLongId};
pub use crate::types::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteStructId, ConcreteTypeId, TypeId, TypeLongId,
};

/// Semantic model of a variable.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct LocalVariable {
    pub id: LocalVarId,
    pub ty: TypeId,
    #[dont_rewrite]
    pub is_mut: bool,
}
impl LocalVariable {
    pub fn stable_ptr(&self, db: &dyn DefsGroup) -> ast::TerminalIdentifierPtr {
        self.id.stable_ptr(db)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Parameter {
    pub id: ParamId,
    #[dont_rewrite]
    pub name: SmolStr,
    pub ty: TypeId,
    #[dont_rewrite]
    pub mutability: Mutability,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::TerminalIdentifierPtr,
}

/// The mutability attribute of a variable.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
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
