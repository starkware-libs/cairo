use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LocalConstId, LocalVarId};
// Reexport objects
pub use cairo_lang_defs::ids::{ParamId, VarId};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, TypedStablePtr};
use smol_str::SmolStr;

pub use super::expr::objects::*;
use crate::db::SemanticGroup;
pub use crate::expr::pattern::{
    Pattern, PatternEnumVariant, PatternFixedSizeArray, PatternLiteral, PatternOtherwise,
    PatternStringLiteral, PatternStruct, PatternTuple, PatternVariable,
};
use crate::items::constant::ConstValueId;
pub use crate::items::enm::{ConcreteVariant, MatchArmSelector, ValueSelectorArm, Variant};
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

/// Semantic model of a local constant.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct LocalConstant {
    pub id: LocalConstId,
    pub value: ConstValueId,
}
impl LocalConstant {
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
impl Parameter {
    pub fn stable_ptr(&self, db: &dyn DefsGroup) -> ast::ParamPtr {
        self.id.stable_ptr(db)
    }
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
pub enum Binding {
    LocalVar(LocalVariable),
    LocalConst(LocalConstant),
    Param(Parameter),
}
impl Binding {
    pub fn id(&self) -> VarId {
        match self {
            Binding::LocalVar(local) => VarId::Local(local.id),
            Binding::LocalConst(local) => VarId::Const(local.id),
            Binding::Param(param) => VarId::Param(param.id),
        }
    }
    pub fn ty(&self, db: &dyn SemanticGroup) -> TypeId {
        match self {
            Binding::LocalVar(local) => local.ty,
            Binding::LocalConst(local) => db.lookup_intern_const_value(local.value).ty(db).unwrap(),
            Binding::Param(param) => param.ty,
        }
    }
    pub fn is_mut(&self) -> bool {
        match self {
            Binding::LocalVar(local) => local.is_mut,
            Binding::LocalConst(_) => false,
            Binding::Param(param) => param.mutability != Mutability::Immutable,
        }
    }
    pub fn stable_ptr(&self, db: &dyn DefsGroup) -> SyntaxStablePtrId {
        match self {
            Binding::LocalVar(local) => local.stable_ptr(db).untyped(),
            Binding::LocalConst(local) => local.stable_ptr(db).untyped(),
            Binding::Param(param) => param.stable_ptr(db).untyped(),
        }
    }
}
impl From<LocalVariable> for Binding {
    fn from(var: LocalVariable) -> Self {
        Self::LocalVar(var)
    }
}
impl From<Parameter> for Binding {
    fn from(param: Parameter) -> Self {
        Self::Param(param)
    }
}
