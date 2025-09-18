use cairo_lang_defs::ids::{LocalVarId, StatementItemId};
// Reexport objects
pub use cairo_lang_defs::ids::{ParamId, VarId};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use salsa::Database;

pub use super::expr::objects::*;
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
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn Database)]
pub struct LocalVariable<'db> {
    pub id: LocalVarId<'db>,
    pub ty: TypeId<'db>,
    #[dont_rewrite]
    pub is_mut: bool,
    #[dont_rewrite]
    pub allow_unused: bool,
}
impl<'db> LocalVariable<'db> {
    pub fn stable_ptr(&self, db: &'db dyn Database) -> ast::TerminalIdentifierPtr<'db> {
        self.id.stable_ptr(db)
    }
}

/// Semantic model of a local item.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn Database)]
pub struct LocalItem<'db> {
    pub id: StatementItemId<'db>,
    pub kind: StatementItemKind<'db>,
}

/// Semantic model of statement item kind.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn Database)]
pub enum StatementItemKind<'db> {
    Constant(ConstValueId<'db>, TypeId<'db>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn Database)]
pub struct Parameter<'db> {
    pub id: ParamId<'db>,
    #[dont_rewrite]
    pub name: SmolStrId<'db>,
    pub ty: TypeId<'db>,
    #[dont_rewrite]
    pub mutability: Mutability,
    #[hide_field_debug_with_db]
    #[dont_rewrite]
    pub stable_ptr: ast::TerminalIdentifierPtr<'db>,
}
impl<'db> Parameter<'db> {
    pub fn stable_ptr(&self, db: &'db dyn Database) -> ast::ParamPtr<'db> {
        self.id.stable_ptr(db)
    }
}

/// The mutability attribute of a variable.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy, salsa::Update)]
pub enum Mutability {
    /// The variable can't be changed.
    Immutable,
    /// The variable can be changed locally.
    Mutable,
    /// Only relevant for a parameter.
    /// The parameter is an in-out parameter and a change in it affects the outer scope.
    Reference,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub enum Binding<'db> {
    LocalVar(LocalVariable<'db>),
    Param(Parameter<'db>),
    LocalItem(LocalItem<'db>),
}
impl<'db> Binding<'db> {
    pub fn id(&self) -> VarId<'db> {
        match self {
            Binding::LocalVar(local) => VarId::Local(local.id),
            Binding::Param(param) => VarId::Param(param.id),
            Binding::LocalItem(local) => VarId::Item(local.id),
        }
    }
    pub fn ty(&self) -> TypeId<'db> {
        match self {
            Binding::LocalVar(local) => local.ty,
            Binding::Param(param) => param.ty,
            Binding::LocalItem(local) => match local.kind {
                StatementItemKind::Constant(_, ty) => ty,
            },
        }
    }
    pub fn is_mut(&self) -> bool {
        match self {
            Binding::LocalVar(local) => local.is_mut,
            Binding::Param(param) => param.mutability != Mutability::Immutable,
            Binding::LocalItem(_) => false,
        }
    }
    pub fn stable_ptr(&self, db: &'db dyn Database) -> SyntaxStablePtrId<'db> {
        match self {
            Binding::LocalVar(local) => local.stable_ptr(db).untyped(),
            Binding::Param(param) => param.stable_ptr(db).untyped(),
            Binding::LocalItem(local) => local.id.name_stable_ptr(db),
        }
    }
}
impl<'db> From<LocalVariable<'db>> for Binding<'db> {
    fn from(var: LocalVariable<'db>) -> Self {
        Self::LocalVar(var)
    }
}
impl<'db> From<Parameter<'db>> for Binding<'db> {
    fn from(param: Parameter<'db>) -> Self {
        Self::Param(param)
    }
}
