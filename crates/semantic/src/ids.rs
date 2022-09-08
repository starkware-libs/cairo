use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{GenericFunctionId, GenericTypeId};
use diagnostics_proc_macros::DebugWithDb;

use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::semantic::{Expr, Statement};

// Ids in this file represent semantic representations.

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum GenericArgumentId {
    Type(TypeId),
    // TODO(spapini): impls and constants as generic values.
}

/// Function instance.
/// For example: ImplA::foo<A, B>, or bar<A>.
// TODO(spapini): Add a function pointer variant.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(SemanticGroup)]
pub enum FunctionLongId {
    Concrete(ConcreteFunction),
    Missing,
}
define_short_id!(FunctionId, FunctionLongId, SemanticGroup, lookup_intern_function);
impl FunctionId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_function(FunctionLongId::Missing)
    }

    pub fn return_type(&self, db: &dyn SemanticGroup) -> TypeId {
        match db.lookup_intern_function(*self) {
            FunctionLongId::Concrete(function) => function.return_type,
            FunctionLongId::Missing => TypeId::missing(db),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteFunction {
    pub generic_function: GenericFunctionId,
    pub generic_args: Vec<GenericArgumentId>,
    pub return_type: TypeId,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteFunction {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        self.generic_function.fmt(f, db.as_defs_group())?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for arg in self.generic_args.iter() {
                write!(f, "{:?},", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        if self.return_type != unit_ty(db) && !self.generic_args.is_empty() {
            write!(f, " -> {:?}", self.return_type)?;
        }
        Ok(())
    }
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
impl TypeId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_type(TypeLongId::Missing)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteType {
    pub generic_type: GenericTypeId,
    pub generic_args: Vec<GenericArgumentId>,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteType {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        self.generic_type.fmt(f, db.as_defs_group())?;
        if !self.generic_args.is_empty() {
            write!(f, "<")?;
            for arg in self.generic_args.iter() {
                write!(f, "{:?},", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

// CodeElements.
// Expressions and statements are defined at 'semantic.rs'.
define_short_id!(ExprId, Expr, SemanticGroup, lookup_intern_expr);
define_short_id!(StatementId, Statement, SemanticGroup, lookup_intern_statement);
