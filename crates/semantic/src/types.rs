use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{GenericParamId, GenericTypeId, LanguageElementId};
use diagnostics_proc_macros::DebugWithDb;
use itertools::Itertools;
use syntax::node::ast;
use utils::{OptionFrom, OptionHelper};

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::{resolve_path, ResolveScope};
use crate::{semantic, GenericArgumentId};

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum TypeLongId {
    Concrete(ConcreteType),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    GenericParameter(GenericParamId),
    Missing,
}
impl OptionFrom<TypeLongId> for ConcreteType {
    fn option_from(other: TypeLongId) -> Option<Self> {
        if let TypeLongId::Concrete(res) = other { Some(res) } else { None }
    }
}

define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type);
impl TypeId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_type(TypeLongId::Missing)
    }
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match db.lookup_intern_type(*self) {
            TypeLongId::Concrete(ConcreteType { generic_type, generic_args }) => {
                // TODO(spapini): Format generics.
                let generic_type_format = generic_type.format(db.upcast());
                if generic_args.is_empty() {
                    generic_type_format
                } else {
                    format!(
                        "{}::<{}>",
                        generic_type_format,
                        generic_args
                            .into_iter()
                            .map(|arg| match arg {
                                crate::GenericArgumentId::Type(ty) => ty.format(db),
                            })
                            .join(", ")
                    )
                }
            }
            TypeLongId::Tuple(inner_types) => {
                format!("({})", inner_types.into_iter().map(|ty| ty.format(db)).join(", "))
            }
            TypeLongId::GenericParameter(generic_param) => {
                generic_param.name(db.upcast()).to_string()
            }
            TypeLongId::Missing => "<missing>".to_string(),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteType {
    pub generic_type: GenericTypeId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteType {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        self.generic_type.fmt(f, db)?;
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

// TODO(yuval): move to a separate module "type".
// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path.
/// pub fn maybe_resolve_type(
pub fn resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    scope: &ResolveScope,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, scope, ty_syntax).unwrap_or_else(|| TypeId::missing(db))
}
pub fn maybe_resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    scope: &ResolveScope,
    ty_syntax: &ast::Expr,
) -> Option<TypeId> {
    let syntax_db = db.upcast();
    Some(match ty_syntax {
        ast::Expr::Path(path) => {
            let item = resolve_path(db, diagnostics, scope, path)?;
            TypeId::option_from(item).on_none(|| diagnostics.report(path, UnknownStruct))?
        }
        ast::Expr::Parenthesized(expr_syntax) => {
            resolve_type(db, diagnostics, scope, &expr_syntax.expr(syntax_db))
        }
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|subexpr_syntax| resolve_type(db, diagnostics, scope, &subexpr_syntax))
                .collect();
            db.intern_type(TypeLongId::Tuple(sub_tys))
        }
        _ => {
            diagnostics.report(ty_syntax, UnknownType);
            return None;
        }
    })
}

/// Query implementation of [crate::db::SemanticGroup::generic_type_generic_params].
pub fn generic_type_generic_params(
    db: &dyn SemanticGroup,
    generic_type: GenericTypeId,
) -> Option<Vec<GenericParamId>> {
    // TODO(spapini): other types.
    match generic_type {
        GenericTypeId::Struct(_) => Some(vec![]),
        GenericTypeId::Enum(_) => Some(vec![]),
        GenericTypeId::Extern(id) => db.extern_type_declaration_generic_params(id),
    }
}

pub fn substitute_generics(
    db: &dyn SemanticGroup,
    substitution: &HashMap<GenericParamId, GenericArgumentId>,
    ty: crate::TypeId,
) -> TypeId {
    match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete) => db.intern_type(TypeLongId::Concrete(ConcreteType {
            generic_type: concrete.generic_type,
            generic_args: concrete
                .generic_args
                .into_iter()
                .map(|generic_arg| {
                    let GenericArgumentId::Type(ty) = generic_arg;
                    GenericArgumentId::Type(substitute_generics(db, substitution, ty))
                })
                .collect(),
        })),
        TypeLongId::Tuple(tys) => db.intern_type(TypeLongId::Tuple(
            tys.into_iter().map(|ty| substitute_generics(db, substitution, ty)).collect(),
        )),
        TypeLongId::GenericParameter(generic_param) => substitution
            .get(&generic_param)
            .map(|generic_arg| {
                let GenericArgumentId::Type(ty) = generic_arg;
                *ty
            })
            .unwrap_or(ty),
        TypeLongId::Missing => ty,
    }
}
