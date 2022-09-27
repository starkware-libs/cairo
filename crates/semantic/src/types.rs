use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, StructId};
use itertools::Itertools;
use syntax::node::{ast, TypedSyntaxNode};
use utils::OptionFrom;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::{specialize_type, ResolvedItem, Resolver};
use crate::{semantic, GenericArgumentId};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        db.lookup_intern_type(*self).format(db)
    }
}
impl TypeLongId {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            TypeLongId::Concrete(concrete) => concrete.format(db),
            TypeLongId::Tuple(inner_types) => {
                format!("({})", inner_types.iter().map(|ty| ty.format(db)).join(", "))
            }
            TypeLongId::GenericParameter(generic_param) => {
                generic_param.name(db.upcast()).to_string()
            }
            TypeLongId::Missing => "<missing>".to_string(),
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for TypeLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConcreteType {
    Struct(ConcreteStruct),
    Enum(ConcreteEnum),
    Extern(ConcreteExternType),
}
impl ConcreteType {
    pub fn new(generic_ty: GenericTypeId, generic_args: Vec<semantic::GenericArgumentId>) -> Self {
        match generic_ty {
            GenericTypeId::Struct(id) => {
                ConcreteType::Struct(ConcreteStruct { struct_id: id, generic_args })
            }
            GenericTypeId::Enum(id) => {
                ConcreteType::Enum(ConcreteEnum { enum_id: id, generic_args })
            }
            GenericTypeId::Extern(id) => {
                ConcreteType::Extern(ConcreteExternType { extern_type_id: id, generic_args })
            }
        }
    }
    pub fn generic_type(&self) -> GenericTypeId {
        match self {
            ConcreteType::Struct(concrete) => GenericTypeId::Struct(concrete.struct_id),
            ConcreteType::Enum(concrete) => GenericTypeId::Enum(concrete.enum_id),
            ConcreteType::Extern(concrete) => GenericTypeId::Extern(concrete.extern_type_id),
        }
    }
    pub fn generic_args(&self) -> &[semantic::GenericArgumentId] {
        match self {
            ConcreteType::Struct(concrete) => &concrete.generic_args,
            ConcreteType::Enum(concrete) => &concrete.generic_args,
            ConcreteType::Extern(concrete) => &concrete.generic_args,
        }
    }
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        // TODO(spapini): Format generics.
        let generic_type_format = self.generic_type().format(db.upcast());
        let generic_args = self.generic_args();
        if generic_args.is_empty() {
            generic_type_format
        } else {
            format!(
                "{}::<{}>",
                generic_type_format,
                generic_args
                    .iter()
                    .map(|arg| match arg {
                        crate::GenericArgumentId::Type(ty) => ty.format(db),
                    })
                    .join(", ")
            )
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteType {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteStruct {
    pub struct_id: StructId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteEnum {
    pub enum_id: EnumId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteExternType {
    pub extern_type_id: ExternTypeId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}

// TODO(yuval): move to a separate module "type".
// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path.
/// pub fn maybe_resolve_type(
pub fn resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, resolver, ty_syntax).unwrap_or_else(|| TypeId::missing(db))
}
pub fn maybe_resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> Option<TypeId> {
    let syntax_db = db.upcast();
    Some(match ty_syntax {
        ast::Expr::Path(path) => match resolver.resolve_path(diagnostics, path)? {
            ResolvedItem::GenericType(generic_type) => {
                specialize_type(db, diagnostics, path.stable_ptr().untyped(), generic_type, vec![])?
            }
            ResolvedItem::Type(ty) => ty,

            _ => {
                diagnostics.report(path, NotAType);
                return None;
            }
        },
        ast::Expr::Parenthesized(expr_syntax) => {
            resolve_type(db, diagnostics, resolver, &expr_syntax.expr(syntax_db))
        }
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|subexpr_syntax| resolve_type(db, diagnostics, resolver, &subexpr_syntax))
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
        TypeLongId::Concrete(concrete) => db.intern_type(TypeLongId::Concrete(ConcreteType::new(
            concrete.generic_type(),
            concrete
                .generic_args()
                .iter()
                .map(|generic_arg| {
                    let GenericArgumentId::Type(ty) = generic_arg;
                    GenericArgumentId::Type(substitute_generics(db, substitution, *ty))
                })
                .collect(),
        ))),
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
