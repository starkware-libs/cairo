use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, StructId};
use diagnostics::{skip_diagnostic, DiagnosticAdded, Maybe};
use itertools::Itertools;
use syntax::node::ast;
use utils::OptionFrom;

use crate::corelib::{concrete_copy_trait, concrete_drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::items::imp::{find_impls_at_context, ImplLookupContext};
use crate::resolve_path::{ResolvedConcreteItem, Resolver};
use crate::{semantic, GenericArgumentId};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeLongId {
    Concrete(ConcreteTypeId),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    GenericParameter(GenericParamId),
    Missing(DiagnosticAdded),
}
impl OptionFrom<TypeLongId> for ConcreteTypeId {
    fn option_from(other: TypeLongId) -> Option<Self> {
        if let TypeLongId::Concrete(res) = other { Some(res) } else { None }
    }
}

define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type);
impl TypeId {
    pub fn missing(db: &dyn SemanticGroup, diag_added: DiagnosticAdded) -> Self {
        db.intern_type(TypeLongId::Missing(diag_added))
    }

    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        db.lookup_intern_type(*self).format(db)
    }

    /// Returns [Maybe::Err] if the type is [TypeLongId::Missing].
    pub fn check_not_missing(&self, db: &dyn SemanticGroup) -> Maybe<()> {
        if let TypeLongId::Missing(diag_added) = db.lookup_intern_type(*self) {
            Err(diag_added)
        } else {
            Ok(())
        }
    }

    /// Returns `true` if the type is [TypeLongId::Missing].
    pub fn is_missing(&self, db: &dyn SemanticGroup) -> bool {
        self.check_not_missing(db).is_err()
    }
}
impl TypeLongId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            TypeLongId::Concrete(concrete) => concrete.format(db),
            TypeLongId::Tuple(inner_types) => {
                if inner_types.len() == 1 {
                    format!("({},)", inner_types[0].format(db))
                } else {
                    format!("({})", inner_types.iter().map(|ty| ty.format(db)).join(", "))
                }
            }
            TypeLongId::GenericParameter(generic_param) => {
                generic_param.name(db.upcast()).to_string()
            }
            TypeLongId::Missing(_) => "<missing>".to_string(),
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
pub enum ConcreteTypeId {
    Struct(ConcreteStructId),
    Enum(ConcreteEnumId),
    Extern(ConcreteExternTypeId),
}
impl ConcreteTypeId {
    pub fn new(
        db: &dyn SemanticGroup,
        generic_ty: GenericTypeId,
        generic_args: Vec<semantic::GenericArgumentId>,
    ) -> Self {
        match generic_ty {
            GenericTypeId::Struct(id) => ConcreteTypeId::Struct(
                db.intern_concrete_struct(ConcreteStructLongId { struct_id: id, generic_args }),
            ),
            GenericTypeId::Enum(id) => ConcreteTypeId::Enum(
                db.intern_concrete_enum(ConcreteEnumLongId { enum_id: id, generic_args }),
            ),
            GenericTypeId::Extern(id) => {
                ConcreteTypeId::Extern(db.intern_concrete_extern_type(ConcreteExternTypeLongId {
                    extern_type_id: id,
                    generic_args,
                }))
            }
        }
    }
    pub fn generic_type(&self, db: &dyn SemanticGroup) -> GenericTypeId {
        match self {
            ConcreteTypeId::Struct(id) => {
                GenericTypeId::Struct(db.lookup_intern_concrete_struct(*id).struct_id)
            }
            ConcreteTypeId::Enum(id) => {
                GenericTypeId::Enum(db.lookup_intern_concrete_enum(*id).enum_id)
            }
            ConcreteTypeId::Extern(id) => {
                GenericTypeId::Extern(db.lookup_intern_concrete_extern_type(*id).extern_type_id)
            }
        }
    }
    pub fn generic_args(&self, db: &dyn SemanticGroup) -> Vec<semantic::GenericArgumentId> {
        match self {
            ConcreteTypeId::Struct(id) => db.lookup_intern_concrete_struct(*id).generic_args,
            ConcreteTypeId::Enum(id) => db.lookup_intern_concrete_enum(*id).generic_args,
            ConcreteTypeId::Extern(id) => db.lookup_intern_concrete_extern_type(*id).generic_args,
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        // TODO(spapini): Format generics.
        let generic_type_format = self.generic_type(db).format(db.upcast());
        let generic_args = self.generic_args(db);
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
                        crate::GenericArgumentId::Literal(literal_id) => literal_id.format(db),
                    })
                    .join(", ")
            )
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteTypeId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteStructLongId {
    pub struct_id: StructId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
define_short_id!(
    ConcreteStructId,
    ConcreteStructLongId,
    SemanticGroup,
    lookup_intern_concrete_struct
);
impl ConcreteStructId {
    pub fn struct_id(&self, db: &dyn SemanticGroup) -> StructId {
        db.lookup_intern_concrete_struct(*self).struct_id
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteEnumLongId {
    pub enum_id: EnumId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
define_short_id!(ConcreteEnumId, ConcreteEnumLongId, SemanticGroup, lookup_intern_concrete_enum);
impl ConcreteEnumId {
    pub fn enum_id(&self, db: &dyn SemanticGroup) -> EnumId {
        db.lookup_intern_concrete_enum(*self).enum_id
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteExternTypeLongId {
    pub extern_type_id: ExternTypeId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
define_short_id!(
    ConcreteExternTypeId,
    ConcreteExternTypeLongId,
    SemanticGroup,
    lookup_intern_concrete_extern_type
);
impl ConcreteExternTypeId {
    pub fn extern_type_id(&self, db: &dyn SemanticGroup) -> ExternTypeId {
        db.lookup_intern_concrete_extern_type(*self).extern_type_id
    }
}

// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path.
pub fn resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, resolver, ty_syntax)
        .unwrap_or_else(|diag_added| TypeId::missing(db, diag_added))
}
pub fn maybe_resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> Maybe<TypeId> {
    let syntax_db = db.upcast();
    Ok(match ty_syntax {
        ast::Expr::Path(path) => {
            match resolver.resolve_concrete_path(diagnostics, path, NotFoundItemType::Type)? {
                ResolvedConcreteItem::Type(ty) => ty,
                _ => {
                    return Err(diagnostics.report(path, NotAType));
                }
            }
        }
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
            return Err(diagnostics.report(ty_syntax, UnknownType));
        }
    })
}

/// Query implementation of [crate::db::SemanticGroup::generic_type_generic_params].
pub fn generic_type_generic_params(
    db: &dyn SemanticGroup,
    generic_type: GenericTypeId,
) -> Maybe<Vec<GenericParamId>> {
    match generic_type {
        GenericTypeId::Struct(id) => db.struct_generic_params(id),
        GenericTypeId::Enum(id) => db.enum_generic_params(id),
        GenericTypeId::Extern(id) => db.extern_type_declaration_generic_params(id),
    }
}

pub fn substitute_generics(
    db: &dyn SemanticGroup,
    substitution: &HashMap<GenericParamId, GenericArgumentId>,
    ty: crate::TypeId,
) -> TypeId {
    match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete) => {
            db.intern_type(TypeLongId::Concrete(ConcreteTypeId::new(
                db,
                concrete.generic_type(db),
                concrete
                    .generic_args(db)
                    .iter()
                    .map(|generic_arg| match generic_arg {
                        GenericArgumentId::Type(ty) => {
                            GenericArgumentId::Type(substitute_generics(db, substitution, *ty))
                        }
                        GenericArgumentId::Literal(_) => *generic_arg,
                    })
                    .collect(),
            )))
        }
        TypeLongId::Tuple(tys) => db.intern_type(TypeLongId::Tuple(
            tys.into_iter().map(|ty| substitute_generics(db, substitution, ty)).collect(),
        )),
        TypeLongId::GenericParameter(generic_param) => substitution
            .get(&generic_param)
            .map(|generic_arg| match generic_arg {
                GenericArgumentId::Type(ty) => *ty,
                GenericArgumentId::Literal(_) => {
                    // TODO(ilya): Add diagnostics: "Expected type. Got literal"
                    TypeId::missing(db, skip_diagnostic())
                }
            })
            .unwrap_or(ty),
        TypeLongId::Missing(_) => ty,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct TypeInfo {
    /// Can the type be (trivially) dropped.
    pub droppable: bool,
    /// Can the type be (trivially) duplicated.
    pub duplicatable: bool,
}

/// Query implementation of [crate::db::SemanticGroup::type_info].
pub fn type_info(
    db: &dyn SemanticGroup,
    mut lookup_context: ImplLookupContext,
    ty: TypeId,
) -> Maybe<TypeInfo> {
    // TODO(spapini): Validate Copy and Drop for structs and enums.
    Ok(match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete_type_id) => {
            let module = concrete_type_id.generic_type(db).module(db.upcast());
            // Look for Copy and Drop trait also in the defining module.
            if !lookup_context.extra_modules.contains(&module) {
                lookup_context.extra_modules.push(module);
            }
            let droppable =
                !find_impls_at_context(db, &lookup_context, concrete_drop_trait(db, ty))?
                    .is_empty();
            let duplicatable =
                !find_impls_at_context(db, &lookup_context, concrete_copy_trait(db, ty))?
                    .is_empty();
            TypeInfo { droppable, duplicatable }
        }
        TypeLongId::Tuple(tys) => {
            let infos = tys
                .into_iter()
                .map(|ty| db.type_info(lookup_context.clone(), ty))
                .collect::<Maybe<Vec<_>>>()?;
            let droppable = infos.iter().all(|info| info.droppable);
            let duplicatable = infos.iter().all(|info| info.duplicatable);
            TypeInfo { droppable, duplicatable }
        }
        TypeLongId::GenericParameter(_) => todo!(),
        TypeLongId::Missing(diag_added) => {
            return Err(diag_added);
        }
    })
}
