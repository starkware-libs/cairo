use std::collections::HashMap;

use db_utils::define_short_id;
use debug::DebugWithDb;
use defs::ids::{EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, StructId};
use itertools::Itertools;
use syntax::node::ast;
use utils::OptionFrom;

use crate::corelib::{concrete_copy_trait, concrete_drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
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
    Never,
    Missing,
}
impl OptionFrom<TypeLongId> for ConcreteTypeId {
    fn option_from(other: TypeLongId) -> Option<Self> {
        if let TypeLongId::Concrete(res) = other { Some(res) } else { None }
    }
}

define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type);
impl TypeId {
    pub fn missing(db: &dyn SemanticGroup) -> Self {
        db.intern_type(TypeLongId::Missing)
    }
    pub fn never(db: &dyn SemanticGroup) -> Self {
        db.intern_type(TypeLongId::Never)
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        db.lookup_intern_type(*self).format(db)
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
            TypeLongId::Never => "<never>".to_string(),
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
impl PartialOrd for TypeId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl Ord for TypeId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
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
        ast::Expr::Path(path) => match resolver.resolve_concrete_path(diagnostics, path)? {
            ResolvedConcreteItem::Type(ty) => ty,
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
                    .map(|generic_arg| {
                        let GenericArgumentId::Type(ty) = generic_arg;
                        GenericArgumentId::Type(substitute_generics(db, substitution, *ty))
                    })
                    .collect(),
            )))
        }
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
        TypeLongId::Missing | TypeLongId::Never => ty,
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
) -> Option<TypeInfo> {
    // TODO(spapini): Validate Copy and Drop for structs and enums.
    Some(match db.lookup_intern_type(ty) {
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
                .collect::<Option<Vec<_>>>()?;
            let droppable = infos.iter().all(|info| info.droppable);
            let duplicatable = infos.iter().all(|info| info.duplicatable);
            TypeInfo { droppable, duplicatable }
        }
        TypeLongId::GenericParameter(_) => todo!(),
        TypeLongId::Never => TypeInfo { droppable: true, duplicatable: true },
        TypeLongId::Missing => {
            return None;
        }
    })
}
