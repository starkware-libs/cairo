use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, StructId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_utils::{define_short_id, try_extract_matches, OptionFrom};
use itertools::Itertools;

use crate::corelib::{concrete_copy_trait, concrete_destruct_trait, concrete_drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::{InferenceResult, TypeVar};
use crate::items::imp::{get_impl_at_context, ImplId, ImplLookupContext};
use crate::resolve_path::{ResolvedConcreteItem, Resolver};
use crate::{semantic, semantic_object_for_id};

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum TypeLongId {
    Concrete(ConcreteTypeId),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    Snapshot(TypeId),
    GenericParameter(GenericParamId),
    Var(TypeVar),
    Missing(#[dont_rewrite] DiagnosticAdded),
}
impl OptionFrom<TypeLongId> for ConcreteTypeId {
    fn option_from(other: TypeLongId) -> Option<Self> {
        try_extract_matches!(other, TypeLongId::Concrete)
    }
}

define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type);
semantic_object_for_id!(TypeId, lookup_intern_type, intern_type, TypeLongId);
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

    /// Returns `true` if the type is `()`.
    pub fn is_unit(&self, db: &dyn SemanticGroup) -> bool {
        matches!(db.lookup_intern_type(*self), TypeLongId::Tuple(types) if types.is_empty())
    }

    /// Returns the [TypeHead] for a type if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<TypeHead> {
        db.lookup_intern_type(*self).head(db)
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
            TypeLongId::Snapshot(ty) => format!("@{}", ty.format(db)),
            TypeLongId::GenericParameter(generic_param) => {
                format!("{:?}", generic_param.debug(db.elongate()))
            }
            TypeLongId::Var(var) => format!("?{}", var.id),
            TypeLongId::Missing(_) => "<missing>".to_string(),
        }
    }

    /// Returns the [TypeHead] for a type if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<TypeHead> {
        Some(match self {
            TypeLongId::Concrete(concrete) => TypeHead::Concrete(concrete.generic_type(db)),
            TypeLongId::Tuple(_) => TypeHead::Tuple,
            TypeLongId::Snapshot(inner) => TypeHead::Snapshot(Box::new(inner.head(db)?)),
            TypeLongId::GenericParameter(_) | TypeLongId::Var(_) | TypeLongId::Missing(_) => {
                return None;
            }
        })
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

/// Head of a type. A non-param non-variable type has a head, which represents the kind of the root
/// node in its type tree. This is used for caching queries for fast lookups when the type is not
/// completely inferred yet.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeHead {
    Concrete(GenericTypeId),
    Snapshot(Box<TypeHead>),
    Tuple,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
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
                generic_args.iter().map(|arg| arg.format(db)).join(", ")
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

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
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
semantic_object_for_id!(
    ConcreteStructId,
    lookup_intern_concrete_struct,
    intern_concrete_struct,
    ConcreteStructLongId
);
impl ConcreteStructId {
    pub fn struct_id(&self, db: &dyn SemanticGroup) -> StructId {
        db.lookup_intern_concrete_struct(*self).struct_id
    }
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteStructLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", ConcreteTypeId::Struct(db.intern_concrete_struct(self.clone())).debug(db))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteEnumLongId {
    pub enum_id: EnumId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
define_short_id!(ConcreteEnumId, ConcreteEnumLongId, SemanticGroup, lookup_intern_concrete_enum);
semantic_object_for_id!(
    ConcreteEnumId,
    lookup_intern_concrete_enum,
    intern_concrete_enum,
    ConcreteEnumLongId
);
impl ConcreteEnumId {
    pub fn enum_id(&self, db: &dyn SemanticGroup) -> EnumId {
        db.lookup_intern_concrete_enum(*self).enum_id
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
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
semantic_object_for_id!(
    ConcreteExternTypeId,
    lookup_intern_concrete_extern_type,
    intern_concrete_extern_type,
    ConcreteExternTypeLongId
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
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(syntax_db), ast::UnaryOperator::At(_)) =>
        {
            let ty = resolve_type(db, diagnostics, resolver, &unary_syntax.expr(syntax_db));
            db.intern_type(TypeLongId::Snapshot(ty))
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(syntax_db), ast::UnaryOperator::Desnap(_)) =>
        {
            let ty = resolve_type(db, diagnostics, resolver, &unary_syntax.expr(syntax_db));
            if let Some(desnapped_ty) =
                try_extract_matches!(db.lookup_intern_type(ty), TypeLongId::Snapshot)
            {
                desnapped_ty
            } else {
                return Err(diagnostics.report(ty_syntax, DesnapNonSnapshot));
            }
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
) -> Maybe<Vec<semantic::GenericParam>> {
    match generic_type {
        GenericTypeId::Struct(id) => db.struct_generic_params(id),
        GenericTypeId::Enum(id) => db.enum_generic_params(id),
        GenericTypeId::Extern(id) => db.extern_type_declaration_generic_params(id),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeInfo {
    pub droppable: InferenceResult<()>,
    pub duplicatable: InferenceResult<()>,
    pub destruct_impl: InferenceResult<ImplId>,
}

// TODO(spapini): type info lookup for non generic types needs to not depend on lookup_context.
// This is to ensure that sierra genreator will see a consistent type info of types.
/// Query implementation of [crate::db::SemanticGroup::type_info].
pub fn type_info(
    db: &dyn SemanticGroup,
    mut lookup_context: ImplLookupContext,
    ty: TypeId,
) -> Maybe<TypeInfo> {
    // Dummy stable pointer for type inference variables, since inference is disabled.
    let stable_ptr = db.intern_stable_ptr(SyntaxStablePtr::Root);
    let destruct_impl = get_impl_at_context(
        db,
        lookup_context.clone(),
        concrete_destruct_trait(db, ty),
        stable_ptr,
    );
    Ok(match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete_type_id) => {
            let module = concrete_type_id.generic_type(db).parent_module(db.upcast());
            // Look for Copy and Drop trait also in the defining module.
            if !lookup_context.extra_modules.contains(&module) {
                lookup_context.extra_modules.push(module);
            }
            let droppable = get_impl_at_context(
                db,
                lookup_context.clone(),
                concrete_drop_trait(db, ty),
                stable_ptr,
            )
            .map(|_| ());
            let duplicatable = get_impl_at_context(
                db,
                lookup_context.clone(),
                concrete_copy_trait(db, ty),
                stable_ptr,
            )
            .map(|_| ());
            TypeInfo { droppable, duplicatable, destruct_impl }
        }
        TypeLongId::GenericParameter(_) => {
            let droppable = get_impl_at_context(
                db,
                lookup_context.clone(),
                concrete_drop_trait(db, ty),
                stable_ptr,
            )
            .map(|_| ());
            let duplicatable = get_impl_at_context(
                db,
                lookup_context.clone(),
                concrete_copy_trait(db, ty),
                stable_ptr,
            )
            .map(|_| ());

            TypeInfo { droppable, duplicatable, destruct_impl }
        }
        TypeLongId::Tuple(tys) => {
            let infos = tys
                .into_iter()
                .map(|ty| db.type_info(lookup_context.clone(), ty))
                .collect::<Maybe<Vec<_>>>()?;
            let droppable = if let Some(err) =
                infos.iter().filter_map(|info| info.droppable.clone().err()).next()
            {
                Err(err)
            } else {
                Ok(())
            };
            let duplicatable = if let Some(err) =
                infos.iter().filter_map(|info| info.duplicatable.clone().err()).next()
            {
                Err(err)
            } else {
                Ok(())
            };
            TypeInfo { droppable, duplicatable, destruct_impl }
        }
        TypeLongId::Var(_) => panic!("Types should be fully resolved at this point."),
        TypeLongId::Missing(diag_added) => {
            return Err(diag_added);
        }
        TypeLongId::Snapshot(_) => {
            TypeInfo { droppable: Ok(()), duplicatable: Ok(()), destruct_impl }
        }
    })
}

/// Peels all wrapping Snapshot (`@`) from the type.
/// Returns the number of peeled snapshots and the inner type.
pub fn peel_snapshots(db: &dyn SemanticGroup, ty: TypeId) -> (usize, TypeLongId) {
    let mut long_ty = db.lookup_intern_type(ty);
    let mut n_snapshots = 0;
    while let TypeLongId::Snapshot(ty) = long_ty {
        long_ty = db.lookup_intern_type(ty);
        n_snapshots += 1;
    }
    (n_snapshots, long_ty)
}

/// Wraps a type with Snapshot (`@`) `n_snapshots` times.
pub fn wrap_in_snapshots(db: &dyn SemanticGroup, mut ty: TypeId, n_snapshots: usize) -> TypeId {
    for _ in 0..n_snapshots {
        ty = db.intern_type(TypeLongId::Snapshot(ty));
    }
    ty
}
