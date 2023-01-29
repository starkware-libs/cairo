use std::ops::Deref;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, StructId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{define_short_id, extract_matches, OptionFrom};
use itertools::Itertools;

use crate::corelib::{concrete_copy_trait, concrete_drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::{Inference, TypeVar};
use crate::items::imp::{find_impls_at_context, ImplLookupContext};
use crate::resolve_path::{ResolvedConcreteItem, Resolver};
use crate::{semantic, ConcreteImplId, ConcreteVariant, FunctionId, GenericArgumentId};

/// A substitution of generic arguments in generic parameters. Used for concretization.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GenericSubstitution(pub OrderedHashMap<GenericParamId, GenericArgumentId>);
impl Deref for GenericSubstitution {
    type Target = OrderedHashMap<GenericParamId, GenericArgumentId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
#[allow(clippy::derive_hash_xor_eq)]
impl std::hash::Hash for GenericSubstitution {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().collect_vec().hash(state);
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeLongId {
    Concrete(ConcreteTypeId),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    GenericParameter(GenericParamId),
    Var(TypeVar),
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
            TypeLongId::Var(var) => format!("?{}", var.id),
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
pub fn resolve_type_with_inference(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, inference, resolver, ty_syntax)
        .unwrap_or_else(|diag_added| TypeId::missing(db, diag_added))
}
pub fn resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, &mut Inference::disabled(db), resolver, ty_syntax)
        .unwrap_or_else(|diag_added| TypeId::missing(db, diag_added))
}
pub fn maybe_resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> Maybe<TypeId> {
    let syntax_db = db.upcast();
    Ok(match ty_syntax {
        ast::Expr::Path(path) => {
            match resolver.resolve_concrete_path(
                diagnostics,
                inference,
                path,
                NotFoundItemType::Type,
            )? {
                ResolvedConcreteItem::Type(ty) => ty,
                _ => {
                    return Err(diagnostics.report(path, NotAType));
                }
            }
        }
        ast::Expr::Parenthesized(expr_syntax) => resolve_type_with_inference(
            db,
            diagnostics,
            inference,
            resolver,
            &expr_syntax.expr(syntax_db),
        ),
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|subexpr_syntax| {
                    resolve_type_with_inference(
                        db,
                        diagnostics,
                        inference,
                        resolver,
                        &subexpr_syntax,
                    )
                })
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

pub fn substitute_ty(
    db: &dyn SemanticGroup,
    substitution: &GenericSubstitution,
    ty: TypeId,
) -> TypeId {
    match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete) => {
            db.intern_type(TypeLongId::Concrete(ConcreteTypeId::new(
                db,
                concrete.generic_type(db),
                substitute_generics_args(db, substitution, concrete.generic_args(db)),
            )))
        }
        TypeLongId::Tuple(tys) => db.intern_type(TypeLongId::Tuple(
            tys.into_iter().map(|ty| substitute_ty(db, substitution, ty)).collect(),
        )),
        TypeLongId::GenericParameter(generic_param) => substitution
            .get(&generic_param)
            .map(|generic_arg| *extract_matches!(generic_arg, GenericArgumentId::Type))
            .unwrap_or(ty),
        TypeLongId::Var(_) => panic!("Types should be fully resolved at this point."),
        TypeLongId::Missing(_) => ty,
    }
}

/// Substituted generics in a [FunctionId].
pub fn substitute_function(
    db: &dyn SemanticGroup,
    substitution: &GenericSubstitution,
    function: &mut FunctionId,
) {
    let mut long_function = db.lookup_intern_function(*function);
    substitute_generics_args_inplace(db, substitution, &mut long_function.function.generic_args);
    long_function.function.generic_function.generic_args_apply(db, |generic_args| {
        substitute_generics_args_inplace(db, substitution, generic_args)
    });
    *function = db.intern_function(long_function);
}

/// Substituted generics in a [ConcreteVariant].
pub fn substitute_variant(
    db: &dyn SemanticGroup,
    substitution: &GenericSubstitution,
    variant: &mut ConcreteVariant,
) {
    variant.ty = substitute_ty(db.upcast(), substitution, variant.ty);
    let mut long_concrete_enum = db.lookup_intern_concrete_enum(variant.concrete_enum_id);
    substitute_generics_args_inplace(db, substitution, &mut long_concrete_enum.generic_args);
    variant.concrete_enum_id = db.intern_concrete_enum(long_concrete_enum);
}

/// Substitutes generics in a slice of [GenericArgumentId].
pub fn substitute_generics_args_inplace(
    db: &dyn SemanticGroup,
    substitution: &GenericSubstitution,
    generic_args: &mut [GenericArgumentId],
) {
    for arg in generic_args.iter_mut() {
        match arg {
            GenericArgumentId::Type(ty) => *ty = substitute_ty(db.upcast(), substitution, *ty),
            GenericArgumentId::Literal(_) => {}
            GenericArgumentId::Impl(concrete_impl) => {
                *concrete_impl = substitute_impl(db.upcast(), substitution, *concrete_impl)
            }
        }
    }
}

/// Substituted generics in a vector of [GenericArgumentId]s and returns the new vector.
pub fn substitute_generics_args(
    db: &dyn SemanticGroup,
    substitution: &GenericSubstitution,
    mut generic_args: Vec<GenericArgumentId>,
) -> Vec<GenericArgumentId> {
    substitute_generics_args_inplace(db, substitution, &mut generic_args);
    generic_args
}

/// Substituted generics in a [ConcreteImplId].
fn substitute_impl(
    db: &dyn SemanticGroup,
    substitution: &GenericSubstitution,
    concrete_impl: ConcreteImplId,
) -> ConcreteImplId {
    let mut long_concrete_impl = db.lookup_intern_concrete_impl(concrete_impl);
    substitute_generics_args_inplace(db, substitution, &mut long_concrete_impl.generic_args);
    // TODO(spapini): One of the options for ConcreteImpl should be a Generic Impl Param.
    // When this happens, handle it here.
    db.intern_concrete_impl(long_concrete_impl)
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
    let inference = Inference::disabled(db);
    // Dummy stable pointer for type inference variables, since inference is disabled.
    let stable_ptr = db.intern_stable_ptr(SyntaxStablePtr::Root);
    Ok(match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete_type_id) => {
            let module = concrete_type_id.generic_type(db).parent_module(db.upcast());
            // Look for Copy and Drop trait also in the defining module.
            if !lookup_context.extra_modules.contains(&module) {
                lookup_context.extra_modules.push(module);
            }
            let droppable = !find_impls_at_context(
                db,
                &inference,
                &lookup_context,
                concrete_drop_trait(db, ty),
                stable_ptr,
            )?
            .is_empty();
            let duplicatable = !find_impls_at_context(
                db,
                &inference,
                &lookup_context,
                concrete_copy_trait(db, ty),
                stable_ptr,
            )?
            .is_empty();
            TypeInfo { droppable, duplicatable }
        }
        TypeLongId::GenericParameter(_) => {
            let droppable = !find_impls_at_context(
                db,
                &inference,
                &lookup_context,
                concrete_drop_trait(db, ty),
                stable_ptr,
            )?
            .is_empty();
            let duplicatable = !find_impls_at_context(
                db,
                &inference,
                &lookup_context,
                concrete_copy_trait(db, ty),
                stable_ptr,
            )?
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
        TypeLongId::Var(_) => panic!("Types should be fully resolved at this point."),
        TypeLongId::Missing(diag_added) => {
            return Err(diag_added);
        }
    })
}
