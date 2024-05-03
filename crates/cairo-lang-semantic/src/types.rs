use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, GenericParamId, GenericTypeId, ImplDefId, ImplTypeDefId, ModuleFileId,
    NamedLanguageElementId, StructId, TraitTypeId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_syntax::attribute::consts::{MUST_USE_ATTR, PHANTOM_ATTR};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::{define_short_id, try_extract_matches, Intern, LookupIntern, OptionFrom};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::Zero;
use smol_str::SmolStr;

use crate::corelib::{
    concrete_copy_trait, concrete_destruct_trait, concrete_drop_trait,
    concrete_panic_destruct_trait, get_usize_ty,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::{Inference, InferenceData, InferenceError, InferenceId, TypeVar};
use crate::items::attribute::SemanticQueryAttrs;
use crate::items::constant::{resolve_const_expr_and_evaluate, ConstValue, ConstValueId};
use crate::items::imp::{ImplId, ImplLookupContext};
use crate::resolve::{ResolvedConcreteItem, Resolver};
use crate::substitution::SemanticRewriter;
use crate::{semantic, semantic_object_for_id, ConcreteTraitId, FunctionId, GenericArgumentId};

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum TypeLongId {
    Concrete(ConcreteTypeId),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId>),
    Snapshot(TypeId),
    GenericParameter(GenericParamId),
    Var(TypeVar),
    Coupon(FunctionId),
    FixedSizeArray {
        type_id: TypeId,
        size: ConstValueId,
    },
    ImplType(ImplTypeId),
    TraitType(TraitTypeId),
    Missing(#[dont_rewrite] DiagnosticAdded),
}
impl OptionFrom<TypeLongId> for ConcreteTypeId {
    fn option_from(other: TypeLongId) -> Option<Self> {
        try_extract_matches!(other, TypeLongId::Concrete)
    }
}

define_short_id!(TypeId, TypeLongId, SemanticGroup, lookup_intern_type, intern_type);
semantic_object_for_id!(TypeId, lookup_intern_type, intern_type, TypeLongId);
impl TypeId {
    pub fn missing(db: &dyn SemanticGroup, diag_added: DiagnosticAdded) -> Self {
        TypeLongId::Missing(diag_added).intern(db)
    }

    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        self.lookup_intern(db).format(db)
    }

    /// Returns [Maybe::Err] if the type is [TypeLongId::Missing].
    pub fn check_not_missing(&self, db: &dyn SemanticGroup) -> Maybe<()> {
        if let TypeLongId::Missing(diag_added) = self.lookup_intern(db) {
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
        matches!(self.lookup_intern(db), TypeLongId::Tuple(types) if types.is_empty())
    }

    /// Returns the [TypeHead] for a type if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<TypeHead> {
        self.lookup_intern(db).head(db)
    }

    /// Returns true if the type does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        db.priv_type_is_fully_concrete(*self)
    }

    /// Returns true if the type does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        db.priv_type_is_var_free(*self)
    }

    /// Returns whether the type is phantom.
    /// Type is considered phantom if it has the `#[phantom]` attribute, or is a tuple or fixed
    /// sized array containing it.
    pub fn is_phantom(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db).is_phantom(db)
    }
}
impl TypeLongId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        let def_db = db.upcast();
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
                format!("{}", generic_param.name(def_db).unwrap_or_else(|| "_".into()))
            }
            TypeLongId::ImplType(impl_type_id) => {
                format!("{}::{}", impl_type_id.impl_id.name(db), impl_type_id.ty.name(def_db))
            }
            TypeLongId::Var(var) => format!("?{}", var.id.0),
            TypeLongId::Coupon(function_id) => format!("{}::Coupon", function_id.full_name(db)),
            TypeLongId::Missing(_) => "<missing>".to_string(),
            TypeLongId::FixedSizeArray { type_id, size } => {
                format!("[{}; {:?}]", type_id.format(db), size.debug(db.elongate()))
            }
            TypeLongId::TraitType(trait_type_id) => {
                format!(
                    "{}::{}",
                    trait_type_id.trait_id(def_db).name(def_db),
                    trait_type_id.name(def_db)
                )
            }
        }
    }

    /// Returns the [TypeHead] for a type if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<TypeHead> {
        Some(match self {
            TypeLongId::Concrete(concrete) => TypeHead::Concrete(concrete.generic_type(db)),
            TypeLongId::Tuple(_) => TypeHead::Tuple,
            TypeLongId::Snapshot(inner) => TypeHead::Snapshot(Box::new(inner.head(db)?)),
            TypeLongId::Coupon(_) => TypeHead::Coupon,
            TypeLongId::FixedSizeArray { .. } => TypeHead::FixedSizeArray,
            TypeLongId::GenericParameter(_)
            | TypeLongId::Var(_)
            | TypeLongId::Missing(_)
            | TypeLongId::ImplType(_)
            | TypeLongId::TraitType(_) => {
                return None;
            }
        })
    }

    /// Returns whether the type is phantom.
    /// Type is considered phantom if it has the `#[phantom]` attribute, or is a tuple or fixed
    /// sized array containing it.
    pub fn is_phantom(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            TypeLongId::Concrete(id) => match id {
                ConcreteTypeId::Struct(id) => id.has_attr(db, PHANTOM_ATTR),
                ConcreteTypeId::Enum(id) => id.has_attr(db, PHANTOM_ATTR),
                ConcreteTypeId::Extern(id) => id.has_attr(db, PHANTOM_ATTR),
            }
            .unwrap_or_default(),
            TypeLongId::Tuple(inner) => inner.iter().any(|ty| ty.is_phantom(db)),
            TypeLongId::FixedSizeArray { type_id, .. } => type_id.is_phantom(db),
            TypeLongId::Snapshot(_)
            | TypeLongId::GenericParameter(_)
            | TypeLongId::Var(_)
            | TypeLongId::Coupon(_)
            | TypeLongId::TraitType(_)
            | TypeLongId::ImplType(_)
            | TypeLongId::Missing(_) => false,
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

/// Tries to implize a type, recursively, according to known inference data.
///
/// "Implization" is reducing a trait type or a wrapped trait type, to the more concrete type,
/// according to the assignment of that trait type in its impl, if the impl is known according to
/// the context.
///
/// This function uses the given `inference` as "read-only". That is, it doesn't add new inference
/// data, only uses the existing data.
///
/// `impl_ctx` is the impl context we're at, if any. That is, if we're inside an impl function, the
/// wrapping impl is the context here.
pub fn implize_type(
    db: &dyn SemanticGroup,
    type_to_reduce: TypeId,
    impl_ctx: Option<ImplDefId>,
    inference: &mut Inference<'_>,
) -> Maybe<TypeId> {
    // TODO(yuval): inline.
    implize_type_recursive(db, type_to_reduce, impl_ctx, inference)
}

/// Tries to implize a type, recursively, according to known inference data.
///
/// Assumes `inference.solve()` was called and doesn't change the inference structure (although
/// it's passed as &mut which is required per it's API).
///
/// `impl_ctx` is the impl context we're at, if any. That is, if we're inside an impl function, the
/// wrapping impl is the context here.
fn implize_type_recursive(
    db: &dyn SemanticGroup,
    type_to_reduce: TypeId,
    impl_ctx: Option<ImplDefId>,
    inference: &mut Inference<'_>,
) -> Maybe<TypeId> {
    // First, reduce if already inferred.
    let type_to_reduce = inference.rewrite(type_to_reduce).unwrap();

    // Then, reduce recursively.
    let mut long_ty = type_to_reduce.lookup_intern(db);
    match &mut long_ty {
        TypeLongId::Concrete(concrete_type) => {
            let mut generic_args = concrete_type.generic_args(db);
            for generic_arg in generic_args.iter_mut() {
                let GenericArgumentId::Type(generic_arg_type) = generic_arg else {
                    continue;
                };
                *generic_arg_type =
                    implize_type_recursive(db, *generic_arg_type, impl_ctx, inference)?;
                *generic_arg = GenericArgumentId::Type(*generic_arg_type);
            }
            concrete_type.modify_generic_args(db, generic_args);
        }
        TypeLongId::Tuple(types) => {
            for ty in types.iter_mut() {
                *ty = implize_type_recursive(db, *ty, impl_ctx, inference)?;
            }
        }
        TypeLongId::Snapshot(ty) => *ty = implize_type_recursive(db, *ty, impl_ctx, inference)?,
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::Coupon(_)
        | TypeLongId::Missing(_) => {}
        TypeLongId::FixedSizeArray { type_id, .. } => {
            *type_id = implize_type_recursive(db, *type_id, impl_ctx, inference)?
        }
        TypeLongId::TraitType(ty) => {
            let Some(impl_ctx) = impl_ctx else {
                // Nothing to implize. No need to report anything as it should be reported during
                // resolution.
                return Ok(type_to_reduce);
            };
            return Ok(db.trait_type_implized_by_context(*ty, impl_ctx)?.unwrap_or(type_to_reduce));
        }
    }
    let type_to_reduce = long_ty.intern(db);

    // Finally, reduce/implize the impl type itself, if possible.

    let TypeLongId::ImplType(mut impl_type_id) = type_to_reduce.lookup_intern(db) else {
        // Nothing to implize.
        return Ok(type_to_reduce);
    };

    // Try to reduce the impl type if its impl is an ImplVar (by reducing its impl).
    impl_type_id = reduce_trait_impl_type(impl_type_id, inference);

    // Try to implize the impl type if its impl is concrete.
    if let Some(ty) = db.impl_type_concrete_implized(impl_type_id)? {
        return Ok(ty);
    }

    // Try to implize by the impl context, if given. E.g. for `Self::MyType` inside an impl.
    if let Some(impl_def_id) = impl_ctx {
        if let Some(ty) = db.trait_type_implized_by_context(impl_type_id.ty(), impl_def_id)? {
            return Ok(ty);
        }
    }

    // Could not reduce.
    Ok(type_to_reduce)
}

/// Reduces an impl type if its impl is an ImplVar. E.g. in the case of MyTrait::MyType when there
/// is only a single impl for MyTrait in the context.
///
/// Assumes the given `inference.solve()` was called.
fn reduce_trait_impl_type(impl_type_id: ImplTypeId, inference: &mut Inference<'_>) -> ImplTypeId {
    let ImplTypeId { impl_id, ty } = impl_type_id;
    if !matches!(impl_id, crate::items::imp::ImplId::ImplVar(_)) {
        return impl_type_id;
    };

    let impl_id = inference.rewrite(impl_id).unwrap();

    ImplTypeId { impl_id, ty }
}

/// Head of a type. A type that is not one of {generic param, type variable, impl type} has a head,
/// which represents the kind of the root node in its type tree. This is used for caching queries
/// for fast lookups when the type is not completely inferred yet.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeHead {
    Concrete(GenericTypeId),
    Snapshot(Box<TypeHead>),
    Tuple,
    Coupon,
    FixedSizeArray,
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
                ConcreteStructLongId { struct_id: id, generic_args }.intern(db),
            ),
            GenericTypeId::Enum(id) => {
                ConcreteTypeId::Enum(ConcreteEnumLongId { enum_id: id, generic_args }.intern(db))
            }
            GenericTypeId::Extern(id) => ConcreteTypeId::Extern(
                ConcreteExternTypeLongId { extern_type_id: id, generic_args }.intern(db),
            ),
        }
    }
    pub fn generic_type(&self, db: &dyn SemanticGroup) -> GenericTypeId {
        match self {
            ConcreteTypeId::Struct(id) => GenericTypeId::Struct(id.lookup_intern(db).struct_id),
            ConcreteTypeId::Enum(id) => GenericTypeId::Enum(id.lookup_intern(db).enum_id),
            ConcreteTypeId::Extern(id) => {
                GenericTypeId::Extern(id.lookup_intern(db).extern_type_id)
            }
        }
    }
    pub fn generic_args(&self, db: &dyn SemanticGroup) -> Vec<semantic::GenericArgumentId> {
        match self {
            ConcreteTypeId::Struct(id) => id.lookup_intern(db).generic_args,
            ConcreteTypeId::Enum(id) => id.lookup_intern(db).generic_args,
            ConcreteTypeId::Extern(id) => id.lookup_intern(db).generic_args,
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

    /// Returns whether the type has the `#[must_use]` attribute.
    pub fn is_must_use(&self, db: &dyn SemanticGroup) -> Maybe<bool> {
        match self {
            ConcreteTypeId::Struct(id) => id.has_attr(db, MUST_USE_ATTR),
            ConcreteTypeId::Enum(id) => id.has_attr(db, MUST_USE_ATTR),
            ConcreteTypeId::Extern(id) => id.has_attr(db, MUST_USE_ATTR),
        }
    }
    /// Returns true if the type does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        self.generic_args(db)
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the type does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.generic_args(db).iter().all(|generic_argument_id| generic_argument_id.is_var_free(db))
    }

    /// Modifies the generic arguments of the type to the given `new_generic_args`.
    fn modify_generic_args(
        &mut self,
        db: &dyn SemanticGroup,
        new_generic_args: Vec<GenericArgumentId>,
    ) {
        match self {
            ConcreteTypeId::Struct(id) => {
                let long_id = id.lookup_intern(db);
                let new_long_id =
                    ConcreteStructLongId { generic_args: new_generic_args, ..long_id };
                *self = ConcreteTypeId::Struct(new_long_id.intern(db));
            }
            ConcreteTypeId::Enum(id) => {
                let long_id = id.lookup_intern(db);
                let new_long_id = ConcreteEnumLongId { generic_args: new_generic_args, ..long_id };
                *self = ConcreteTypeId::Enum(new_long_id.intern(db));
            }
            ConcreteTypeId::Extern(id) => {
                let long_id = id.lookup_intern(db);
                let new_long_id =
                    ConcreteExternTypeLongId { generic_args: new_generic_args, ..long_id };
                *self = ConcreteTypeId::Extern(new_long_id.intern(db));
            }
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
    lookup_intern_concrete_struct,
    intern_concrete_struct
);
semantic_object_for_id!(
    ConcreteStructId,
    lookup_intern_concrete_struct,
    intern_concrete_struct,
    ConcreteStructLongId
);
impl ConcreteStructId {
    pub fn struct_id(&self, db: &dyn SemanticGroup) -> StructId {
        self.lookup_intern(db).struct_id
    }
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteStructLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", ConcreteTypeId::Struct(self.clone().intern(db)).debug(db))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteEnumLongId {
    pub enum_id: EnumId,
    pub generic_args: Vec<semantic::GenericArgumentId>,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteEnumLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", ConcreteTypeId::Enum(self.clone().intern(db)).debug(db))
    }
}

define_short_id!(
    ConcreteEnumId,
    ConcreteEnumLongId,
    SemanticGroup,
    lookup_intern_concrete_enum,
    intern_concrete_enum
);
semantic_object_for_id!(
    ConcreteEnumId,
    lookup_intern_concrete_enum,
    intern_concrete_enum,
    ConcreteEnumLongId
);
impl ConcreteEnumId {
    pub fn enum_id(&self, db: &dyn SemanticGroup) -> EnumId {
        self.lookup_intern(db).enum_id
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
    lookup_intern_concrete_extern_type,
    intern_concrete_extern_type
);
semantic_object_for_id!(
    ConcreteExternTypeId,
    lookup_intern_concrete_extern_type,
    intern_concrete_extern_type,
    ConcreteExternTypeLongId
);
impl ConcreteExternTypeId {
    pub fn extern_type_id(&self, db: &dyn SemanticGroup) -> ExternTypeId {
        self.lookup_intern(db).extern_type_id
    }
}

/// An impl item of kind type.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplTypeId {
    /// The impl the item type is in.
    impl_id: ImplId,
    /// The trait type this impl type "implements".
    ty: TraitTypeId,
}
impl ImplTypeId {
    /// Creates a new impl type id. For an impl type of a concrete impl, asserts that the trait
    /// type belongs to the same trait that the impl implements (panics if not).
    pub fn new(impl_id: ImplId, ty: TraitTypeId, db: &dyn SemanticGroup) -> Self {
        if let crate::items::imp::ImplId::Concrete(concrete_impl) = impl_id {
            let impl_def_id = concrete_impl.impl_def_id(db);
            assert_eq!(Ok(ty.trait_id(db.upcast())), db.impl_def_trait(impl_def_id));
        }

        ImplTypeId { impl_id, ty }
    }
    pub fn impl_id(&self) -> ImplId {
        self.impl_id
    }
    pub fn ty(&self) -> TraitTypeId {
        self.ty
    }
    /// Gets the impl type def (language element), if `self.impl_id` is of a concrete impl.
    pub fn impl_type_def(&self, db: &dyn SemanticGroup) -> Maybe<Option<ImplTypeDefId>> {
        match self.impl_id {
            ImplId::Concrete(concrete_impl_id) => concrete_impl_id.get_impl_type_def(db, self.ty),
            ImplId::GenericParameter(_) | ImplId::ImplVar(_) => Ok(None),
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> SmolStr {
        format!("{}::{}", self.impl_id.name(db.upcast()), self.ty.name(db.upcast())).into()
    }
}
impl DebugWithDb<dyn SemanticGroup> for ImplTypeId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
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
            TypeLongId::Tuple(sub_tys).intern(db)
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(syntax_db), ast::UnaryOperator::At(_)) =>
        {
            let ty = resolve_type(db, diagnostics, resolver, &unary_syntax.expr(syntax_db));
            TypeLongId::Snapshot(ty).intern(db)
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(syntax_db), ast::UnaryOperator::Desnap(_)) =>
        {
            let ty = resolve_type(db, diagnostics, resolver, &unary_syntax.expr(syntax_db));
            if let Some(desnapped_ty) =
                try_extract_matches!(ty.lookup_intern(db), TypeLongId::Snapshot)
            {
                desnapped_ty
            } else {
                return Err(diagnostics.report(ty_syntax, DesnapNonSnapshot));
            }
        }
        ast::Expr::FixedSizeArray(array_syntax) => {
            let [ty] = &array_syntax.exprs(syntax_db).elements(syntax_db)[..] else {
                return Err(diagnostics.report(ty_syntax, FixedSizeArrayTypeNonSingleType));
            };
            let ty = resolve_type(db, diagnostics, resolver, ty);
            let size = match extract_fixed_size_array_size(db, diagnostics, array_syntax, resolver)?
            {
                Some(size) => size,
                None => {
                    return Err(diagnostics.report(ty_syntax, FixedSizeArrayTypeEmptySize));
                }
            };
            TypeLongId::FixedSizeArray { type_id: ty, size }.intern(db)
        }
        _ => {
            return Err(diagnostics.report(ty_syntax, UnknownType));
        }
    })
}

/// Extracts the size of a fixed size array, or none if the size is missing. Reports an error if the
/// size is not a numeric literal.
pub fn extract_fixed_size_array_size(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    syntax: &ast::ExprFixedSizeArray,
    resolver: &Resolver<'_>,
) -> Maybe<Option<ConstValueId>> {
    let syntax_db = db.upcast();
    match syntax.size(syntax_db) {
        ast::OptionFixedSizeArraySize::FixedSizeArraySize(size_clause) => {
            let environment = Environment::empty();
            let resolver = Resolver::with_data(
                db,
                (resolver.data).clone_with_inference_id(db, resolver.inference_data.inference_id),
            );
            let mut ctx =
                ComputationContext::new(db, diagnostics, None, resolver, None, environment);
            let size_expr_syntax = size_clause.size(syntax_db);
            let size = compute_expr_semantic(&mut ctx, &size_expr_syntax, None);
            let (_, const_value) = resolve_const_expr_and_evaluate(
                db,
                &mut ctx,
                &size,
                size_expr_syntax.stable_ptr().untyped(),
                get_usize_ty(db),
            );
            match &const_value {
                ConstValue::Int(_) => Ok(Some(const_value.intern(db))),
                ConstValue::Generic(_) => Ok(Some(const_value.intern(db))),

                _ => Err(diagnostics.report(syntax, FixedSizeArrayNonNumericSize)),
            }
        }
        ast::OptionFixedSizeArraySize::Empty(_) => Ok(None),
    }
}

/// Verifies that a given fixed size array size is within limits, and adds a diagnostic if not.
pub fn verify_fixed_size_array_size(
    diagnostics: &mut SemanticDiagnostics,
    size: &BigInt,
    syntax: &ast::ExprFixedSizeArray,
) -> Maybe<()> {
    if size > &BigInt::from(i16::MAX) {
        return Err(diagnostics.report(syntax, FixedSizeArraySizeTooBig));
    }
    Ok(())
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
    pub droppable: Result<ImplId, InferenceError>,
    pub copyable: Result<ImplId, InferenceError>,
    pub destruct_impl: Result<ImplId, InferenceError>,
    pub panic_destruct_impl: Result<ImplId, InferenceError>,
}

/// Checks if there is at least one impl that can be inferred for a specific concrete trait.
pub fn get_impl_at_context(
    db: &dyn SemanticGroup,
    lookup_context: ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
    stable_ptr: Option<SyntaxStablePtrId>,
) -> Result<ImplId, InferenceError> {
    let mut inference_data = InferenceData::new(InferenceId::NoContext);
    let mut inference = inference_data.inference(db);
    // It's ok to consume the errors without reporting as this is a helper function meant to find an
    // impl and return it, but it's ok if the impl can't be found.
    let impl_id = inference.new_impl_var(concrete_trait_id, stable_ptr, lookup_context).map_err(
        |err_set| {
            inference
                .consume_error_without_reporting(err_set)
                .expect("Error couldn't be already consumed")
        },
    )?;
    if let Err((err_set, _)) = inference.finalize_without_reporting() {
        return Err(inference
            .consume_error_without_reporting(err_set)
            .expect("Error couldn't be already consumed"));
    };
    Ok(inference.rewrite(impl_id).no_err())
}

/// Query implementation of [crate::db::SemanticGroup::single_value_type].
pub fn single_value_type(db: &dyn SemanticGroup, ty: TypeId) -> Maybe<bool> {
    Ok(match ty.lookup_intern(db) {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(id) => {
                for member in db.struct_members(id.struct_id(db))?.values() {
                    if !db.single_value_type(member.ty)? {
                        return Ok(false);
                    }
                }
                true
            }
            ConcreteTypeId::Enum(id) => {
                let variants = db.enum_variants(id.enum_id(db))?;
                if variants.len() != 1 {
                    return Ok(false);
                }

                db.single_value_type(
                    db.variant_semantic(id.enum_id(db), *variants.values().next().unwrap())?.ty,
                )?
            }
            ConcreteTypeId::Extern(_) => false,
        },
        TypeLongId::Tuple(types) => {
            for ty in &types {
                if !db.single_value_type(*ty)? {
                    return Ok(false);
                }
            }
            true
        }
        TypeLongId::Snapshot(ty) => db.single_value_type(ty)?,
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::Missing(_)
        | TypeLongId::Coupon(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::TraitType(_) => false,
        TypeLongId::FixedSizeArray { type_id, size } => {
            db.single_value_type(type_id)?
                || matches!(size.lookup_intern(db),
                            ConstValue::Int(value) if value.is_zero())
        }
    })
}

/// Adds diagnostics for a type, post semantic analysis of types.
pub fn add_type_based_diagnostics(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    ty: TypeId,
    stable_ptr: impl Into<SyntaxStablePtrId> + Copy,
) {
    if db.type_size_info(ty) == Ok(TypeSizeInformation::Infinite) {
        diagnostics.report(stable_ptr, InfiniteSizeType(ty));
    }
    if let TypeLongId::Concrete(ConcreteTypeId::Extern(extrn)) = ty.lookup_intern(db) {
        let long_id = extrn.lookup_intern(db);
        if long_id.extern_type_id.name(db.upcast()).as_str() == "Array" {
            if let [GenericArgumentId::Type(arg_ty)] = &long_id.generic_args[..] {
                if db.type_size_info(*arg_ty) == Ok(TypeSizeInformation::ZeroSized) {
                    diagnostics.report(stable_ptr, ArrayOfZeroSizedElements(*arg_ty));
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSizeInformation {
    /// The type has an infinite size - caused by a recursion in it.
    /// If the type simply holds an infinite type, it would be considered `Other`, for diagnostics
    /// reasons.
    Infinite,
    /// The type is zero size.
    ZeroSized,
    /// The typed has some none zero size.
    Other,
}

/// Query implementation of [crate::db::SemanticGroup::type_size_info].
pub fn type_size_info(db: &dyn SemanticGroup, ty: TypeId) -> Maybe<TypeSizeInformation> {
    match ty.lookup_intern(db) {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(id) => {
                let mut zero_sized = true;
                for (_, member) in db.struct_members(id.struct_id(db))? {
                    if db.type_size_info(member.ty)? != TypeSizeInformation::ZeroSized {
                        zero_sized = false;
                    }
                }
                if zero_sized {
                    return Ok(TypeSizeInformation::ZeroSized);
                }
            }
            ConcreteTypeId::Enum(id) => {
                for (_, variant) in db.enum_variants(id.enum_id(db))? {
                    // Recursive calling in order to find infinite sized types.
                    db.type_size_info(db.variant_semantic(id.enum_id(db), variant)?.ty)?;
                }
            }
            ConcreteTypeId::Extern(_) => {}
        },
        TypeLongId::Tuple(types) => {
            let mut zero_sized = true;
            for ty in types {
                if db.type_size_info(ty)? != TypeSizeInformation::ZeroSized {
                    zero_sized = false;
                }
            }
            if zero_sized {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
        TypeLongId::Snapshot(ty) => {
            if db.type_size_info(ty)? == TypeSizeInformation::ZeroSized {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
        TypeLongId::Coupon(_) => return Ok(TypeSizeInformation::ZeroSized),
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::Missing(_)
        | TypeLongId::TraitType(_)
        | TypeLongId::ImplType(_) => {}
        TypeLongId::FixedSizeArray { type_id, size } => {
            if matches!(size.lookup_intern(db), ConstValue::Int(value) if value.is_zero())
                || db.type_size_info(type_id)? == TypeSizeInformation::ZeroSized
            {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
    }
    Ok(TypeSizeInformation::Other)
}

/// Cycle handling of [crate::db::SemanticGroup::type_size_info].
pub fn type_size_info_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &[String],
    _ty: &TypeId,
) -> Maybe<TypeSizeInformation> {
    Ok(TypeSizeInformation::Infinite)
}

// TODO(spapini): type info lookup for non generic types needs to not depend on lookup_context.
// This is to ensure that sierra generator will see a consistent type info of types.
/// Query implementation of [crate::db::SemanticGroup::type_info].
pub fn type_info(
    db: &dyn SemanticGroup,
    lookup_context: ImplLookupContext,
    ty: TypeId,
) -> Maybe<TypeInfo> {
    // Dummy stable pointer for type inference variables, since inference is disabled.
    let droppable =
        get_impl_at_context(db, lookup_context.clone(), concrete_drop_trait(db, ty), None);
    let copyable =
        get_impl_at_context(db, lookup_context.clone(), concrete_copy_trait(db, ty), None);
    let destruct_impl =
        get_impl_at_context(db, lookup_context.clone(), concrete_destruct_trait(db, ty), None);
    let panic_destruct_impl =
        get_impl_at_context(db, lookup_context, concrete_panic_destruct_trait(db, ty), None);
    Ok(TypeInfo { droppable, copyable, destruct_impl, panic_destruct_impl })
}

pub fn priv_type_is_fully_concrete(db: &dyn SemanticGroup, ty: TypeId) -> bool {
    match ty.lookup_intern(db) {
        TypeLongId::Concrete(concrete_type_id) => concrete_type_id.is_fully_concrete(db),
        TypeLongId::Tuple(types) => types.iter().all(|ty| ty.is_fully_concrete(db)),
        TypeLongId::Snapshot(ty) => ty.is_fully_concrete(db),
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::Missing(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::TraitType(_) => false,
        TypeLongId::Coupon(function_id) => function_id.is_fully_concrete(db),
        TypeLongId::FixedSizeArray { type_id, size } => {
            type_id.is_fully_concrete(db) && size.is_fully_concrete(db)
        }
    }
}

pub fn priv_type_is_var_free(db: &dyn SemanticGroup, ty: TypeId) -> bool {
    match ty.lookup_intern(db) {
        TypeLongId::Concrete(concrete_type_id) => concrete_type_id.is_var_free(db),
        TypeLongId::Tuple(types) => types.iter().all(|ty| ty.is_var_free(db)),
        TypeLongId::Snapshot(ty) => ty.is_var_free(db),
        TypeLongId::Var(_) => false,
        TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) | TypeLongId::ImplType(_) => true,
        TypeLongId::Coupon(function_id) => function_id.is_var_free(db),
        TypeLongId::FixedSizeArray { type_id, size } => {
            type_id.is_var_free(db) && size.is_var_free(db)
        }
        TypeLongId::TraitType(_) => true,
    }
}

/// Peels all wrapping Snapshot (`@`) from the type.
/// Returns the number of peeled snapshots and the inner type.
pub fn peel_snapshots(db: &dyn SemanticGroup, ty: TypeId) -> (usize, TypeLongId) {
    let mut long_ty = ty.lookup_intern(db);
    let mut n_snapshots = 0;
    while let TypeLongId::Snapshot(ty) = long_ty {
        long_ty = ty.lookup_intern(db);
        n_snapshots += 1;
    }
    (n_snapshots, long_ty)
}

/// Wraps a type with Snapshot (`@`) `n_snapshots` times.
pub fn wrap_in_snapshots(db: &dyn SemanticGroup, mut ty: TypeId, n_snapshots: usize) -> TypeId {
    for _ in 0..n_snapshots {
        ty = TypeLongId::Snapshot(ty).intern(db);
    }
    ty
}

/// Returns `true` if coupons are enabled in the module.
pub(crate) fn are_coupons_enabled(db: &dyn SemanticGroup, module_file_id: ModuleFileId) -> bool {
    let owning_crate = module_file_id.0.owning_crate(db.upcast());
    let Some(config) = db.crate_config(owning_crate) else { return false };
    config.settings.experimental_features.coupons
}
