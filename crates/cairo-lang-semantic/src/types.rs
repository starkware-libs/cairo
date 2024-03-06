use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, GenericParamId, GenericTypeId, ImplDefId, ImplTypeDefId, ModuleFileId,
    NamedLanguageElementId, StructId, TraitOrImplContext, TraitTypeId,
};
use cairo_lang_diagnostics::{skip_diagnostic, DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_syntax::attribute::consts::{MUST_USE_ATTR, UNSTABLE_ATTR};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::{define_short_id, extract_matches, try_extract_matches, OptionFrom};
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::Zero;
use smol_str::SmolStr;

use crate::corelib::{
    concrete_copy_trait, concrete_destruct_trait, concrete_drop_trait,
    concrete_panic_destruct_trait, core_bool_ty, get_usize_ty,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::{InferenceData, InferenceId, InferenceResult, TypeVar};
use crate::items::attribute::SemanticQueryAttrs;
use crate::items::constant::{resolve_const_expr_and_evaluate, ConstValue, ConstValueId};
use crate::items::imp::{ImplId, ImplLookupContext};
use crate::items::trt::ConcreteTraitTypeLongId;
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
    pub fn lookup(&self, db: &dyn SemanticGroup) -> TypeLongId {
        db.lookup_intern_type(*self)
    }

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

    /// Returns true if the type does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        match db.lookup_intern_type(*self) {
            TypeLongId::Concrete(concrete_type_id) => concrete_type_id.is_fully_concrete(db),
            TypeLongId::Tuple(types) => types.iter().all(|ty| ty.is_fully_concrete(db)),
            TypeLongId::Snapshot(ty) => ty.is_fully_concrete(db),
            TypeLongId::GenericParameter(_) => false,
            TypeLongId::Var(_) => false,
            TypeLongId::Missing(_) => false,
            TypeLongId::Coupon(function_id) => function_id.is_fully_concrete(db),
            TypeLongId::FixedSizeArray { type_id, .. } => type_id.is_fully_concrete(db),
            TypeLongId::ImplType(_) => false, /* TODO(yg): I've put false here for now for
                                               * tests to work, but what's the right logic here? */
        }
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
                format!("{}", generic_param.name(db.upcast()).unwrap_or_else(|| "_".into()))
            }
            TypeLongId::ImplType(impl_type_id) => {
                format!(
                    "{}::{}",
                    impl_type_id.impl_id.name(db.upcast()),
                    impl_type_id.ty.name(db.upcast())
                )
            }
            TypeLongId::Var(var) => format!("?{}", var.id.0),
            TypeLongId::Coupon(function_id) => format!("{}::Coupon", function_id.full_name(db)),
            TypeLongId::Missing(_) => "<missing>".to_string(),
            TypeLongId::FixedSizeArray { type_id, size } => {
                format!("[{}; {:?}]", type_id.format(db), size.debug(db.elongate()))
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
            TypeLongId::GenericParameter(_) | TypeLongId::Var(_) | TypeLongId::Missing(_) => {
                return None;
            }
            TypeLongId::ImplType(_) => return None, /* TODO(yg): I've put this here to make tests
                                                     * pass, but what't the right logic? If None,
                                                     * unite with other Nones. */
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

// TODO(yg): doc.
/// `trait_or_impl_context` is the context we're at. That is, if we're inside an impl function, the
/// wrapping impl is the context here.
pub fn reduce_impl_type_if_possible(
    db: &dyn SemanticGroup,
    type_to_reduce: TypeId,
    trait_or_impl_context: TraitOrImplContext,
    resolver: &mut Resolver,
) -> Maybe<TypeId> {
    // First, reduce if already inferred.
    let mut inference = resolver.inference();
    let type_to_reduce = inference.rewrite(type_to_reduce).unwrap();

    // Then, reduce recursively.
    let mut long_ty = type_to_reduce.lookup(db);
    match &mut long_ty {
        TypeLongId::Concrete(concrete_type) => {
            let mut generic_args = concrete_type.generic_args(db);
            for generic_arg in generic_args.iter_mut() {
                let GenericArgumentId::Type(generic_arg_type) = generic_arg else {
                    continue;
                };
                *generic_arg_type = reduce_impl_type_if_possible(
                    db,
                    *generic_arg_type,
                    trait_or_impl_context,
                    resolver,
                )?;
                *generic_arg = GenericArgumentId::Type(*generic_arg_type);
            }
            concrete_type.modify_generic_args(db, generic_args);
        }
        TypeLongId::Tuple(types) => {
            for ty in types.iter_mut() {
                *ty = reduce_impl_type_if_possible(db, *ty, trait_or_impl_context, resolver)?;
            }
        }
        TypeLongId::Snapshot(ty) => {
            *ty = reduce_impl_type_if_possible(db, *ty, trait_or_impl_context, resolver)?
        }
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::Missing(_) => {}
        TypeLongId::Coupon(_) => todo!(), // TODO(yg)
        TypeLongId::FixedSizeArray { type_id, size } => {} /* TODO(yg): i've put it here for
                                            * tests to pass, but what is the
                                            * right behavior? */
    }
    let type_to_reduce = db.intern_type(long_ty);

    // Finally, reduce the impl type itself, if possible.

    let TypeLongId::ImplType(mut impl_type_id) = db.lookup_intern_type(type_to_reduce) else {
        // Nothing to implize.
        return Ok(type_to_reduce);
    };

    // TODO(yg): fix doc.
    // Try to implize an impl type if it's an ImplVar.
    // println!("yg reduce_if_possible before resolving: {:?}", impl_type_id);
    impl_type_id = reduce_trait_impl_type(db, impl_type_id, resolver);
    // println!("yg reduce_if_possible after resolving: {:?}", impl_type_id);

    // Try to implize by the concrete impl type if it is concrete.
    if let Some(ty) = reduce_concrete_impl_type(db, impl_type_id)? {
        println!("yg returning type: {:?}", ty.format(db));
        return Ok(ty);
    }
    // Try to implize by the impl context, if given.
    if let TraitOrImplContext::Impl { impl_def_id } = trait_or_impl_context {
        if let Some(ty) = reduce_in_impl_context(db, impl_type_id, impl_def_id)? {
            return Ok(ty);
        }
    }

    // Could not reduce.
    Ok(type_to_reduce)
}

// TODO(yg): doc.
fn reduce_in_impl_context(
    db: &dyn SemanticGroup,
    impl_type_id: ImplTypeId,
    impl_def_id: ImplDefId,
) -> Maybe<Option<TypeId>> {
    let Some(impl_type_def_id) = db.impl_type_by_trait_type(impl_def_id, impl_type_id.ty)? else {
        return Ok(None);
    };

    Ok(Some(db.impl_type_resolved_type(impl_type_def_id)?))
}

// TODO(yg): doc. Resolves an impl type if the impl is concrete. Returns a TypeId that's not an impl
// type with a concrete impl.
// TODO(yg/yuval): make sure impl_type_resolved_type gets a non impl type (that is, resolves in
// chain). Also make sure cycles are handled correctly.
// TODO(yg): query, cycle handling.
fn reduce_concrete_impl_type(
    db: &dyn SemanticGroup,
    impl_type_id: ImplTypeId,
) -> Maybe<Option<TypeId>> {
    let crate::items::imp::ImplId::Concrete(concrete_impl) = impl_type_id.impl_id else {
        return Ok(None);
    };

    let impl_def_id = concrete_impl.impl_def_id(db);
    reduce_in_impl_context(db, impl_type_id, impl_def_id)
}

// TODO(yg): rename, fix doc, tidy up.
fn reduce_trait_impl_type(
    db: &dyn SemanticGroup,
    impl_type_id: ImplTypeId,
    resolver: &mut Resolver,
) -> ImplTypeId {
    let ImplTypeId { impl_id, ty } = impl_type_id;
    // TODO(yg): matches.
    let crate::items::imp::ImplId::ImplVar(_) = impl_id else {
        println!("yg returning early");
        return impl_type_id;
    };

    let mut inference = resolver.inference();
    println!("yg impl_id before: {:?}", impl_id.debug(db.elongate()));
    inference.solve().unwrap();
    let impl_id = inference.rewrite(impl_id).unwrap();

    // TODO(yg): why do I get NumericLiteralu32??? I don't get it with the first line!
    println!("yg impl_id after: {:?}", impl_id.debug(db.elongate()));
    ImplTypeId { impl_id, ty }

    // let impl_var = inference.rewrite(impl_var).unwrap();

    // let concrete_trait_id = impl_var.concrete_trait_id(db);
    // println!("yg concrete_trait_id: {:?}", concrete_trait_id.debug(db.elongate()));
    // let trait_type_id = impl_type_id.ty;
    // let concrete_trait_type_long_id =
    //     ConcreteTraitTypeLongId::new(db, concrete_trait_id, trait_type_id);
    // let concrete_trait_type_id = db.intern_concrete_trait_type(concrete_trait_type_long_id);

    // let impl_lookup_context = impl_var.lookup_context(db);
    // let ty = inference.infer_trait_type(
    //         concrete_trait_type_id,
    //         &impl_lookup_context,
    //         None, // TODO(yg): Some(identifier.stable_ptr().untyped()),
    //     )
    //     // TODO(yg): do this right. don't skip_diagnostic.
    //     // .map_err(|err| err.report(diagnostics, identifier.stable_ptr().untyped()))?;
    //     .map_err(|_| skip_diagnostic())?;
    // println!("yg ty1: {:?}", ty.debug(db.elongate()));
    // // let ty = inference.rewrite(ty).unwrap();
    // // println!("yg ty2: {:?}", ty.debug(db.elongate()));
    // // inference.solve().unwrap();
    // // println!("yg ty3: {:?}", ty.debug(db.elongate()));
    // // let ty = inference.rewrite(ty).unwrap();
    // // println!("yg ty4: {:?}", ty.debug(db.elongate()));

    // // println!("yg impl_id before: {:?}", impl_id.debug(db.elongate()));
    // // let impl_id = inference.rewrite(impl_id).unwrap();
    // // println!("yg impl_id after: {:?}", impl_id.debug(db.elongate()));

    // Ok(Some(ty))
}

/// Head of a type. A non-param non-variable type has a head, which represents the kind of the root
/// node in its type tree. This is used for caching queries for fast lookups when the type is not
/// completely inferred yet.
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
    /// Returns whether the type has the `#[must_use]` attribute.
    pub fn is_must_use(&self, db: &dyn SemanticGroup) -> Maybe<bool> {
        match self {
            ConcreteTypeId::Struct(id) => id.has_attr(db, MUST_USE_ATTR),
            ConcreteTypeId::Enum(id) => id.has_attr(db, MUST_USE_ATTR),
            ConcreteTypeId::Extern(_) => Ok(false),
        }
    }
    /// Returns the attribute if a type has the `#[unstable(feature: "some-string")]` attribute.
    pub fn unstable_attr(&self, db: &dyn SemanticGroup) -> Maybe<Option<Attribute>> {
        match self {
            ConcreteTypeId::Struct(id) => id.find_attr(db, UNSTABLE_ATTR),
            ConcreteTypeId::Enum(id) => id.find_attr(db, UNSTABLE_ATTR),
            ConcreteTypeId::Extern(_) => Ok(None),
        }
    }
    /// Returns true if the type does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        self.generic_args(db)
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    // TODO(ygd)
    pub fn modify_generic_args(
        &mut self,
        db: &dyn SemanticGroup,
        new_generic_args: Vec<GenericArgumentId>,
    ) {
        match self {
            ConcreteTypeId::Struct(id) => {
                let long_id = db.lookup_intern_concrete_struct(*id);
                let new_long_id =
                    ConcreteStructLongId { generic_args: new_generic_args, ..long_id };
                *self = ConcreteTypeId::Struct(db.intern_concrete_struct(new_long_id));
            }
            ConcreteTypeId::Enum(id) => {
                let long_id = db.lookup_intern_concrete_enum(*id);
                let new_long_id = ConcreteEnumLongId { generic_args: new_generic_args, ..long_id };
                *self = ConcreteTypeId::Enum(db.intern_concrete_enum(new_long_id));
            }
            ConcreteTypeId::Extern(id) => {
                let long_id = db.lookup_intern_concrete_extern_type(*id);
                let new_long_id =
                    ConcreteExternTypeLongId { generic_args: new_generic_args, ..long_id };
                *self = ConcreteTypeId::Extern(db.intern_concrete_extern_type(new_long_id));
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
impl DebugWithDb<dyn SemanticGroup> for ConcreteEnumLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", ConcreteTypeId::Enum(db.intern_concrete_enum(self.clone())).debug(db))
    }
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
            db.intern_type(TypeLongId::FixedSizeArray { type_id: ty, size })
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
            let size = compute_expr_semantic(&mut ctx, &size_expr_syntax);
            let (_, const_value) = resolve_const_expr_and_evaluate(
                db,
                &mut ctx,
                &size,
                size_expr_syntax.stable_ptr().untyped(),
                get_usize_ty(db),
            );
            match &const_value {
                ConstValue::Int(_) => Ok(Some(db.intern_const_value(const_value))),
                ConstValue::Generic(_) => Ok(Some(db.intern_const_value(const_value))),

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
    pub droppable: InferenceResult<ImplId>,
    pub copyable: InferenceResult<ImplId>,
    pub destruct_impl: InferenceResult<ImplId>,
    pub panic_destruct_impl: InferenceResult<ImplId>,
}

/// Checks if there is at least one impl that can be inferred for a specific concrete trait.
pub fn get_impl_at_context(
    db: &dyn SemanticGroup,
    lookup_context: ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
    stable_ptr: Option<SyntaxStablePtrId>,
) -> InferenceResult<ImplId> {
    let mut inference_data = InferenceData::new(InferenceId::NoContext);
    let mut inference = inference_data.inference(db);
    println!("yg new_impl_var 6");
    let impl_id = inference.new_impl_var(concrete_trait_id, stable_ptr, lookup_context)?;
    if let Some((_, err)) = inference.finalize() {
        return Err(err);
    };
    Ok(inference.rewrite(impl_id).no_err())
}

/// Query implementation of [crate::db::SemanticGroup::single_value_type].
pub fn single_value_type(db: &dyn SemanticGroup, ty: TypeId) -> Maybe<bool> {
    Ok(match db.lookup_intern_type(ty) {
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
        TypeLongId::GenericParameter(_) => false,
        TypeLongId::Var(_) => false,
        TypeLongId::Missing(_) => false,
        TypeLongId::Coupon(_) => false,
        TypeLongId::FixedSizeArray { type_id, size } => {
            db.single_value_type(type_id)?
                || matches!(db.lookup_intern_const_value(size),
                            ConstValue::Int(value) if value.is_zero())
        }
        TypeLongId::ImplType(_) => todo!(), // TODO(yg)
    })
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

/// Returns `true` if coupons are enabled in the module.
pub(crate) fn are_coupons_enabled(db: &dyn SemanticGroup, module_file_id: ModuleFileId) -> bool {
    let owning_crate = module_file_id.0.owning_crate(db.upcast());
    let Some(config) = db.crate_config(owning_crate) else { return false };
    config.settings.experimental_features.coupons
}
