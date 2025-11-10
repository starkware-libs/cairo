use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, ModuleId,
    NamedLanguageElementId, StructId, TraitTypeId, UnstableSalsaId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_syntax::attribute::consts::MUST_USE_ATTR;
use cairo_lang_syntax::node::ast::PathSegment;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::{Intern, OptionFrom, define_short_id, extract_matches, try_extract_matches};
use itertools::{Itertools, chain};
use num_bigint::BigInt;
use num_traits::Zero;
use salsa::Database;
use sha3::{Digest, Keccak256};

use crate::corelib::{
    CorelibSemantic, concrete_copy_trait, concrete_destruct_trait, concrete_drop_trait,
    concrete_panic_destruct_trait, core_box_ty, get_usize_ty, unit_ty,
};
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{ComputationContext, compute_expr_semantic};
use crate::expr::fmt::CountingWriter;
use crate::expr::inference::canonic::{CanonicalTrait, ResultNoErrEx};
use crate::expr::inference::solver::{SemanticSolver, SolutionSet, enrich_lookup_context};
use crate::expr::inference::{InferenceData, InferenceError, InferenceId, TypeVar};
use crate::items::attribute::SemanticQueryAttrs;
use crate::items::constant::{ConstValue, ConstValueId, resolve_const_expr_and_evaluate};
use crate::items::enm::{EnumSemantic, SemanticEnumEx};
use crate::items::extern_type::ExternTypeSemantic;
use crate::items::generics::{GenericParamSemantic, fmt_generic_args};
use crate::items::imp::{ImplId, ImplLookupContext, ImplLookupContextId, ImplSemantic};
use crate::items::structure::StructSemantic;
use crate::resolve::{ResolutionContext, ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::substitution::SemanticRewriter;
use crate::{ConcreteTraitId, FunctionId, GenericArgumentId, semantic, semantic_object_for_id};

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum TypeLongId<'db> {
    Concrete(ConcreteTypeId<'db>),
    /// Some expressions might have invalid types during processing, either due to errors or
    /// during inference.
    Tuple(Vec<TypeId<'db>>),
    Snapshot(TypeId<'db>),
    GenericParameter(GenericParamId<'db>),
    Var(TypeVar<'db>),
    Coupon(FunctionId<'db>),
    FixedSizeArray {
        type_id: TypeId<'db>,
        size: ConstValueId<'db>,
    },
    ImplType(ImplTypeId<'db>),
    Closure(ClosureTypeLongId<'db>),
    Missing(#[dont_rewrite] DiagnosticAdded),
}
impl<'db> OptionFrom<TypeLongId<'db>> for ConcreteTypeId<'db> {
    fn option_from(other: TypeLongId<'db>) -> Option<Self> {
        try_extract_matches!(other, TypeLongId::Concrete)
    }
}

define_short_id!(TypeId, TypeLongId<'db>);
semantic_object_for_id!(TypeId, TypeLongId<'a>);
impl<'db> TypeId<'db> {
    pub fn missing(db: &'db dyn Database, diag_added: DiagnosticAdded) -> Self {
        TypeLongId::Missing(diag_added).intern(db)
    }

    pub fn format(&self, db: &dyn Database) -> String {
        self.long(db).format(db)
    }

    /// Returns [Maybe::Err] if the type is [TypeLongId::Missing].
    pub fn check_not_missing(&self, db: &dyn Database) -> Maybe<()> {
        if let TypeLongId::Missing(diag_added) = self.long(db) { Err(*diag_added) } else { Ok(()) }
    }

    /// Returns `true` if the type is [TypeLongId::Missing].
    pub fn is_missing(&self, db: &dyn Database) -> bool {
        self.check_not_missing(db).is_err()
    }

    /// Returns `true` if the type is `()`.
    pub fn is_unit(&self, db: &dyn Database) -> bool {
        matches!(self.long(db), TypeLongId::Tuple(types) if types.is_empty())
    }

    /// Returns the [TypeHead] for a type if available.
    pub fn head(&self, db: &'db dyn Database) -> Option<TypeHead<'db>> {
        self.long(db).head(db)
    }

    /// Returns true if the type does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn Database) -> bool {
        db.priv_type_is_fully_concrete(*self)
    }

    /// Returns true if the type does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn Database) -> bool {
        db.priv_type_is_var_free(*self)
    }

    /// Returns whether the type is phantom.
    /// Type is considered phantom if it has the `#[phantom]` attribute, or is a tuple or fixed
    /// sized array containing it.
    pub fn is_phantom(&self, db: &dyn Database) -> bool {
        self.long(db).is_phantom(db)
    }

    /// Short name of the type argument.
    pub fn short_name(&self, db: &dyn Database) -> String {
        db.priv_type_short_name(*self)
    }
}
impl<'db> TypeLongId<'db> {
    pub fn format(&self, db: &dyn Database) -> String {
        format!("{:?}", self.debug(db))
    }

    /// Returns the [TypeHead] for a type if available.
    pub fn head(&self, db: &'db dyn Database) -> Option<TypeHead<'db>> {
        Some(match self {
            TypeLongId::Concrete(concrete) => TypeHead::Concrete(concrete.generic_type(db)),
            TypeLongId::Tuple(_) => TypeHead::Tuple,
            TypeLongId::Snapshot(inner) => TypeHead::Snapshot(Box::new(inner.head(db)?)),
            TypeLongId::Coupon(_) => TypeHead::Coupon,
            TypeLongId::FixedSizeArray { .. } => TypeHead::FixedSizeArray,
            TypeLongId::GenericParameter(generic_param_id) => TypeHead::Generic(*generic_param_id),
            TypeLongId::Var(_)
            | TypeLongId::Missing(_)
            | TypeLongId::ImplType(_)
            | TypeLongId::Closure(_) => {
                return None;
            }
        })
    }

    /// Returns whether the type is phantom.
    /// Type is considered phantom if it has the `#[phantom]` attribute, (or an other attribute
    /// declared by a plugin as defining a phantom type), or is a tuple or fixed sized array
    /// containing it.
    pub fn is_phantom(&self, db: &dyn Database) -> bool {
        match self {
            TypeLongId::Concrete(id) => match id {
                ConcreteTypeId::Struct(id) => {
                    let crate_id = id.struct_id(db).long(db).0.owning_crate(db);

                    db.declared_phantom_type_attributes(crate_id)
                        .iter()
                        .any(|attr| id.has_attr(db, attr.long(db)).unwrap_or_default())
                }
                ConcreteTypeId::Enum(id) => {
                    let crate_id = id.enum_id(db).long(db).0.owning_crate(db);

                    db.declared_phantom_type_attributes(crate_id)
                        .iter()
                        .any(|attr| id.has_attr(db, attr.long(db)).unwrap_or_default())
                }
                ConcreteTypeId::Extern(id) => {
                    let crate_id = id.extern_type_id(db).long(db).0.owning_crate(db);

                    db.declared_phantom_type_attributes(crate_id)
                        .iter()
                        .any(|attr| id.has_attr(db, attr.long(db)).unwrap_or_default())
                }
            },
            TypeLongId::Tuple(inner) => inner.iter().any(|ty| ty.is_phantom(db)),
            TypeLongId::FixedSizeArray { type_id, .. } => type_id.is_phantom(db),
            TypeLongId::Snapshot(_)
            | TypeLongId::GenericParameter(_)
            | TypeLongId::Var(_)
            | TypeLongId::Coupon(_)
            | TypeLongId::ImplType(_)
            | TypeLongId::Missing(_)
            | TypeLongId::Closure(_) => false,
        }
    }

    /// Returns the module id of the given type if applicable.
    pub fn module_id(&self, db: &'db dyn Database) -> Option<ModuleId<'db>> {
        match self {
            TypeLongId::Concrete(concrete) => Some(concrete.generic_type(db).parent_module(db)),
            TypeLongId::Snapshot(ty) => {
                let (_n_snapshots, inner_ty) = peel_snapshots(db, *ty);
                inner_ty.module_id(db)
            }
            TypeLongId::GenericParameter(_) => None,
            TypeLongId::Var(_) => None,
            TypeLongId::Coupon(function_id) => {
                function_id.get_concrete(db).generic_function.module_id(db)
            }
            TypeLongId::Missing(_) => None,
            TypeLongId::Tuple(_) => Some(db.core_info().tuple_submodule),
            TypeLongId::ImplType(_) => None,
            TypeLongId::FixedSizeArray { .. } => Some(db.core_info().fixed_size_array_submodule),
            TypeLongId::Closure(closure) => {
                if let Ok(function_id) = closure.parent_function {
                    function_id.get_concrete(db).generic_function.module_id(db)
                } else {
                    None
                }
            }
        }
    }
}
impl<'db> DebugWithDb<'db> for TypeLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        match self {
            TypeLongId::Concrete(concrete) => write!(f, "{}", concrete.format(db)),
            TypeLongId::Tuple(inner_types) => {
                if inner_types.len() == 1 {
                    write!(f, "({},)", inner_types[0].format(db))
                } else {
                    write!(f, "({})", inner_types.iter().map(|ty| ty.format(db)).join(", "))
                }
            }
            TypeLongId::Snapshot(ty) => write!(f, "@{}", ty.format(db)),
            TypeLongId::GenericParameter(generic_param) => {
                write!(f, "{}", generic_param.name(db).map_or("_", |name| name.long(db)))
            }
            TypeLongId::ImplType(impl_type_id) => {
                write!(
                    f,
                    "{:?}::{}",
                    impl_type_id.impl_id.debug(db),
                    impl_type_id.ty.name(db).long(db)
                )
            }
            TypeLongId::Var(var) => write!(f, "?{}", var.id.0),
            TypeLongId::Coupon(function_id) => write!(f, "{}::Coupon", function_id.full_path(db)),
            TypeLongId::Missing(_) => write!(f, "<missing>"),
            TypeLongId::FixedSizeArray { type_id, size } => {
                write!(f, "[{}; {:?}]", type_id.format(db), size.debug(db))
            }
            TypeLongId::Closure(closure) => {
                write!(f, "{:?}", closure.debug(db))
            }
        }
    }
}

/// Head of a type.
///
/// A type that is not one of {generic param, type variable, impl type} has a head, which represents
/// the kind of the root node in its type tree. This is used for caching queries for fast lookups
/// when the type is not completely inferred yet.
#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum TypeHead<'db> {
    Concrete(GenericTypeId<'db>),
    Snapshot(Box<TypeHead<'db>>),
    Generic(GenericParamId<'db>),
    Tuple,
    Coupon,
    FixedSizeArray,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ConcreteTypeId<'db> {
    Struct(ConcreteStructId<'db>),
    Enum(ConcreteEnumId<'db>),
    Extern(ConcreteExternTypeId<'db>),
}
impl<'db> ConcreteTypeId<'db> {
    pub fn new(
        db: &'db dyn Database,
        generic_ty: GenericTypeId<'db>,
        generic_args: Vec<semantic::GenericArgumentId<'db>>,
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
    pub fn generic_type(&self, db: &'db dyn Database) -> GenericTypeId<'db> {
        match self {
            ConcreteTypeId::Struct(id) => GenericTypeId::Struct(id.long(db).struct_id),
            ConcreteTypeId::Enum(id) => GenericTypeId::Enum(id.long(db).enum_id),
            ConcreteTypeId::Extern(id) => GenericTypeId::Extern(id.long(db).extern_type_id),
        }
    }
    pub fn generic_args(&self, db: &'db dyn Database) -> Vec<semantic::GenericArgumentId<'db>> {
        match self {
            ConcreteTypeId::Struct(id) => id.long(db).generic_args.clone(),
            ConcreteTypeId::Enum(id) => id.long(db).generic_args.clone(),
            ConcreteTypeId::Extern(id) => id.long(db).generic_args.clone(),
        }
    }
    pub fn format(&self, db: &dyn Database) -> String {
        format!("{:?}", self.debug(db))
    }

    /// Returns whether the type has the `#[must_use]` attribute.
    pub fn is_must_use(&self, db: &dyn Database) -> Maybe<bool> {
        match self {
            ConcreteTypeId::Struct(id) => id.has_attr(db, MUST_USE_ATTR),
            ConcreteTypeId::Enum(id) => id.has_attr(db, MUST_USE_ATTR),
            ConcreteTypeId::Extern(id) => id.has_attr(db, MUST_USE_ATTR),
        }
    }
    /// Returns true if the type does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn Database) -> bool {
        self.generic_args(db)
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the type does not contain any inference variables.
    pub fn is_var_free(&self, db: &dyn Database) -> bool {
        self.generic_args(db).iter().all(|generic_argument_id| generic_argument_id.is_var_free(db))
    }
}
impl<'db> DebugWithDb<'db> for ConcreteTypeId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        let f = &mut CountingWriter::new(f);
        write!(f, "{}", self.generic_type(db).format(db))?;
        fmt_generic_args(&self.generic_args(db), f, db)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteStructLongId<'db> {
    pub struct_id: StructId<'db>,
    pub generic_args: Vec<semantic::GenericArgumentId<'db>>,
}
define_short_id!(ConcreteStructId, ConcreteStructLongId<'db>);
semantic_object_for_id!(ConcreteStructId, ConcreteStructLongId<'a>);
impl<'db> ConcreteStructId<'db> {
    pub fn struct_id(&self, db: &'db dyn Database) -> StructId<'db> {
        self.long(db).struct_id
    }
}
impl<'db> DebugWithDb<'db> for ConcreteStructLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(f, "{:?}", ConcreteTypeId::Struct(self.clone().intern(db)).debug(db))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteEnumLongId<'db> {
    pub enum_id: EnumId<'db>,
    pub generic_args: Vec<semantic::GenericArgumentId<'db>>,
}
impl<'db> DebugWithDb<'db> for ConcreteEnumLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(f, "{:?}", ConcreteTypeId::Enum(self.clone().intern(db)).debug(db))
    }
}

define_short_id!(ConcreteEnumId, ConcreteEnumLongId<'db>);
semantic_object_for_id!(ConcreteEnumId, ConcreteEnumLongId<'a>);
impl<'db> ConcreteEnumId<'db> {
    pub fn enum_id(&self, db: &'db dyn Database) -> EnumId<'db> {
        self.long(db).enum_id
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteExternTypeLongId<'db> {
    pub extern_type_id: ExternTypeId<'db>,
    pub generic_args: Vec<semantic::GenericArgumentId<'db>>,
}
define_short_id!(ConcreteExternTypeId, ConcreteExternTypeLongId<'db>);
semantic_object_for_id!(ConcreteExternTypeId, ConcreteExternTypeLongId<'a>);
impl<'db> ConcreteExternTypeId<'db> {
    pub fn extern_type_id(&self, db: &'db dyn Database) -> ExternTypeId<'db> {
        self.long(db).extern_type_id
    }
}

/// A type id of a closure function.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ClosureTypeLongId<'db> {
    pub param_tys: Vec<TypeId<'db>>,
    pub ret_ty: TypeId<'db>,
    /// The set of types captured by the closure, this field is used to determined if the
    /// closure has Drop, Destruct or PanicDestruct.
    /// A vector as the fields needs to be hashable.
    pub captured_types: Vec<TypeId<'db>>,
    /// The parent function of the closure or an error.
    pub parent_function: Maybe<FunctionId<'db>>,
    /// Every closure has a unique type that is based on the stable location of its wrapper.
    #[dont_rewrite]
    pub wrapper_location: StableLocation<'db>,
}

impl<'db> DebugWithDb<'db> for ClosureTypeLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(f, "{{closure@{:?}}}", self.wrapper_location.debug(db))
    }
}

/// An impl item of kind type.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub struct ImplTypeId<'db> {
    /// The impl the item type is in.
    impl_id: ImplId<'db>,
    /// The trait type this impl type "implements".
    ty: TraitTypeId<'db>,
}
impl<'db> ImplTypeId<'db> {
    /// Creates a new impl type id. For an impl type of a concrete impl, asserts that the trait
    /// type belongs to the same trait that the impl implements (panics if not).
    pub fn new(impl_id: ImplId<'db>, ty: TraitTypeId<'db>, db: &'db dyn Database) -> Self {
        if let crate::items::imp::ImplLongId::Concrete(concrete_impl) = impl_id.long(db) {
            let impl_def_id = concrete_impl.impl_def_id(db);
            assert_eq!(Ok(ty.trait_id(db)), db.impl_def_trait(impl_def_id));
        }

        ImplTypeId { impl_id, ty }
    }
    pub fn impl_id(&self) -> ImplId<'db> {
        self.impl_id
    }
    pub fn ty(&self) -> TraitTypeId<'db> {
        self.ty
    }
    pub fn format(&self, db: &dyn Database) -> String {
        format!("{}::{}", self.impl_id.name(db), self.ty.name(db).long(db))
    }
}
impl<'db> DebugWithDb<'db> for ImplTypeId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

/// A wrapper around ImplTypeById that implements Ord for saving in an ordered collection.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub struct ImplTypeById<'db>(ImplTypeId<'db>);

impl<'db> From<ImplTypeId<'db>> for ImplTypeById<'db> {
    fn from(impl_type_id: ImplTypeId<'db>) -> Self {
        Self(impl_type_id)
    }
}
impl<'db> Ord for ImplTypeById<'db> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .impl_id
            .get_internal_id()
            .cmp(&other.0.impl_id.get_internal_id())
            .then_with(|| self.0.ty.get_internal_id().cmp(&other.0.ty.get_internal_id()))
    }
}
impl<'db> PartialOrd for ImplTypeById<'db> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path. Used for resolving from non-statement context.
pub fn resolve_type<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    ty_syntax: &ast::Expr<'db>,
) -> TypeId<'db> {
    resolve_type_ex(db, diagnostics, resolver, ty_syntax, ResolutionContext::Default)
}
/// Resolves a type given a module and a path. Allows defining a resolution context.
pub fn resolve_type_ex<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    ty_syntax: &ast::Expr<'db>,
    ctx: ResolutionContext<'db, '_>,
) -> TypeId<'db> {
    maybe_resolve_type(db, diagnostics, resolver, ty_syntax, ctx)
        .unwrap_or_else(|diag_added| TypeId::missing(db, diag_added))
}
fn maybe_resolve_type<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    ty_syntax: &ast::Expr<'db>,
    mut ctx: ResolutionContext<'db, '_>,
) -> Maybe<TypeId<'db>> {
    Ok(match ty_syntax {
        ast::Expr::Path(path) => {
            match resolver.resolve_concrete_path_ex(
                diagnostics,
                path,
                NotFoundItemType::Type,
                ctx,
            )? {
                ResolvedConcreteItem::Type(ty) => ty,
                _ => {
                    return Err(diagnostics.report(path.stable_ptr(db), NotAType));
                }
            }
        }
        ast::Expr::Parenthesized(expr_syntax) => {
            resolve_type_ex(db, diagnostics, resolver, &expr_syntax.expr(db), ctx)
        }
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(db)
                .elements(db)
                .map(|subexpr_syntax| {
                    resolve_type_ex(
                        db,
                        diagnostics,
                        resolver,
                        &subexpr_syntax,
                        match ctx {
                            ResolutionContext::Default => ResolutionContext::Default,
                            ResolutionContext::ModuleItem(id) => ResolutionContext::ModuleItem(id),
                            ResolutionContext::Statement(ref mut env) => {
                                ResolutionContext::Statement(env)
                            }
                        },
                    )
                })
                .collect();
            TypeLongId::Tuple(sub_tys).intern(db)
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(db), ast::UnaryOperator::At(_)) =>
        {
            let ty = resolve_type_ex(db, diagnostics, resolver, &unary_syntax.expr(db), ctx);
            TypeLongId::Snapshot(ty).intern(db)
        }
        // TODO(TomerStarkware): make sure this is unreachable.
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(db), ast::UnaryOperator::Desnap(_)) =>
        {
            let ty = resolve_type_ex(db, diagnostics, resolver, &unary_syntax.expr(db), ctx);
            if let Some(desnapped_ty) = try_extract_matches!(ty.long(db), TypeLongId::Snapshot) {
                *desnapped_ty
            } else {
                return Err(diagnostics.report(ty_syntax.stable_ptr(db), DerefNonRef { ty }));
            }
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(db), ast::UnaryOperator::Reference(_)) =>
        {
            if !are_repr_ptrs_enabled(db, resolver.module_id) {
                return Err(diagnostics.report(ty_syntax.stable_ptr(db), ReprPtrsDisabled));
            }
            let inner_ty = resolve_type_ex(db, diagnostics, resolver, &unary_syntax.expr(db), ctx);
            let snapshot_ty = TypeLongId::Snapshot(inner_ty).intern(db);
            core_box_ty(db, snapshot_ty)
        }
        ast::Expr::FixedSizeArray(array_syntax) => {
            let Some([ty]) = &array_syntax.exprs(db).elements(db).collect_array() else {
                return Err(
                    diagnostics.report(ty_syntax.stable_ptr(db), FixedSizeArrayTypeNonSingleType)
                );
            };
            let ty = resolve_type_ex(db, diagnostics, resolver, ty, ctx);
            let size =
                match extract_fixed_size_array_size(db, diagnostics, array_syntax, resolver)? {
                    Some(size) => size,
                    None => {
                        return Err(diagnostics
                            .report(ty_syntax.stable_ptr(db), FixedSizeArrayTypeEmptySize));
                    }
                };
            TypeLongId::FixedSizeArray { type_id: ty, size }.intern(db)
        }
        _ => {
            return Err(diagnostics.report(ty_syntax.stable_ptr(db), UnknownType));
        }
    })
}

/// A generic argument which is not fully inferred. Used to avoid cycles in the inference.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub enum ShallowGenericArg<'db> {
    /// The generic argument is a generic parameter.
    GenericParameter(GenericParamId<'db>),
    /// The generic argument is a generic type.
    GenericType(GenericTypeId<'db>),
    Snapshot(Box<ShallowGenericArg<'db>>),
    Tuple,
    FixedSizeArray,
}

impl<'db> ShallowGenericArg<'db> {
    /// Returns the module id of the shallow generic argument.
    pub fn module_id(&self, db: &'db dyn Database) -> Option<ModuleId<'db>> {
        match self {
            ShallowGenericArg::GenericParameter(_) => None,
            ShallowGenericArg::GenericType(ty) => Some(ty.module_id(db)),
            ShallowGenericArg::Snapshot(inner) => inner.module_id(db),
            ShallowGenericArg::Tuple => TypeLongId::Tuple(vec![]).module_id(db),
            ShallowGenericArg::FixedSizeArray => TypeLongId::FixedSizeArray {
                type_id: unit_ty(db),
                size: ConstValue::Struct(vec![], unit_ty(db)).intern(db),
            }
            .module_id(db),
        }
    }
    pub fn head(&self) -> TypeHead<'db> {
        match self {
            ShallowGenericArg::GenericParameter(param) => TypeHead::Generic(*param),
            ShallowGenericArg::GenericType(ty) => TypeHead::Concrete(*ty),
            ShallowGenericArg::Snapshot(inner) => TypeHead::Snapshot(Box::new(inner.head())),
            ShallowGenericArg::Tuple => TypeHead::Tuple,
            ShallowGenericArg::FixedSizeArray => TypeHead::FixedSizeArray,
        }
    }
}
/// Resolves a shallow generic argument from a syntax node.
pub fn maybe_resolve_shallow_generic_arg_type<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    ty_syntax: &ast::Expr<'db>,
) -> Option<ShallowGenericArg<'db>> {
    Some(match ty_syntax {
        ast::Expr::Path(path) => {
            if let [PathSegment::Simple(path)] =
                path.segments(db).elements(db).collect_vec().as_slice()
                && let Some(ResolvedConcreteItem::Type(ty)) =
                    resolver.determine_base_item_in_local_scope(&path.ident(db))
            {
                let param = extract_matches!(ty.long(db), TypeLongId::GenericParameter);
                return Some(ShallowGenericArg::GenericParameter(*param));
            }

            match resolver
                .resolve_generic_path_with_args(
                    diagnostics,
                    path,
                    NotFoundItemType::Type,
                    ResolutionContext::Default,
                )
                .ok()?
            {
                ResolvedGenericItem::GenericType(ty) => ShallowGenericArg::GenericType(ty),
                _ => {
                    return None;
                }
            }
        }
        ast::Expr::Parenthesized(expr_syntax) => maybe_resolve_shallow_generic_arg_type(
            db,
            diagnostics,
            resolver,
            &expr_syntax.expr(db),
        )?,
        ast::Expr::Tuple(_) => ShallowGenericArg::Tuple,
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(db), ast::UnaryOperator::At(_)) =>
        {
            ShallowGenericArg::Snapshot(Box::new(maybe_resolve_shallow_generic_arg_type(
                db,
                diagnostics,
                resolver,
                &unary_syntax.expr(db),
            )?))
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(db), ast::UnaryOperator::Desnap(_)) =>
        {
            maybe_resolve_shallow_generic_arg_type(
                db,
                diagnostics,
                resolver,
                &unary_syntax.expr(db),
            )?
        }
        ast::Expr::FixedSizeArray(_) => ShallowGenericArg::FixedSizeArray,
        _ => {
            return None;
        }
    })
}

/// Extracts the size of a fixed size array, or none if the size is missing. Reports an error if the
/// size is not a numeric literal.
pub fn extract_fixed_size_array_size<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    syntax: &ast::ExprFixedSizeArray<'db>,
    resolver: &mut Resolver<'db>,
) -> Maybe<Option<ConstValueId<'db>>> {
    match syntax.size(db) {
        ast::OptionFixedSizeArraySize::FixedSizeArraySize(size_clause) => {
            let mut ctx = ComputationContext::new_global(db, diagnostics, resolver);
            let size_expr_syntax = size_clause.size(db);
            let size = compute_expr_semantic(&mut ctx, &size_expr_syntax);
            let const_value = resolve_const_expr_and_evaluate(
                db,
                &mut ctx,
                &size,
                size_expr_syntax.stable_ptr(db).untyped(),
                get_usize_ty(db),
                false,
            );
            if matches!(const_value.long(db), ConstValue::Int(_, _) | ConstValue::Generic(_)) {
                Ok(Some(const_value))
            } else {
                Err(diagnostics.report(syntax.stable_ptr(db), FixedSizeArrayNonNumericSize))
            }
        }
        ast::OptionFixedSizeArraySize::Empty(_) => Ok(None),
    }
}

/// Verifies that a given fixed size array size is within limits, and adds a diagnostic if not.
pub fn verify_fixed_size_array_size<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    size: &BigInt,
    syntax: &ast::ExprFixedSizeArray<'db>,
) -> Maybe<()> {
    if size > &BigInt::from(i16::MAX) {
        return Err(diagnostics.report(syntax.stable_ptr(db), FixedSizeArraySizeTooBig));
    }
    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct TypeInfo<'db> {
    pub droppable: Result<ImplId<'db>, InferenceError<'db>>,
    pub copyable: Result<ImplId<'db>, InferenceError<'db>>,
    pub destruct_impl: Result<ImplId<'db>, InferenceError<'db>>,
    pub panic_destruct_impl: Result<ImplId<'db>, InferenceError<'db>>,
}

/// Checks if there is at least one impl that can be inferred for a specific concrete trait.
pub fn get_impl_at_context<'db>(
    db: &'db dyn Database,
    lookup_context: ImplLookupContextId<'db>,
    concrete_trait_id: ConcreteTraitId<'db>,
    stable_ptr: Option<SyntaxStablePtrId<'db>>,
) -> Result<ImplId<'db>, InferenceError<'db>> {
    let constrains =
        db.generic_params_type_constraints(lookup_context.long(db).generic_params.clone());
    if constrains.is_empty() && concrete_trait_id.is_var_free(db) {
        return solve_concrete_trait_no_constraints(db, lookup_context, concrete_trait_id);
    }
    let mut inference_data = InferenceData::new(InferenceId::NoContext);
    let mut inference = inference_data.inference(db);
    inference.conform_generic_params_type_constraints(constrains);
    // It's ok to consume the errors without reporting as this is a helper function meant to find an
    // impl and return it, but it's ok if the impl can't be found.
    let impl_id = inference.new_impl_var(concrete_trait_id, stable_ptr, lookup_context);
    if let Err((err_set, _)) = inference.finalize_without_reporting() {
        return Err(inference
            .consume_error_without_reporting(err_set)
            .expect("Error couldn't be already consumed"));
    };
    Ok(inference.rewrite(impl_id).no_err())
}

/// Implementation of [TypesSemantic::single_value_type].
fn single_value_type(db: &dyn Database, ty: TypeId<'_>) -> Maybe<bool> {
    Ok(match ty.long(db) {
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
            for ty in types {
                if !db.single_value_type(*ty)? {
                    return Ok(false);
                }
            }
            true
        }
        TypeLongId::Snapshot(ty) => db.single_value_type(*ty)?,
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::Missing(_)
        | TypeLongId::Coupon(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::Closure(_) => false,
        TypeLongId::FixedSizeArray { type_id, size } => {
            db.single_value_type(*type_id)?
                || matches!(size.long(db),
                            ConstValue::Int(value, _) if value.is_zero())
        }
    })
}

/// Query implementation of [TypesSemantic::single_value_type].
#[salsa::tracked]
fn single_value_type_tracked<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> Maybe<bool> {
    single_value_type(db, ty)
}

/// Adds diagnostics for a type, post semantic analysis of types.
pub fn add_type_based_diagnostics<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    ty: TypeId<'db>,
    stable_ptr: impl Into<SyntaxStablePtrId<'db>> + Copy,
) {
    if db.type_size_info(ty) == Ok(TypeSizeInformation::Infinite) {
        diagnostics.report(stable_ptr, InfiniteSizeType(ty));
    }
    if let TypeLongId::Concrete(ConcreteTypeId::Extern(extrn)) = ty.long(db) {
        let long_id = extrn.long(db);
        if long_id.extern_type_id.name(db).long(db) == "Array"
            && let [GenericArgumentId::Type(arg_ty)] = &long_id.generic_args[..]
            && db.type_size_info(*arg_ty) == Ok(TypeSizeInformation::ZeroSized)
        {
            diagnostics.report(stable_ptr, ArrayOfZeroSizedElements(*arg_ty));
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

/// Implementation of [TypesSemantic::type_size_info].
fn type_size_info(db: &dyn Database, ty: TypeId<'_>) -> Maybe<TypeSizeInformation> {
    match ty.long(db) {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(id) => {
                if check_all_type_are_zero_sized(
                    db,
                    db.concrete_struct_members(*id)?.iter().map(|(_, member)| &member.ty),
                )? {
                    return Ok(TypeSizeInformation::ZeroSized);
                }
            }
            ConcreteTypeId::Enum(id) => {
                for variant in &db.concrete_enum_variants(*id)? {
                    // Recursive calling in order to find infinite sized types.
                    db.type_size_info(variant.ty)?;
                }
            }
            ConcreteTypeId::Extern(_) => {}
        },
        TypeLongId::Tuple(types) => {
            if check_all_type_are_zero_sized(db, types.iter())? {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
        TypeLongId::Snapshot(ty) => {
            if db.type_size_info(*ty)? == TypeSizeInformation::ZeroSized {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
        TypeLongId::Closure(closure_ty) => {
            if check_all_type_are_zero_sized(db, closure_ty.captured_types.iter())? {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
        TypeLongId::Coupon(_) => return Ok(TypeSizeInformation::ZeroSized),
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::Missing(_)
        | TypeLongId::ImplType(_) => {}
        TypeLongId::FixedSizeArray { type_id, size } => {
            if matches!(size.long(db), ConstValue::Int(value,_) if value.is_zero())
                || db.type_size_info(*type_id)? == TypeSizeInformation::ZeroSized
            {
                return Ok(TypeSizeInformation::ZeroSized);
            }
        }
    }
    Ok(TypeSizeInformation::Other)
}

/// Query implementation of [TypesSemantic::type_size_info].
#[salsa::tracked(cycle_result=type_size_info_cycle)]
fn type_size_info_tracked<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Maybe<TypeSizeInformation> {
    type_size_info(db, ty)
}

/// Checks if all types in the iterator are zero sized.
fn check_all_type_are_zero_sized<'a>(
    db: &dyn Database,
    types: impl Iterator<Item = &'a TypeId<'a>>,
) -> Maybe<bool> {
    let mut zero_sized = true;
    for ty in types {
        if db.type_size_info(*ty)? != TypeSizeInformation::ZeroSized {
            zero_sized = false;
        }
    }
    Ok(zero_sized)
}

/// Cycle handling of [TypesSemantic::type_size_info].
fn type_size_info_cycle<'db>(
    _db: &'db dyn Database,
    _ty: TypeId<'db>,
) -> Maybe<TypeSizeInformation> {
    Ok(TypeSizeInformation::Infinite)
}

// TODO(spapini): type info lookup for non generic types needs to not depend on lookup_context.
// This is to ensure that sierra generator will see a consistent type info of types.
/// Implementation of [TypesSemantic::type_info].
fn type_info<'db>(
    db: &'db dyn Database,
    lookup_context: ImplLookupContextId<'db>,
    ty: TypeId<'db>,
) -> TypeInfo<'db> {
    // Dummy stable pointer for type inference variables, since inference is disabled.
    let droppable = get_impl_at_context(db, lookup_context, concrete_drop_trait(db, ty), None);
    let copyable = get_impl_at_context(db, lookup_context, concrete_copy_trait(db, ty), None);
    let destruct_impl =
        get_impl_at_context(db, lookup_context, concrete_destruct_trait(db, ty), None);
    let panic_destruct_impl =
        get_impl_at_context(db, lookup_context, concrete_panic_destruct_trait(db, ty), None);
    TypeInfo { droppable, copyable, destruct_impl, panic_destruct_impl }
}

/// Query implementation of [TypesSemantic::type_info].
#[salsa::tracked]
fn type_info_tracked<'db>(
    db: &'db dyn Database,
    lookup_context: ImplLookupContextId<'db>,
    ty: TypeId<'db>,
) -> TypeInfo<'db> {
    type_info(db, lookup_context, ty)
}

/// Solves a concrete trait without any constraints.
/// Only works when the given trait is var free.
fn solve_concrete_trait_no_constraints<'db>(
    db: &'db dyn Database,
    lookup_context: ImplLookupContextId<'db>,
    id: ConcreteTraitId<'db>,
) -> Result<ImplId<'db>, InferenceError<'db>> {
    let mut lookup_context = lookup_context.long(db).clone();
    enrich_lookup_context(db, id, &mut lookup_context);
    let lookup_context = lookup_context.intern(db);
    match db.canonic_trait_solutions(
        CanonicalTrait { id, mappings: Default::default() },
        lookup_context,
        Default::default(),
    )? {
        SolutionSet::None => Err(InferenceError::NoImplsFound(id)),
        SolutionSet::Unique(solution) => Ok(solution.0),
        SolutionSet::Ambiguous(ambiguity) => Err(InferenceError::Ambiguity(ambiguity)),
    }
}

/// Implementation of [TypesSemantic::copyable].
fn copyable<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Result<ImplId<'db>, InferenceError<'db>> {
    solve_concrete_trait_no_constraints(
        db,
        ImplLookupContext::new_from_type(ty, db).intern(db),
        concrete_copy_trait(db, ty),
    )
}

/// Query implementation of [TypesSemantic::copyable].
#[salsa::tracked]
fn copyable_tracked<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Result<ImplId<'db>, InferenceError<'db>> {
    copyable(db, ty)
}

/// Implementation of [TypesSemantic::droppable].
fn droppable<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Result<ImplId<'db>, InferenceError<'db>> {
    solve_concrete_trait_no_constraints(
        db,
        ImplLookupContext::new_from_type(ty, db).intern(db),
        concrete_drop_trait(db, ty),
    )
}

/// Query implementation of [TypesSemantic::droppable].
#[salsa::tracked]
fn droppable_tracked<'db>(
    db: &'db dyn Database,
    ty: TypeId<'db>,
) -> Result<ImplId<'db>, InferenceError<'db>> {
    droppable(db, ty)
}

/// Implementation of [PrivTypesSemantic::priv_type_is_fully_concrete].
fn priv_type_is_fully_concrete(db: &dyn Database, ty: TypeId<'_>) -> bool {
    match ty.long(db) {
        TypeLongId::Concrete(concrete_type_id) => concrete_type_id.is_fully_concrete(db),
        TypeLongId::Tuple(types) => types.iter().all(|ty| ty.is_fully_concrete(db)),
        TypeLongId::Snapshot(ty) => ty.is_fully_concrete(db),
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::Missing(_)
        | TypeLongId::ImplType(_) => false,
        TypeLongId::Coupon(function_id) => function_id.is_fully_concrete(db),
        TypeLongId::FixedSizeArray { type_id, size } => {
            type_id.is_fully_concrete(db) && size.is_fully_concrete(db)
        }
        TypeLongId::Closure(closure) => {
            closure.parent_function.map(|id| id.is_fully_concrete(db)).unwrap_or(true)
                && closure.param_tys.iter().all(|param| param.is_fully_concrete(db))
                && closure.ret_ty.is_fully_concrete(db)
        }
    }
}

/// Query implementation of [PrivTypesSemantic::priv_type_is_fully_concrete].
#[salsa::tracked]
pub fn priv_type_is_fully_concrete_tracked<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> bool {
    priv_type_is_fully_concrete(db, ty)
}

pub fn priv_type_is_var_free<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> bool {
    match ty.long(db) {
        TypeLongId::Concrete(concrete_type_id) => concrete_type_id.is_var_free(db),
        TypeLongId::Tuple(types) => types.iter().all(|ty| ty.is_var_free(db)),
        TypeLongId::Snapshot(ty) => ty.is_var_free(db),
        TypeLongId::Var(_) => false,
        TypeLongId::GenericParameter(_) | TypeLongId::Missing(_) => true,
        TypeLongId::Coupon(function_id) => function_id.is_var_free(db),
        TypeLongId::FixedSizeArray { type_id, size } => {
            type_id.is_var_free(db) && size.is_var_free(db)
        }
        // TODO(TomerStarkware): consider rename the function to `priv_type_might_need_rewrite`.
        // a var free ImplType needs to be rewritten if has impl bounds constraints.
        TypeLongId::ImplType(_) => false,
        TypeLongId::Closure(closure) => {
            chain!(&closure.captured_types, &closure.param_tys).all(|param| param.is_var_free(db))
                && closure.ret_ty.is_var_free(db)
        }
    }
}

/// Query implementation of [PrivTypesSemantic::priv_type_is_var_free].
#[salsa::tracked]
pub fn priv_type_is_var_free_tracked<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> bool {
    priv_type_is_var_free(db, ty)
}

pub fn priv_type_short_name(db: &dyn Database, ty: TypeId<'_>) -> String {
    match ty.long(db) {
        TypeLongId::Concrete(concrete_type_id) => {
            let mut result = concrete_type_id.generic_type(db).format(db);
            let mut generic_args = concrete_type_id.generic_args(db).into_iter().peekable();
            if generic_args.peek().is_some() {
                result.push_str("::<h0x");
                let mut hasher = Keccak256::new();
                for arg in generic_args {
                    hasher.update(arg.short_name(db).as_bytes());
                }
                for c in hasher.finalize() {
                    result.push_str(&format!("{c:x}"));
                }
                result.push('>');
            }
            result
        }
        TypeLongId::Tuple(types) => {
            let mut result = String::from("(h0x");
            let mut hasher = Keccak256::new();
            for ty in types {
                hasher.update(ty.short_name(db).as_bytes());
            }
            for c in hasher.finalize() {
                result.push_str(&format!("{c:x}"));
            }
            result.push(')');
            result
        }
        TypeLongId::Snapshot(ty) => {
            format!("@{}", ty.short_name(db))
        }
        TypeLongId::FixedSizeArray { type_id, size } => {
            format!("[{}; {:?}", type_id.short_name(db), size.debug(db))
        }
        other => other.format(db),
    }
}

#[salsa::tracked]
pub fn priv_type_short_name_tracked<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> String {
    priv_type_short_name(db, ty)
}

/// Peels all wrapping Snapshot (`@`) from the type.
/// Returns the number of peeled snapshots and the inner type.
pub fn peel_snapshots<'db>(db: &'db dyn Database, ty: TypeId<'db>) -> (usize, TypeLongId<'db>) {
    peel_snapshots_ex(db, ty.long(db).clone())
}

/// Same as `peel_snapshots`, but takes a `TypeLongId` instead of a `TypeId`.
pub fn peel_snapshots_ex<'db>(
    db: &'db dyn Database,
    mut long_ty: TypeLongId<'db>,
) -> (usize, TypeLongId<'db>) {
    let mut n_snapshots = 0;
    while let TypeLongId::Snapshot(ty) = long_ty {
        long_ty = ty.long(db).clone();
        n_snapshots += 1;
    }
    (n_snapshots, long_ty)
}

/// Wraps a type with Snapshot (`@`) `n_snapshots` times.
pub fn wrap_in_snapshots<'db>(
    db: &'db dyn Database,
    mut ty: TypeId<'db>,
    n_snapshots: usize,
) -> TypeId<'db> {
    for _ in 0..n_snapshots {
        ty = TypeLongId::Snapshot(ty).intern(db);
    }
    ty
}

/// Returns `true` if coupons are enabled in the module.
pub(crate) fn are_coupons_enabled(db: &dyn Database, module_id: ModuleId<'_>) -> bool {
    let owning_crate = module_id.owning_crate(db);
    let Some(config) = db.crate_config(owning_crate) else { return false };
    config.settings.experimental_features.coupons
}

/// Returns `true` if representation pointers are enabled in the module.
pub(crate) fn are_repr_ptrs_enabled(db: &dyn Database, module_id: ModuleId<'_>) -> bool {
    let owning_crate = module_id.owning_crate(db);
    db.crate_config(owning_crate)
        .is_some_and(|config| config.settings.experimental_features.repr_ptrs)
}

/// Trait for types-related semantic queries.
pub trait TypesSemantic<'db>: Database {
    /// Returns the generic params of a generic type.
    fn generic_type_generic_params(
        &'db self,
        generic_type: GenericTypeId<'db>,
    ) -> Maybe<&'db [semantic::GenericParam<'db>]> {
        match generic_type {
            GenericTypeId::Struct(id) => self.struct_generic_params(id),
            GenericTypeId::Enum(id) => self.enum_generic_params(id),
            GenericTypeId::Extern(id) => self.extern_type_declaration_generic_params(id),
        }
    }
    /// Returns true if there is only one value for the given type and hence the values of the given
    /// type are all interchangeable.
    /// Examples include the unit type tuple of a unit type and empty structs.
    /// Always returns false for extern types.
    fn single_value_type(&'db self, ty: TypeId<'db>) -> Maybe<bool> {
        single_value_type_tracked(self.as_dyn_database(), ty)
    }
    /// Returns the type size information for the given type.
    fn type_size_info(&'db self, ty: TypeId<'db>) -> Maybe<TypeSizeInformation> {
        type_size_info_tracked(self.as_dyn_database(), ty)
    }
    /// Returns the type info for a type in a context.
    fn type_info(
        &'db self,
        lookup_context: ImplLookupContextId<'db>,
        ty: TypeId<'db>,
    ) -> TypeInfo<'db> {
        type_info_tracked(self.as_dyn_database(), lookup_context, ty)
    }
    /// Returns the `Copy` impl for a type in general context.
    fn copyable(&'db self, ty: TypeId<'db>) -> Result<ImplId<'db>, InferenceError<'db>> {
        copyable_tracked(self.as_dyn_database(), ty)
    }
    /// Returns the `Drop` impl for a type in general context.
    fn droppable(&'db self, ty: TypeId<'db>) -> Result<ImplId<'db>, InferenceError<'db>> {
        droppable_tracked(self.as_dyn_database(), ty)
    }
}
impl<'db, T: Database + ?Sized> TypesSemantic<'db> for T {}

/// Private trait for types-related semantic queries.
pub trait PrivTypesSemantic<'db>: Database {
    /// Private query to check if a type is fully concrete.
    fn priv_type_is_fully_concrete(&self, ty: TypeId<'db>) -> bool {
        priv_type_is_fully_concrete_tracked(self.as_dyn_database(), ty)
    }
    /// Private query to check if a type contains no variables.
    fn priv_type_is_var_free(&self, ty: TypeId<'db>) -> bool {
        priv_type_is_var_free_tracked(self.as_dyn_database(), ty)
    }
    /// Private query for a shorter unique name for types.
    fn priv_type_short_name(&self, ty: TypeId<'db>) -> String {
        priv_type_short_name_tracked(self.as_dyn_database(), ty)
    }
}
impl<'db, T: Database + ?Sized> PrivTypesSemantic<'db> for T {}
