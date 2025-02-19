use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    EnumId, ExternTypeId, GenericParamId, GenericTypeId, LanguageElementId, ModuleFileId, ModuleId,
    NamedLanguageElementId, StructId, TraitTypeId, UnstableSalsaId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_syntax::attribute::consts::MUST_USE_ATTR;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::{Intern, LookupIntern, OptionFrom, define_short_id, try_extract_matches};
use itertools::{Itertools, chain};
use num_bigint::BigInt;
use num_traits::Zero;
use sha3::{Digest, Keccak256};
use smol_str::SmolStr;

use crate::corelib::{
    concrete_copy_trait, concrete_destruct_trait, concrete_drop_trait,
    concrete_panic_destruct_trait, get_usize_ty,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{
    ComputationContext, ContextFunction, Environment, compute_expr_semantic,
};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::{InferenceData, InferenceError, InferenceId, TypeVar};
use crate::items::attribute::SemanticQueryAttrs;
use crate::items::constant::{ConstValue, ConstValueId, resolve_const_expr_and_evaluate};
use crate::items::imp::{ImplId, ImplLookupContext};
use crate::resolve::{ResolvedConcreteItem, Resolver};
use crate::substitution::SemanticRewriter;
use crate::{ConcreteTraitId, FunctionId, GenericArgumentId, semantic, semantic_object_for_id};

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
    Closure(ClosureTypeLongId),
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

    /// Short name of the type argument.
    pub fn short_name(&self, db: &dyn SemanticGroup) -> String {
        db.priv_type_short_name(*self)
    }
}
impl TypeLongId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        format!("{:?}", self.debug(db.elongate()))
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
            | TypeLongId::Closure(_) => {
                return None;
            }
        })
    }

    /// Returns whether the type is phantom.
    /// Type is considered phantom if it has the `#[phantom]` attribute, (or an other attribute
    /// declared by a plugin as defining a phantom type), or is a tuple or fixed sized array
    /// containing it.
    pub fn is_phantom(&self, db: &dyn SemanticGroup) -> bool {
        let defs_db = db.upcast();

        match self {
            TypeLongId::Concrete(id) => match id {
                ConcreteTypeId::Struct(id) => {
                    let crate_id =
                        db.lookup_intern_struct(id.struct_id(db)).0.0.owning_crate(defs_db);

                    db.declared_phantom_type_attributes(crate_id)
                        .iter()
                        .any(|attr| id.has_attr(db, attr).unwrap_or_default())
                }
                ConcreteTypeId::Enum(id) => {
                    let crate_id = db.lookup_intern_enum(id.enum_id(db)).0.0.owning_crate(defs_db);

                    db.declared_phantom_type_attributes(crate_id)
                        .iter()
                        .any(|attr| id.has_attr(db, attr).unwrap_or_default())
                }
                ConcreteTypeId::Extern(id) => {
                    let crate_id = db
                        .lookup_intern_extern_type(id.extern_type_id(db))
                        .0
                        .0
                        .owning_crate(defs_db);

                    db.declared_phantom_type_attributes(crate_id)
                        .iter()
                        .any(|attr| id.has_attr(db, attr).unwrap_or_default())
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
    pub fn module_id(&self, db: &dyn SemanticGroup) -> Option<ModuleId> {
        match self {
            TypeLongId::Concrete(concrete) => {
                Some(concrete.generic_type(db).module_file_id(db.upcast()).0)
            }
            TypeLongId::Snapshot(ty) => {
                let (_n_snapshots, inner_ty) = peel_snapshots(db, *ty);
                inner_ty.module_id(db)
            }
            TypeLongId::GenericParameter(_) => None,
            TypeLongId::Var(_) => None,
            TypeLongId::Coupon(function_id) => function_id
                .get_concrete(db)
                .generic_function
                .module_file_id(db)
                .map(|module_file_id| module_file_id.0),
            TypeLongId::Missing(_) => None,
            TypeLongId::Tuple(_) => None,
            TypeLongId::ImplType(_) => None,
            TypeLongId::FixedSizeArray { .. } => None,
            TypeLongId::Closure(closure) => {
                if let Ok(function_id) = closure.parent_function {
                    function_id
                        .get_concrete(db)
                        .generic_function
                        .module_file_id(db.upcast())
                        .map(|module_file_id| module_file_id.0)
                } else {
                    None
                }
            }
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for TypeLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        let def_db = db.upcast();
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
                write!(f, "{}", generic_param.name(def_db).unwrap_or_else(|| "_".into()))
            }
            TypeLongId::ImplType(impl_type_id) => {
                write!(f, "{:?}::{}", impl_type_id.impl_id.debug(db), impl_type_id.ty.name(def_db))
            }
            TypeLongId::Var(var) => write!(f, "?{}", var.id.0),
            TypeLongId::Coupon(function_id) => write!(f, "{}::Coupon", function_id.full_path(db)),
            TypeLongId::Missing(_) => write!(f, "<missing>"),
            TypeLongId::FixedSizeArray { type_id, size } => {
                write!(f, "[{}; {:?}]", type_id.format(db), size.debug(db.elongate()))
            }
            TypeLongId::Closure(closure) => {
                write!(f, "{:?}", closure.debug(db.elongate()))
            }
        }
    }
}

/// Head of a type.
///
/// A type that is not one of {generic param, type variable, impl type} has a head, which represents
/// the kind of the root node in its type tree. This is used for caching queries for fast lookups
/// when the type is not completely inferred yet.
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
        let generic_type_format = self.generic_type(db).format(db.upcast());
        let mut generic_args = self.generic_args(db).into_iter();
        if let Some(first) = generic_args.next() {
            // Soft limit for the number of chars in the formatted type.
            const CHARS_BOUND: usize = 500;
            let mut f = generic_type_format;
            f.push_str("::<");
            f.push_str(&first.format(db));
            for arg in generic_args {
                f.push_str(", ");
                if f.len() > CHARS_BOUND {
                    // If the formatted type is becoming too long, add short version of arguments.
                    f.push_str(&arg.short_name(db));
                } else {
                    f.push_str(&arg.format(db));
                }
            }
            f.push('>');
            f
        } else {
            generic_type_format
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

/// A type id of a closure function.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ClosureTypeLongId {
    pub param_tys: Vec<TypeId>,
    pub ret_ty: TypeId,
    /// The set of types captured by the closure, this field is used to determined if the
    /// closure has Drop, Destruct or PanicDestruct.
    /// A vector as the fields needs to be hashable.
    pub captured_types: Vec<TypeId>,
    /// The parent function of the closure or an error.
    pub parent_function: Maybe<FunctionId>,
    /// Every closure has a unique type that is based on the stable location of its wrapper.
    #[dont_rewrite]
    pub wrapper_location: StableLocation,
}

impl DebugWithDb<dyn SemanticGroup> for ClosureTypeLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{{closure@{:?}}}", self.wrapper_location.debug(db.upcast()))
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
        if let crate::items::imp::ImplLongId::Concrete(concrete_impl) = impl_id.lookup_intern(db) {
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

/// A wrapper around ImplTypeById that implements Ord for saving in an ordered collection.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImplTypeById(ImplTypeId);

impl From<ImplTypeId> for ImplTypeById {
    fn from(impl_type_id: ImplTypeId) -> Self {
        Self(impl_type_id)
    }
}
impl Ord for ImplTypeById {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0
            .impl_id
            .get_internal_id()
            .cmp(other.0.impl_id.get_internal_id())
            .then_with(|| self.0.ty.get_internal_id().cmp(other.0.ty.get_internal_id()))
    }
}
impl PartialOrd for ImplTypeById {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// TODO(spapini): add a query wrapper.
/// Resolves a type given a module and a path. Used for resolving from non-statement context.
pub fn resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, resolver, ty_syntax, None)
        .unwrap_or_else(|diag_added| TypeId::missing(db, diag_added))
}
/// Resolves a type given a module and a path. `statement_env` should be provided if called from
/// statement context.
pub fn resolve_type_with_environment(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
    statement_env: Option<&mut Environment>,
) -> TypeId {
    maybe_resolve_type(db, diagnostics, resolver, ty_syntax, statement_env)
        .unwrap_or_else(|diag_added| TypeId::missing(db, diag_added))
}
pub fn maybe_resolve_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    ty_syntax: &ast::Expr,
    mut statement_env: Option<&mut Environment>,
) -> Maybe<TypeId> {
    let syntax_db = db.upcast();
    Ok(match ty_syntax {
        ast::Expr::Path(path) => {
            match resolver.resolve_concrete_path_ex(
                diagnostics,
                path,
                NotFoundItemType::Type,
                statement_env,
            )? {
                ResolvedConcreteItem::Type(ty) => ty,
                _ => {
                    return Err(diagnostics.report(path, NotAType));
                }
            }
        }
        ast::Expr::Parenthesized(expr_syntax) => resolve_type_with_environment(
            db,
            diagnostics,
            resolver,
            &expr_syntax.expr(syntax_db),
            statement_env,
        ),
        ast::Expr::Tuple(tuple_syntax) => {
            let sub_tys = tuple_syntax
                .expressions(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|subexpr_syntax| {
                    resolve_type_with_environment(
                        db,
                        diagnostics,
                        resolver,
                        &subexpr_syntax,
                        statement_env.as_deref_mut(),
                    )
                })
                .collect();
            TypeLongId::Tuple(sub_tys).intern(db)
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(syntax_db), ast::UnaryOperator::At(_)) =>
        {
            let ty = resolve_type_with_environment(
                db,
                diagnostics,
                resolver,
                &unary_syntax.expr(syntax_db),
                statement_env,
            );
            TypeLongId::Snapshot(ty).intern(db)
        }
        ast::Expr::Unary(unary_syntax)
            if matches!(unary_syntax.op(syntax_db), ast::UnaryOperator::Desnap(_)) =>
        {
            let ty = resolve_type_with_environment(
                db,
                diagnostics,
                resolver,
                &unary_syntax.expr(syntax_db),
                statement_env,
            );
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
            let ty = resolve_type_with_environment(db, diagnostics, resolver, ty, statement_env);
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
            let mut ctx = ComputationContext::new(
                db,
                diagnostics,
                resolver,
                None,
                environment,
                ContextFunction::Global,
            );
            let size_expr_syntax = size_clause.size(syntax_db);
            let size = compute_expr_semantic(&mut ctx, &size_expr_syntax);
            let const_value = resolve_const_expr_and_evaluate(
                db,
                &mut ctx,
                &size,
                size_expr_syntax.stable_ptr().untyped(),
                get_usize_ty(db),
                false,
            );
            if matches!(const_value, ConstValue::Int(_, _) | ConstValue::Generic(_)) {
                Ok(Some(const_value.intern(db)))
            } else {
                Err(diagnostics.report(syntax, FixedSizeArrayNonNumericSize))
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
    let constrains = db.generic_params_type_constraints(lookup_context.generic_params.clone());
    inference.conform_generic_params_type_constraints(&constrains);
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
        | TypeLongId::Closure(_) => false,
        TypeLongId::FixedSizeArray { type_id, size } => {
            db.single_value_type(type_id)?
                || matches!(size.lookup_intern(db),
                            ConstValue::Int(value, _) if value.is_zero())
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
                for (_, member) in db.struct_members(id.struct_id(db))?.iter() {
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
        | TypeLongId::ImplType(_)
        | TypeLongId::Closure(_) => {}
        TypeLongId::FixedSizeArray { type_id, size } => {
            if matches!(size.lookup_intern(db), ConstValue::Int(value,_) if value.is_zero())
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
    _cycle: &salsa::Cycle,
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
        | TypeLongId::ImplType(_) => false,
        TypeLongId::Coupon(function_id) => function_id.is_fully_concrete(db),
        TypeLongId::FixedSizeArray { type_id, size } => {
            type_id.is_fully_concrete(db) && size.is_fully_concrete(db)
        }
        TypeLongId::Closure(closure) => {
            closure.param_tys.iter().all(|param| param.is_fully_concrete(db))
                && closure.ret_ty.is_fully_concrete(db)
        }
    }
}

pub fn priv_type_is_var_free(db: &dyn SemanticGroup, ty: TypeId) -> bool {
    match ty.lookup_intern(db) {
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

pub fn priv_type_short_name(db: &dyn SemanticGroup, ty: TypeId) -> String {
    match ty.lookup_intern(db) {
        TypeLongId::Concrete(concrete_type_id) => {
            let mut result = concrete_type_id.generic_type(db).format(db.upcast());
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
            format!("[{}; {:?}", type_id.short_name(db), size.debug(db.elongate()))
        }
        other => other.format(db),
    }
}

/// Peels all wrapping Snapshot (`@`) from the type.
/// Returns the number of peeled snapshots and the inner type.
pub fn peel_snapshots(db: &dyn SemanticGroup, ty: TypeId) -> (usize, TypeLongId) {
    peel_snapshots_ex(db, ty.lookup_intern(db))
}

/// Same as `peel_snapshots`, but takes a `TypeLongId` instead of a `TypeId`.
pub fn peel_snapshots_ex(db: &dyn SemanticGroup, mut long_ty: TypeLongId) -> (usize, TypeLongId) {
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
