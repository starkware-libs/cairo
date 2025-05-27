//! Bidirectional type inference.

use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    GlobalUseId, ImplAliasId, ImplDefId, ImplFunctionId, ImplImplDefId, LanguageElementId,
    LocalVarId, LookupItemId, MacroCallId, MemberId, NamedLanguageElementId, ParamId, StructId,
    TraitConstantId, TraitFunctionId, TraitId, TraitImplId, TraitTypeId, VarId, VariantId,
};
use cairo_lang_diagnostics::{DiagnosticAdded, Maybe, skip_diagnostic};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::deque::Deque;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{
    Intern, LookupIntern, define_short_id, extract_matches, try_extract_matches,
};

use self::canonic::{CanonicalImpl, CanonicalMapping, CanonicalTrait, NoError};
use self::solver::{Ambiguity, SolutionSet, enrich_lookup_context};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::objects::*;
use crate::expr::pattern::*;
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplFunctionBodyId, ImplGenericFunctionId,
    ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{
    GeneratedImplId, GeneratedImplItems, GeneratedImplLongId, ImplId, ImplImplId, ImplLongId,
    ImplLookupContext, UninferredGeneratedImplId, UninferredGeneratedImplLongId, UninferredImpl,
};
use crate::items::trt::{
    ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId, ConcreteTraitTypeId,
    ConcreteTraitTypeLongId,
};
use crate::substitution::{HasDb, RewriteResult, SemanticRewriter};
use crate::types::{
    ClosureTypeLongId, ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId,
    ImplTypeById, ImplTypeId,
};
use crate::{
    ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId,
    ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant,
    FunctionId, FunctionLongId, GenericArgumentId, GenericParam, LocalVariable, MatchArmSelector,
    Member, Parameter, SemanticObject, Signature, TypeId, TypeLongId, ValueSelectorArm,
    add_basic_rewrites, add_expr_rewrites, add_rewrite, semantic_object_for_id,
};

pub mod canonic;
pub mod conform;
pub mod infers;
pub mod solver;

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct TypeVar<'db> {
    pub inference_id: InferenceId<'db>,
    pub id: LocalTypeVarId,
}

/// A const variable, created when a generic const argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub struct ConstVar<'db> {
    pub inference_id: InferenceId<'db>,
    pub id: LocalConstVarId,
}

/// An id for an inference context. Each inference variable is associated with an inference id.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum InferenceId<'db> {
    LookupItemDeclaration(LookupItemId<'db>),
    LookupItemGenerics(LookupItemId<'db>),
    LookupItemDefinition(LookupItemId<'db>),
    ImplDefTrait(ImplDefId<'db>),
    ImplAliasImplDef(ImplAliasId<'db>),
    GenericParam(GenericParamId<'db>),
    GenericImplParamTrait(GenericParamId<'db>),
    GlobalUseStar(GlobalUseId<'db>),
    MacroCall(MacroCallId<'db>),
    Canonical,
    /// For resolving that will not be used anywhere in the semantic model.
    NoContext,
}

/// An impl variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplVar<'db> {
    pub inference_id: InferenceId<'db>,
    #[dont_rewrite]
    pub id: LocalImplVarId,
    pub concrete_trait_id: ConcreteTraitId<'db>,
    #[dont_rewrite]
    pub lookup_context: ImplLookupContext<'db>,
}
impl<'db> ImplVar<'db> {
    pub fn intern(&self, db: &'db dyn SemanticGroup) -> ImplVarId<'db> {
        self.clone().intern(db)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject, salsa::Update)]
pub struct LocalTypeVarId(pub usize);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject, salsa::Update)]
pub struct LocalImplVarId(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject, salsa::Update)]
pub struct LocalConstVarId(pub usize);

define_short_id!(ImplVarId, ImplVar<'db>, SemanticGroup, lookup_intern_impl_var, intern_impl_var);
impl<'db> ImplVarId<'db> {
    pub fn id(&self, db: &dyn SemanticGroup) -> LocalImplVarId {
        self.lookup_intern(db).id
    }
    pub fn concrete_trait_id(&self, db: &'db dyn SemanticGroup) -> ConcreteTraitId<'db> {
        self.lookup_intern(db).concrete_trait_id
    }
    pub fn lookup_context(&self, db: &'db dyn SemanticGroup) -> ImplLookupContext<'db> {
        self.lookup_intern(db).lookup_context
    }
}
semantic_object_for_id!(ImplVarId<'a>, lookup_intern_impl_var, intern_impl_var, ImplVar<'a>);

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, SemanticObject, salsa::Update)]
pub enum InferenceVar {
    Type(LocalTypeVarId),
    Const(LocalConstVarId),
    Impl(LocalImplVarId),
}

// TODO(spapini): Add to diagnostics.
#[derive(Clone, Debug, Eq, Hash, PartialEq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum InferenceError<'db> {
    /// An inference error wrapping a previously reported error.
    Reported(DiagnosticAdded),
    Cycle(InferenceVar),
    TypeKindMismatch {
        ty0: TypeId<'db>,
        ty1: TypeId<'db>,
    },
    ConstKindMismatch {
        const0: ConstValueId<'db>,
        const1: ConstValueId<'db>,
    },
    ImplKindMismatch {
        impl0: ImplId<'db>,
        impl1: ImplId<'db>,
    },
    GenericArgMismatch {
        garg0: GenericArgumentId<'db>,
        garg1: GenericArgumentId<'db>,
    },
    TraitMismatch {
        trt0: TraitId<'db>,
        trt1: TraitId<'db>,
    },
    ImplTypeMismatch {
        impl_id: ImplId<'db>,
        trait_type_id: TraitTypeId<'db>,
        ty0: TypeId<'db>,
        ty1: TypeId<'db>,
    },
    GenericFunctionMismatch {
        func0: GenericFunctionId<'db>,
        func1: GenericFunctionId<'db>,
    },
    ConstNotInferred,
    // TODO(spapini): These are only used for external interface. Separate them along with the
    // finalize() function to a wrapper.
    NoImplsFound(ConcreteTraitId<'db>),
    Ambiguity(Ambiguity<'db>),
    TypeNotInferred(TypeId<'db>),
}
impl<'db> InferenceError<'db> {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            InferenceError::Reported(_) => "Inference error occurred.".into(),
            InferenceError::Cycle(_var) => "Inference cycle detected".into(),
            InferenceError::TypeKindMismatch { ty0, ty1 } => {
                format!("Type mismatch: `{:?}` and `{:?}`.", ty0.debug(db), ty1.debug(db))
            }
            InferenceError::ConstKindMismatch { const0, const1 } => {
                format!("Const mismatch: `{:?}` and `{:?}`.", const0.debug(db), const1.debug(db))
            }
            InferenceError::ImplKindMismatch { impl0, impl1 } => {
                format!("Impl mismatch: `{:?}` and `{:?}`.", impl0.debug(db), impl1.debug(db))
            }
            InferenceError::GenericArgMismatch { garg0, garg1 } => {
                format!(
                    "Generic arg mismatch: `{:?}` and `{:?}`.",
                    garg0.debug(db),
                    garg1.debug(db)
                )
            }
            InferenceError::TraitMismatch { trt0, trt1 } => {
                format!("Trait mismatch: `{:?}` and `{:?}`.", trt0.debug(db), trt1.debug(db))
            }
            InferenceError::ConstNotInferred => "Failed to infer constant.".into(),
            InferenceError::NoImplsFound(concrete_trait_id) => {
                let info = db.core_info();
                let trait_id = concrete_trait_id.trait_id(db);
                if trait_id == info.numeric_literal_trt {
                    let generic_type = extract_matches!(
                        concrete_trait_id.generic_args(db)[0],
                        GenericArgumentId::Type
                    );
                    return format!(
                        "Mismatched types. The type `{:?}` cannot be created from a numeric \
                         literal.",
                        generic_type.debug(db)
                    );
                } else if trait_id == info.string_literal_trt {
                    let generic_type = extract_matches!(
                        concrete_trait_id.generic_args(db)[0],
                        GenericArgumentId::Type
                    );
                    return format!(
                        "Mismatched types. The type `{:?}` cannot be created from a string \
                         literal.",
                        generic_type.debug(db)
                    );
                }
                format!(
                    "Trait has no implementation in context: {:?}.",
                    concrete_trait_id.debug(db)
                )
            }
            InferenceError::Ambiguity(ambiguity) => ambiguity.format(db),
            InferenceError::TypeNotInferred(ty) => {
                format!("Type annotations needed. Failed to infer {:?}.", ty.debug(db))
            }
            InferenceError::GenericFunctionMismatch { func0, func1 } => {
                format!("Function mismatch: `{}` and `{}`.", func0.format(db), func1.format(db))
            }
            InferenceError::ImplTypeMismatch { impl_id, trait_type_id, ty0, ty1 } => {
                format!(
                    "`{}::{}` type mismatch: `{:?}` and `{:?}`.",
                    impl_id.format(db),
                    trait_type_id.name(db),
                    ty0.debug(db),
                    ty1.debug(db)
                )
            }
        }
    }
}

impl<'db> InferenceError<'db> {
    pub fn report(
        &self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
    ) -> DiagnosticAdded {
        match self {
            InferenceError::Reported(diagnostic_added) => *diagnostic_added,
            _ => diagnostics
                .report(stable_ptr, SemanticDiagnosticKind::InternalInferenceError(self.clone())),
        }
    }
}

/// This struct is used to ensure that when an inference error occurs, it is properly set in the
/// `Inference` object, and then properly consumed.
///
/// It must not be constructed directly. Instead, it is returned by [Inference::set_error].
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct ErrorSet;

pub type InferenceResult<T> = Result<T, ErrorSet>;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
pub enum InferenceErrorStatus {
    Pending,
    Consumed,
}

/// A mapping of an impl var's trait items to concrete items
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, SemanticObject, salsa::Update)]
pub struct ImplVarTraitItemMappings<'db> {
    /// The trait types of the impl var.
    types: OrderedHashMap<TraitTypeId<'db>, TypeId<'db>>,
    /// The trait constants of the impl var.
    constants: OrderedHashMap<TraitConstantId<'db>, ConstValueId<'db>>,
    /// The trait impls of the impl var.
    impls: OrderedHashMap<TraitImplId<'db>, ImplId<'db>>,
}

/// State of inference.
#[derive(Debug, DebugWithDb, PartialEq, Eq, salsa::Update)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceData<'db> {
    pub inference_id: InferenceId<'db>,
    /// Current inferred assignment for type variables.
    pub type_assignment: OrderedHashMap<LocalTypeVarId, TypeId<'db>>,
    /// Current inferred assignment for const variables.
    pub const_assignment: OrderedHashMap<LocalConstVarId, ConstValueId<'db>>,
    /// Current inferred assignment for impl variables.
    pub impl_assignment: OrderedHashMap<LocalImplVarId, ImplId<'db>>,
    /// Unsolved impl variables mapping to a maps of trait items to a corresponding item variable.
    /// Upon solution of the trait conforms the fully known item to the variable.
    pub impl_vars_trait_item_mappings: HashMap<LocalImplVarId, ImplVarTraitItemMappings<'db>>,
    /// Type variables.
    pub type_vars: Vec<TypeVar<'db>>,
    /// Const variables.
    pub const_vars: Vec<ConstVar<'db>>,
    /// Impl variables.
    pub impl_vars: Vec<ImplVar<'db>>,
    /// Mapping from variables to stable pointers, if exist.
    pub stable_ptrs: HashMap<InferenceVar, SyntaxStablePtrId<'db>>,
    /// Inference variables that are pending to be solved.
    pending: Deque<LocalImplVarId>,
    /// Inference variables that have been refuted - no solutions exist.
    refuted: Vec<LocalImplVarId>,
    /// Inference variables that have been solved.
    solved: Vec<LocalImplVarId>,
    /// Inference variables that are currently ambiguous. May be solved later.
    ambiguous: Vec<(LocalImplVarId, Ambiguity<'db>)>,
    /// Mapping from impl types to type variables.
    pub impl_type_bounds: Arc<BTreeMap<ImplTypeById<'db>, TypeId<'db>>>,

    // Error handling members.
    /// The current error status.
    pub error_status: Result<(), InferenceErrorStatus>,
    /// `Some` only when error_state is Err(Pending).
    error: Option<InferenceError<'db>>,
    /// `Some` only when error_state is Err(Consumed).
    consumed_error: Option<DiagnosticAdded>,
}
impl<'db> InferenceData<'db> {
    pub fn new(inference_id: InferenceId<'db>) -> Self {
        Self {
            inference_id,
            type_assignment: OrderedHashMap::default(),
            impl_assignment: OrderedHashMap::default(),
            const_assignment: OrderedHashMap::default(),
            impl_vars_trait_item_mappings: HashMap::new(),
            type_vars: Vec::new(),
            impl_vars: Vec::new(),
            const_vars: Vec::new(),
            stable_ptrs: HashMap::new(),
            pending: Deque::new(),
            refuted: Vec::new(),
            solved: Vec::new(),
            ambiguous: Vec::new(),
            impl_type_bounds: Default::default(),
            error_status: Ok(()),
            error: None,
            consumed_error: None,
        }
    }
    pub fn inference<'r>(&'r mut self, db: &'db dyn SemanticGroup) -> Inference<'db, 'r> {
        Inference::new(db, self)
    }
    pub fn clone_with_inference_id(
        &self,
        db: &'db dyn SemanticGroup,
        inference_id: InferenceId<'db>,
    ) -> InferenceData<'db> {
        let mut inference_id_replacer =
            InferenceIdReplacer::new(db, self.inference_id, inference_id);
        Self {
            inference_id,
            type_assignment: self
                .type_assignment
                .iter()
                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                .collect(),
            const_assignment: self
                .const_assignment
                .iter()
                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                .collect(),
            impl_assignment: self
                .impl_assignment
                .iter()
                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                .collect(),
            impl_vars_trait_item_mappings: self
                .impl_vars_trait_item_mappings
                .iter()
                .map(|(k, mappings)| {
                    (
                        *k,
                        ImplVarTraitItemMappings {
                            types: mappings
                                .types
                                .iter()
                                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                                .collect(),
                            constants: mappings
                                .constants
                                .iter()
                                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                                .collect(),
                            impls: mappings
                                .impls
                                .iter()
                                .map(|(k, v)| (*k, inference_id_replacer.rewrite(*v).no_err()))
                                .collect(),
                        },
                    )
                })
                .collect(),
            type_vars: inference_id_replacer.rewrite(self.type_vars.clone()).no_err(),
            const_vars: inference_id_replacer.rewrite(self.const_vars.clone()).no_err(),
            impl_vars: inference_id_replacer.rewrite(self.impl_vars.clone()).no_err(),
            stable_ptrs: self.stable_ptrs.clone(),
            pending: inference_id_replacer.rewrite(self.pending.clone()).no_err(),
            refuted: inference_id_replacer.rewrite(self.refuted.clone()).no_err(),
            solved: inference_id_replacer.rewrite(self.solved.clone()).no_err(),
            ambiguous: inference_id_replacer.rewrite(self.ambiguous.clone()).no_err(),
            // we do not need to rewrite the impl type bounds, as they all should be var free.
            impl_type_bounds: self.impl_type_bounds.clone(),

            error_status: self.error_status,
            error: self.error.clone(),
            consumed_error: self.consumed_error,
        }
    }
    pub fn temporary_clone(&self) -> InferenceData<'db> {
        Self {
            inference_id: self.inference_id,
            type_assignment: self.type_assignment.clone(),
            const_assignment: self.const_assignment.clone(),
            impl_assignment: self.impl_assignment.clone(),
            impl_vars_trait_item_mappings: self.impl_vars_trait_item_mappings.clone(),
            type_vars: self.type_vars.clone(),
            const_vars: self.const_vars.clone(),
            impl_vars: self.impl_vars.clone(),
            stable_ptrs: self.stable_ptrs.clone(),
            pending: self.pending.clone(),
            refuted: self.refuted.clone(),
            solved: self.solved.clone(),
            ambiguous: self.ambiguous.clone(),
            impl_type_bounds: self.impl_type_bounds.clone(),
            error_status: self.error_status,
            error: self.error.clone(),
            consumed_error: self.consumed_error,
        }
    }
}

/// State of inference. A system of inference constraints.
pub struct Inference<'db: 'id, 'id> {
    db: &'db dyn SemanticGroup,
    pub data: &'id mut InferenceData<'db>,
}

impl<'db, 'id> Deref for Inference<'db, 'id> {
    type Target = InferenceData<'db>;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}
impl DerefMut for Inference<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl std::fmt::Debug for Inference<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = self.data.debug(self.db.elongate());
        write!(f, "{x:?}")
    }
}

impl<'db, 'id> Inference<'db, 'id> {
    fn new(db: &'db dyn SemanticGroup, data: &'id mut InferenceData<'db>) -> Self {
        Self { db, data }
    }

    /// Getter for an [ImplVar].
    fn impl_var(&self, var_id: LocalImplVarId) -> &ImplVar<'db> {
        &self.impl_vars[var_id.0]
    }

    /// Getter for an impl var assignment.
    pub fn impl_assignment(&self, var_id: LocalImplVarId) -> Option<ImplId<'db>> {
        self.impl_assignment.get(&var_id).copied()
    }

    /// Getter for a type var assignment.
    fn type_assignment(&self, var_id: LocalTypeVarId) -> Option<TypeId<'db>> {
        self.type_assignment.get(&var_id).copied()
    }

    /// Allocates a new [TypeVar] for an unknown type that needs to be inferred.
    /// Returns a wrapping TypeId.
    pub fn new_type_var(&mut self, stable_ptr: Option<SyntaxStablePtrId<'db>>) -> TypeId<'db> {
        let var = self.new_type_var_raw(stable_ptr);

        TypeLongId::Var(var).intern(self.db)
    }

    /// Allocates a new [TypeVar] for an unknown type that needs to be inferred.
    /// Returns the variable id.
    pub fn new_type_var_raw(&mut self, stable_ptr: Option<SyntaxStablePtrId<'db>>) -> TypeVar<'db> {
        let var =
            TypeVar { inference_id: self.inference_id, id: LocalTypeVarId(self.type_vars.len()) };
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Type(var.id), stable_ptr);
        }
        self.type_vars.push(var);
        var
    }

    /// Sets the infrence's impl type bounds to the given map, and rewrittes the types so all the
    /// types are var free.
    pub fn set_impl_type_bounds(
        &mut self,
        impl_type_bounds: OrderedHashMap<ImplTypeId<'db>, TypeId<'db>>,
    ) {
        let impl_type_bounds_finalized = impl_type_bounds
            .iter()
            .filter_map(|(impl_type, ty)| {
                let rewritten_type = self.rewrite(ty.lookup_intern(self.db)).no_err();
                if !matches!(rewritten_type, TypeLongId::Var(_)) {
                    return Some(((*impl_type).into(), rewritten_type.intern(self.db)));
                }
                // conformed the var type to the original impl type to remove it from the pending
                // list.
                self.conform_ty(*ty, TypeLongId::ImplType(*impl_type).intern(self.db)).ok();
                None
            })
            .collect();

        self.data.impl_type_bounds = Arc::new(impl_type_bounds_finalized);
    }

    /// Allocates a new [ConstVar] for an unknown consts that needs to be inferred.
    /// Returns a wrapping [ConstValueId].
    pub fn new_const_var(
        &mut self,
        stable_ptr: Option<SyntaxStablePtrId<'db>>,
        ty: TypeId<'db>,
    ) -> ConstValueId<'db> {
        let var = self.new_const_var_raw(stable_ptr);
        ConstValue::Var(var, ty).intern(self.db)
    }

    /// Allocates a new [ConstVar] for an unknown type that needs to be inferred.
    /// Returns the variable id.
    pub fn new_const_var_raw(
        &mut self,
        stable_ptr: Option<SyntaxStablePtrId<'db>>,
    ) -> ConstVar<'db> {
        let var = ConstVar {
            inference_id: self.inference_id,
            id: LocalConstVarId(self.const_vars.len()),
        };
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Const(var.id), stable_ptr);
        }
        self.const_vars.push(var);
        var
    }

    /// Allocates a new [ImplVar] for an unknown type that needs to be inferred.
    /// Returns a wrapping ImplId.
    pub fn new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId<'db>,
        stable_ptr: Option<SyntaxStablePtrId<'db>>,
        lookup_context: ImplLookupContext<'db>,
    ) -> ImplId<'db> {
        let var = self.new_impl_var_raw(lookup_context, concrete_trait_id, stable_ptr);
        ImplLongId::ImplVar(self.impl_var(var).intern(self.db)).intern(self.db)
    }

    /// Allocates a new [ImplVar] for an unknown type that needs to be inferred.
    /// Returns the variable id.
    fn new_impl_var_raw(
        &mut self,
        lookup_context: ImplLookupContext<'db>,
        concrete_trait_id: ConcreteTraitId<'db>,
        stable_ptr: Option<SyntaxStablePtrId<'db>>,
    ) -> LocalImplVarId {
        let mut lookup_context = lookup_context;
        lookup_context.insert_module(concrete_trait_id.trait_id(self.db).module_file_id(self.db).0);

        let id = LocalImplVarId(self.impl_vars.len());
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Impl(id), stable_ptr);
        }
        let var =
            ImplVar { inference_id: self.inference_id, id, concrete_trait_id, lookup_context };
        self.impl_vars.push(var);
        self.pending.push_back(id);
        id
    }

    /// Solves the inference system. After a successful solve, there are no more pending impl
    /// inferences.
    /// Returns whether the inference was successful. If not, the error may be found by
    /// `.error_state()`.
    pub fn solve(&mut self) -> InferenceResult<()> {
        self.solve_ex().map_err(|(err_set, _)| err_set)
    }

    /// Same as `solve`, but returns the error stable pointer if an error occurred.
    fn solve_ex(&mut self) -> Result<(), (ErrorSet, Option<SyntaxStablePtrId<'db>>)> {
        let ambiguous = std::mem::take(&mut self.ambiguous).into_iter();
        self.pending.extend(ambiguous.map(|(var, _)| var));
        while let Some(var) = self.pending.pop_front() {
            // First inference error stops inference.
            self.solve_single_pending(var).map_err(|err_set| {
                (err_set, self.stable_ptrs.get(&InferenceVar::Impl(var)).copied())
            })?;
        }
        Ok(())
    }

    fn solve_single_pending(&mut self, var: LocalImplVarId) -> InferenceResult<()> {
        if self.impl_assignment.contains_key(&var) {
            return Ok(());
        }
        let solution = match self.impl_var_solution_set(var)? {
            SolutionSet::None => {
                self.refuted.push(var);
                return Ok(());
            }
            SolutionSet::Ambiguous(ambiguity) => {
                self.ambiguous.push((var, ambiguity));
                return Ok(());
            }
            SolutionSet::Unique(solution) => solution,
        };

        // Solution found. Assign it.
        self.assign_local_impl(var, solution)?;

        // Something changed.
        self.solved.push(var);
        let ambiguous = std::mem::take(&mut self.ambiguous).into_iter();
        self.pending.extend(ambiguous.map(|(var, _)| var));

        Ok(())
    }

    /// Returns the solution set status for the inference:
    /// Whether there is a unique solution, multiple solutions, no solutions or an error.
    pub fn solution_set(&mut self) -> InferenceResult<SolutionSet<'db, ()>> {
        self.solve()?;
        if !self.refuted.is_empty() {
            return Ok(SolutionSet::None);
        }
        if let Some((_, ambiguity)) = self.ambiguous.first() {
            return Ok(SolutionSet::Ambiguous(ambiguity.clone()));
        }
        assert!(self.pending.is_empty(), "solution() called on an unsolved solver");
        Ok(SolutionSet::Unique(()))
    }

    /// Finalizes the inference by inferring uninferred numeric literals as felt252.
    /// Returns an error and does not report it.
    pub fn finalize_without_reporting(
        &mut self,
    ) -> Result<(), (ErrorSet, Option<SyntaxStablePtrId<'db>>)> {
        if self.error_status.is_err() {
            // TODO(yuval): consider adding error location to the set error.
            return Err((ErrorSet, None));
        }
        let info = self.db.core_info();
        let numeric_trait_id = info.numeric_literal_trt;
        let felt_ty = info.felt252;

        // Conform all uninferred numeric literals to felt252.
        loop {
            let mut changed = false;
            self.solve_ex()?;
            for (var, _) in self.ambiguous.clone() {
                let impl_var = self.impl_var(var).clone();
                if impl_var.concrete_trait_id.trait_id(self.db) != numeric_trait_id {
                    continue;
                }
                // Uninferred numeric trait. Resolve as felt252.
                let ty = extract_matches!(
                    impl_var.concrete_trait_id.generic_args(self.db)[0],
                    GenericArgumentId::Type
                );
                if self.rewrite(ty).no_err() == felt_ty {
                    continue;
                }
                self.conform_ty(ty, felt_ty).map_err(|err_set| {
                    (err_set, self.stable_ptrs.get(&InferenceVar::Impl(impl_var.id)).copied())
                })?;
                changed = true;
                break;
            }
            if !changed {
                break;
            }
        }
        assert!(
            self.pending.is_empty(),
            "pending should all be solved by this point. Guaranteed by solve()."
        );

        let Some((var, err)) = self.first_undetermined_variable() else {
            return Ok(());
        };
        Err((self.set_error(err), self.stable_ptrs.get(&var).copied()))
    }

    /// Finalizes the inference and report diagnostics if there are any errors.
    /// All the remaining type vars are mapped to the `missing` type, to prevent additional
    /// diagnostics.
    pub fn finalize<'m>(
        &'m mut self,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
    ) {
        if let Err((err_set, err_stable_ptr)) = self.finalize_without_reporting() {
            let diag = self.report_on_pending_error(
                err_set,
                diagnostics,
                err_stable_ptr.unwrap_or(stable_ptr),
            );

            let ty_missing = TypeId::missing(self.db, diag);
            for var in &self.data.type_vars {
                self.data.type_assignment.entry(var.id).or_insert(ty_missing);
            }
        }
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    /// Does not set the error but return it, which is ok as this is a private helper function.
    fn first_undetermined_variable(&mut self) -> Option<(InferenceVar, InferenceError<'db>)> {
        if let Some(var) = self.refuted.first().copied() {
            let impl_var = self.impl_var(var).clone();
            let concrete_trait_id = impl_var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).no_err();
            return Some((
                InferenceVar::Impl(var),
                InferenceError::NoImplsFound(concrete_trait_id),
            ));
        }
        let mut fallback_ret = None;
        if let Some((var, ambiguity)) = self.ambiguous.first() {
            // Note: do not rewrite `ambiguity`, since it is expressed in canonical variables.
            let ret =
                Some((InferenceVar::Impl(*var), InferenceError::Ambiguity(ambiguity.clone())));
            if !matches!(ambiguity, Ambiguity::WillNotInfer(_)) {
                return ret;
            } else {
                fallback_ret = ret;
            }
        }
        for (id, var) in self.type_vars.iter().enumerate() {
            if self.type_assignment(LocalTypeVarId(id)).is_none() {
                let ty = TypeLongId::Var(*var).intern(self.db);
                return Some((InferenceVar::Type(var.id), InferenceError::TypeNotInferred(ty)));
            }
        }
        for (id, var) in self.const_vars.iter().enumerate() {
            if !self.const_assignment.contains_key(&LocalConstVarId(id)) {
                let infernence_var = InferenceVar::Const(var.id);
                return Some((infernence_var, InferenceError::ConstNotInferred));
            }
        }
        fallback_ret
    }

    /// Assigns a value to a local impl variable id. See assign_impl().
    fn assign_local_impl(
        &mut self,
        var: LocalImplVarId,
        impl_id: ImplId<'db>,
    ) -> InferenceResult<ImplId<'db>> {
        let concrete_trait = impl_id
            .concrete_trait(self.db)
            .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
        self.conform_traits(self.impl_var(var).concrete_trait_id, concrete_trait)?;
        if let Some(other_impl) = self.impl_assignment(var) {
            return self.conform_impl(impl_id, other_impl);
        }
        if !impl_id.is_var_free(self.db) && self.impl_contains_var(impl_id, InferenceVar::Impl(var))
        {
            return Err(self.set_error(InferenceError::Cycle(InferenceVar::Impl(var))));
        }
        self.impl_assignment.insert(var, impl_id);
        if let Some(mappings) = self.impl_vars_trait_item_mappings.remove(&var) {
            for (trait_type_id, ty) in mappings.types {
                let impl_ty = self
                    .db
                    .impl_type_concrete_implized(ImplTypeId::new(impl_id, trait_type_id, self.db))
                    .map_err(|_| ErrorSet)?;
                if let Err(err_set) = self.conform_ty(ty, impl_ty) {
                    // Override the error with ImplTypeMismatch.
                    let ty0 = self.rewrite(ty).no_err();
                    let ty1 = self.rewrite(impl_ty).no_err();

                    self.error =
                        Some(InferenceError::ImplTypeMismatch { impl_id, trait_type_id, ty0, ty1 });
                    return Err(err_set);
                }
            }
            for (trait_constant, constant_id) in mappings.constants {
                self.conform_const(
                    constant_id,
                    self.db
                        .impl_constant_concrete_implized_value(ImplConstantId::new(
                            impl_id,
                            trait_constant,
                            self.db,
                        ))
                        .map_err(|_| ErrorSet)?,
                )?;
            }
            for (trait_impl, inner_impl_id) in mappings.impls {
                self.conform_impl(
                    inner_impl_id,
                    self.db
                        .impl_impl_concrete_implized(ImplImplId::new(impl_id, trait_impl, self.db))
                        .map_err(|_| ErrorSet)?,
                )?;
            }
        }
        Ok(impl_id)
    }

    /// Tries to assigns value to an [ImplVarId]. Return the assigned impl, or an error.
    fn assign_impl(
        &mut self,
        var_id: ImplVarId<'db>,
        impl_id: ImplId<'db>,
    ) -> InferenceResult<ImplId<'db>> {
        let var = var_id.lookup_intern(self.db);
        if var.inference_id != self.inference_id {
            return Err(self.set_error(InferenceError::ImplKindMismatch {
                impl0: ImplLongId::ImplVar(var_id).intern(self.db),
                impl1: impl_id,
            }));
        }
        self.assign_local_impl(var.id, impl_id)
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_ty(&mut self, var: TypeVar<'db>, ty: TypeId<'db>) -> InferenceResult<TypeId<'db>> {
        if var.inference_id != self.inference_id {
            return Err(self.set_error(InferenceError::TypeKindMismatch {
                ty0: TypeLongId::Var(var).intern(self.db),
                ty1: ty,
            }));
        }
        assert!(!self.type_assignment.contains_key(&var.id), "Cannot reassign variable.");
        let inference_var = InferenceVar::Type(var.id);
        if !ty.is_var_free(self.db) && self.ty_contains_var(ty, inference_var) {
            return Err(self.set_error(InferenceError::Cycle(inference_var)));
        }
        // If assigning var to var - making sure assigning to the lower id for proper canonization.
        if let TypeLongId::Var(other) = ty.lookup_intern(self.db) {
            if other.inference_id == self.inference_id && other.id.0 > var.id.0 {
                let var_ty = TypeLongId::Var(var).intern(self.db);
                self.type_assignment.insert(other.id, var_ty);
                return Ok(var_ty);
            }
        }
        self.type_assignment.insert(var.id, ty);
        Ok(ty)
    }

    /// Assigns a value to a [ConstVar]. Return the assigned const, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_const(
        &mut self,
        var: ConstVar<'db>,
        id: ConstValueId<'db>,
    ) -> InferenceResult<ConstValueId<'db>> {
        if var.inference_id != self.inference_id {
            return Err(self.set_error(InferenceError::ConstKindMismatch {
                const0: ConstValue::Var(var, TypeId::missing(self.db, skip_diagnostic()))
                    .intern(self.db),
                const1: id,
            }));
        }

        self.const_assignment.insert(var.id, id);
        Ok(id)
    }

    /// Computes the solution set for an impl variable with a recursive query.
    fn impl_var_solution_set(
        &mut self,
        var: LocalImplVarId,
    ) -> InferenceResult<SolutionSet<'db, ImplId<'db>>> {
        let impl_var = self.impl_var(var).clone();
        // Update the concrete trait of the impl var.
        let concrete_trait_id = self.rewrite(impl_var.concrete_trait_id).no_err();
        self.impl_vars[impl_var.id.0].concrete_trait_id = concrete_trait_id;
        let impl_var_trait_item_mappings =
            self.impl_vars_trait_item_mappings.get(&var).cloned().unwrap_or_default();
        let solution_set = self.trait_solution_set(
            concrete_trait_id,
            impl_var_trait_item_mappings,
            impl_var.lookup_context,
        )?;
        Ok(match solution_set {
            SolutionSet::None => SolutionSet::None,
            SolutionSet::Unique((canonical_impl, canonicalizer)) => {
                SolutionSet::Unique(canonical_impl.embed(self, &canonicalizer))
            }
            SolutionSet::Ambiguous(ambiguity) => SolutionSet::Ambiguous(ambiguity),
        })
    }

    /// Computes the solution set for a trait with a recursive query.
    pub fn trait_solution_set(
        &mut self,
        concrete_trait_id: ConcreteTraitId<'db>,
        impl_var_trait_item_mappings: ImplVarTraitItemMappings<'db>,
        mut lookup_context: ImplLookupContext<'db>,
    ) -> InferenceResult<SolutionSet<'db, (CanonicalImpl<'db>, CanonicalMapping<'db>)>> {
        let impl_var_trait_item_mappings = self.rewrite(impl_var_trait_item_mappings).no_err();
        // TODO(spapini): This is done twice. Consider doing it only here.
        let concrete_trait_id = self.rewrite(concrete_trait_id).no_err();
        enrich_lookup_context(self.db, concrete_trait_id, &mut lookup_context);

        // Don't try to resolve impls if the first generic param is a variable.
        let generic_args = concrete_trait_id.generic_args(self.db);
        match generic_args.first() {
            Some(GenericArgumentId::Type(ty)) => {
                if let TypeLongId::Var(_) = ty.lookup_intern(self.db) {
                    // Don't try to infer such impls.
                    return Ok(SolutionSet::Ambiguous(Ambiguity::WillNotInfer(concrete_trait_id)));
                }
            }
            Some(GenericArgumentId::Impl(imp)) => {
                // Don't try to infer such impls.
                if let ImplLongId::ImplVar(_) = imp.lookup_intern(self.db) {
                    return Ok(SolutionSet::Ambiguous(Ambiguity::WillNotInfer(concrete_trait_id)));
                }
            }
            Some(GenericArgumentId::Constant(const_value)) => {
                if let ConstValue::Var(_, _) = const_value.lookup_intern(self.db) {
                    // Don't try to infer such impls.
                    return Ok(SolutionSet::Ambiguous(Ambiguity::WillNotInfer(concrete_trait_id)));
                }
            }
            _ => {}
        };
        let (canonical_trait, canonicalizer) = CanonicalTrait::canonicalize(
            self.db,
            self.inference_id,
            concrete_trait_id,
            impl_var_trait_item_mappings,
        );
        // impl_type_bounds order is deterimend by the generic params of the function and therefore
        // is consistent.
        let solution_set = match self.db.canonic_trait_solutions(
            canonical_trait,
            lookup_context,
            (*self.data.impl_type_bounds).clone(),
        ) {
            Ok(solution_set) => solution_set,
            Err(err) => return Err(self.set_error(err)),
        };
        match solution_set {
            SolutionSet::None => Ok(SolutionSet::None),
            SolutionSet::Unique(canonical_impl) => {
                Ok(SolutionSet::Unique((canonical_impl, canonicalizer)))
            }
            SolutionSet::Ambiguous(ambiguity) => Ok(SolutionSet::Ambiguous(ambiguity)),
        }
    }

    /// Validate that the given impl is valid based on its negative impls arguments.
    /// Returns `SolutionSet::Unique(canonical_impl)` if the impl is valid and
    /// SolutionSet::Ambiguous(...) otherwise.
    fn validate_neg_impls<'m>(
        &'m mut self,
        lookup_context: &ImplLookupContext<'db>,
        canonical_impl: CanonicalImpl<'db>,
    ) -> InferenceResult<SolutionSet<'db, CanonicalImpl<'db>>> {
        /// Validates that no solution set is found for the negative impls.
        fn validate_no_solution_set<'db, 'mt>(
            inference: &mut Inference<'db, 'mt>,
            canonical_impl: CanonicalImpl<'db>,
            lookup_context: &ImplLookupContext<'db>,
            negative_impls_concrete_traits: impl Iterator<Item = Maybe<ConcreteTraitId<'db>>>,
        ) -> InferenceResult<SolutionSet<'db, CanonicalImpl<'db>>> {
            for concrete_trait_id in negative_impls_concrete_traits {
                let concrete_trait_id = concrete_trait_id.map_err(|diag_added| {
                    inference.set_error(InferenceError::Reported(diag_added))
                })?;
                for garg in concrete_trait_id.generic_args(inference.db) {
                    let GenericArgumentId::Type(ty) = garg else {
                        continue;
                    };
                    let ty = inference.rewrite(ty).no_err();
                    // If the negative impl has a generic argument that is not fully
                    // concrete we can't tell if we should rule out the candidate impl.
                    // For example if we have -TypeEqual<S, T> we can't tell if S and
                    // T are going to be assigned the same concrete type.
                    // We return `SolutionSet::Ambiguous` here to indicate that more
                    // information is needed.
                    // Closure can only have one type, even if it's not fully concrete, so can use
                    // it and not get ambiguity.
                    if !matches!(ty.lookup_intern(inference.db), TypeLongId::Closure(_))
                        && !ty.is_fully_concrete(inference.db)
                    {
                        // TODO(ilya): Try to detect the ambiguity earlier in the
                        // inference process.
                        return Ok(SolutionSet::Ambiguous(
                            Ambiguity::NegativeImplWithUnresolvedGenericArgs {
                                impl_id: canonical_impl.0,
                                ty,
                            },
                        ));
                    }
                }

                if !matches!(
                    inference.trait_solution_set(
                        concrete_trait_id,
                        ImplVarTraitItemMappings::default(),
                        lookup_context.clone()
                    )?,
                    SolutionSet::None
                ) {
                    // If a negative impl has an impl, then we should skip it.
                    return Ok(SolutionSet::None);
                }
            }

            Ok(SolutionSet::Unique(canonical_impl))
        }
        match canonical_impl.0.lookup_intern(self.db) {
            ImplLongId::Concrete(concrete_impl) => {
                let substitution = concrete_impl
                    .substitution(self.db)
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                let generic_params = self
                    .db
                    .impl_def_generic_params(concrete_impl.impl_def_id(self.db))
                    .map_err(|diag_added| self.set_error(InferenceError::Reported(diag_added)))?;
                let concrete_traits = generic_params
                    .iter()
                    .filter_map(|generic_param| {
                        try_extract_matches!(generic_param, GenericParam::NegImpl)
                    })
                    .map(|generic_param| {
                        substitution
                            .substitute(self.db, generic_param.clone())
                            .and_then(|generic_param| generic_param.concrete_trait)
                    });
                validate_no_solution_set(self, canonical_impl, lookup_context, concrete_traits)
            }
            ImplLongId::GeneratedImpl(generated_impl) => validate_no_solution_set(
                self,
                canonical_impl,
                lookup_context,
                generated_impl
                    .lookup_intern(self.db)
                    .generic_params
                    .iter()
                    .filter_map(|generic_param| {
                        try_extract_matches!(generic_param, GenericParam::NegImpl)
                    })
                    .map(|generic_param| generic_param.concrete_trait),
            ),
            ImplLongId::GenericParameter(_)
            | ImplLongId::ImplVar(_)
            | ImplLongId::ImplImpl(_)
            | ImplLongId::SelfImpl(_) => Ok(SolutionSet::Unique(canonical_impl)),
        }
    }

    // Error handling methods
    // ======================

    /// Sets an error in the inference state.
    /// Does nothing if an error is already set.
    /// Returns an `ErrorSet` that can be used in reporting the error.
    pub fn set_error(&mut self, err: InferenceError<'db>) -> ErrorSet {
        if self.error_status.is_err() {
            return ErrorSet;
        }
        self.error_status = if let InferenceError::Reported(diag_added) = err {
            self.consumed_error = Some(diag_added);
            Err(InferenceErrorStatus::Consumed)
        } else {
            self.error = Some(err);
            Err(InferenceErrorStatus::Pending)
        };
        ErrorSet
    }

    /// Returns whether an error is set (either pending or consumed).
    pub fn is_error_set(&self) -> InferenceResult<()> {
        if self.error_status.is_err() { Err(ErrorSet) } else { Ok(()) }
    }

    /// Consumes the error but doesn't report it. If there is no error, or the error is consumed,
    /// returns None. This should be used with caution. Always prefer to use
    /// (1) `report_on_pending_error` if possible, or (2) `consume_reported_error` which is safer.
    ///
    /// Gets an `ErrorSet` to "enforce" it is only called when an error is set.
    pub fn consume_error_without_reporting(
        &mut self,
        err_set: ErrorSet,
    ) -> Option<InferenceError<'db>> {
        self.consume_error_inner(err_set, skip_diagnostic())
    }

    /// Consumes the error that is already reported. If there is no error, or the error is consumed,
    /// does nothing. This should be used with caution. Always prefer to use
    /// `report_on_pending_error` if possible.
    ///
    /// Gets an `ErrorSet` to "enforce" it is only called when an error is set.
    /// Gets an `DiagnosticAdded` to "enforce" it is only called when a diagnostic was reported.
    pub fn consume_reported_error(&mut self, err_set: ErrorSet, diag_added: DiagnosticAdded) {
        self.consume_error_inner(err_set, diag_added);
    }

    /// Consumes the error and returns it, but doesn't report it. If there is no error, or the error
    /// is already consumed, returns None. This should be used with caution. Always prefer to use
    /// `report_on_pending_error` if possible.
    ///
    /// Gets an `ErrorSet` to "enforce" it is only called when an error is set.
    /// Gets an `DiagnosticAdded` to "enforce" it is only called when a diagnostic was reported.
    fn consume_error_inner(
        &mut self,
        _err_set: ErrorSet,
        diag_added: DiagnosticAdded,
    ) -> Option<InferenceError<'db>> {
        if self.error_status != Err(InferenceErrorStatus::Pending) {
            return None;
            // panic!("consume_error when there is no pending error");
        }
        self.error_status = Err(InferenceErrorStatus::Consumed);
        self.consumed_error = Some(diag_added);
        self.error.take()
    }

    /// Consumes the pending error, if any, and reports it.
    /// Should only be called when an error is set, otherwise it panics.
    /// Gets an `ErrorSet` to "enforce" it is only called when an error is set.
    /// If an error was set but it's already consumed, it doesn't report it again but returns the
    /// stored `DiagnosticAdded`.
    pub fn report_on_pending_error(
        &mut self,
        _err_set: ErrorSet,
        diagnostics: &mut SemanticDiagnostics<'db>,
        stable_ptr: SyntaxStablePtrId<'db>,
    ) -> DiagnosticAdded {
        let Err(state_error) = self.error_status else {
            panic!("report_on_pending_error should be called only on error");
        };
        match state_error {
            InferenceErrorStatus::Consumed => self
                .consumed_error
                .expect("consumed_error is not set although error_status is Err(Consumed)"),
            InferenceErrorStatus::Pending => {
                let diag_added = match mem::take(&mut self.error)
                    .expect("error is not set although error_status is Err(Pending)")
                {
                    InferenceError::TypeNotInferred(_) if diagnostics.error_count > 0 => {
                        // If we have other diagnostics, there is no need to TypeNotInferred.

                        // Note that `diagnostics` is not empty, so it is safe to return
                        // 'DiagnosticAdded' here.
                        skip_diagnostic()
                    }
                    diag => diag.report(diagnostics, stable_ptr),
                };

                self.error_status = Err(InferenceErrorStatus::Consumed);
                self.consumed_error = Some(diag_added);
                diag_added
            }
        }
    }

    /// If the current status is of a pending error, reports an alternative diagnostic, by calling
    /// `report`, and consumes the error. Otherwise, does nothing.
    pub fn report_modified_if_pending(
        &mut self,
        err_set: ErrorSet,
        report: impl FnOnce() -> DiagnosticAdded,
    ) {
        if self.error_status == Err(InferenceErrorStatus::Pending) {
            self.consume_reported_error(err_set, report());
        }
    }
}

impl<'a, 'mt> HasDb<&'a dyn SemanticGroup> for Inference<'a, 'mt> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a, 'mt>, Inference<'a, 'mt>, NoError, @exclude TypeLongId TypeId ImplLongId ImplId ConstValue);
add_expr_rewrites!(<'a, 'mt>, Inference<'a, 'mt>, NoError, @exclude);
add_rewrite!(<'a, 'mt>, Inference<'a, 'mt>, NoError, Ambiguity<'a>);
impl<'db, 'mt> SemanticRewriter<TypeId<'db>, NoError> for Inference<'db, 'mt> {
    fn internal_rewrite(&mut self, value: &mut TypeId<'db>) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db, 'mt> SemanticRewriter<ImplId<'db>, NoError> for Inference<'db, 'mt> {
    fn internal_rewrite(&mut self, value: &mut ImplId<'db>) -> Result<RewriteResult, NoError> {
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}
impl<'db, 'mt> SemanticRewriter<TypeLongId<'db>, NoError> for Inference<'db, 'mt> {
    fn internal_rewrite(&mut self, value: &mut TypeLongId<'db>) -> Result<RewriteResult, NoError> {
        match value {
            TypeLongId::Var(var) => {
                if let Some(type_id) = self.type_assignment.get(&var.id) {
                    let mut long_type_id = type_id.lookup_intern(self.db);
                    if let RewriteResult::Modified = self.internal_rewrite(&mut long_type_id)? {
                        *self.type_assignment.get_mut(&var.id).unwrap() =
                            long_type_id.clone().intern(self.db);
                    }
                    *value = long_type_id;
                    return Ok(RewriteResult::Modified);
                }
            }
            TypeLongId::ImplType(impl_type_id) => {
                if let Some(type_id) = self.impl_type_bounds.get(&((*impl_type_id).into())) {
                    *value = type_id.lookup_intern(self.db);
                    self.internal_rewrite(value)?;
                    return Ok(RewriteResult::Modified);
                }
                let impl_type_id_rewrite_result = self.internal_rewrite(impl_type_id)?;
                let impl_id = impl_type_id.impl_id();
                let trait_ty = impl_type_id.ty();
                return Ok(match impl_id.lookup_intern(self.db) {
                    ImplLongId::GenericParameter(_)
                    | ImplLongId::SelfImpl(_)
                    | ImplLongId::ImplImpl(_) => impl_type_id_rewrite_result,
                    ImplLongId::Concrete(_) => {
                        if let Ok(ty) = self.db.impl_type_concrete_implized(ImplTypeId::new(
                            impl_id, trait_ty, self.db,
                        )) {
                            *value = self.rewrite(ty).no_err().lookup_intern(self.db);
                            RewriteResult::Modified
                        } else {
                            impl_type_id_rewrite_result
                        }
                    }
                    ImplLongId::ImplVar(var) => {
                        *value = self.rewritten_impl_type(var, trait_ty).lookup_intern(self.db);
                        return Ok(RewriteResult::Modified);
                    }
                    ImplLongId::GeneratedImpl(generated) => {
                        *value = self
                            .rewrite(
                                *generated
                                    .lookup_intern(self.db)
                                    .impl_items
                                    .0
                                    .get(&impl_type_id.ty())
                                    .unwrap(),
                            )
                            .no_err()
                            .lookup_intern(self.db);
                        RewriteResult::Modified
                    }
                });
            }
            _ => {}
        }
        value.default_rewrite(self)
    }
}
impl<'db, 'mt> SemanticRewriter<ConstValue<'db>, NoError> for Inference<'db, 'mt> {
    fn internal_rewrite(&mut self, value: &mut ConstValue<'db>) -> Result<RewriteResult, NoError> {
        match value {
            ConstValue::Var(var, _) => {
                return Ok(if let Some(const_value_id) = self.const_assignment.get(&var.id) {
                    let mut const_value = const_value_id.lookup_intern(self.db);
                    if let RewriteResult::Modified = self.internal_rewrite(&mut const_value)? {
                        *self.const_assignment.get_mut(&var.id).unwrap() =
                            const_value.clone().intern(self.db);
                    }
                    *value = const_value;
                    RewriteResult::Modified
                } else {
                    RewriteResult::NoChange
                });
            }
            ConstValue::ImplConstant(impl_constant_id) => {
                let impl_constant_id_rewrite_result = self.internal_rewrite(impl_constant_id)?;
                let impl_id = impl_constant_id.impl_id();
                let trait_constant = impl_constant_id.trait_constant_id();
                return Ok(match impl_id.lookup_intern(self.db) {
                    ImplLongId::GenericParameter(_)
                    | ImplLongId::SelfImpl(_)
                    | ImplLongId::GeneratedImpl(_)
                    | ImplLongId::ImplImpl(_) => impl_constant_id_rewrite_result,
                    ImplLongId::Concrete(_) => {
                        if let Ok(constant) = self.db.impl_constant_concrete_implized_value(
                            ImplConstantId::new(impl_id, trait_constant, self.db),
                        ) {
                            *value = self.rewrite(constant).no_err().lookup_intern(self.db);
                            RewriteResult::Modified
                        } else {
                            impl_constant_id_rewrite_result
                        }
                    }
                    ImplLongId::ImplVar(var) => {
                        *value = self
                            .rewritten_impl_constant(var, trait_constant)
                            .lookup_intern(self.db);
                        return Ok(RewriteResult::Modified);
                    }
                });
            }
            _ => {}
        }
        value.default_rewrite(self)
    }
}
impl<'db, 'mt> SemanticRewriter<ImplLongId<'db>, NoError> for Inference<'db, 'mt> {
    fn internal_rewrite(&mut self, value: &mut ImplLongId<'db>) -> Result<RewriteResult, NoError> {
        match value {
            ImplLongId::ImplVar(var) => {
                let long_id = var.lookup_intern(self.db);
                // Relax the candidates.
                let impl_var_id = long_id.id;
                if let Some(impl_id) = self.impl_assignment(impl_var_id) {
                    let mut long_impl_id = impl_id.lookup_intern(self.db);
                    if let RewriteResult::Modified = self.internal_rewrite(&mut long_impl_id)? {
                        *self.impl_assignment.get_mut(&impl_var_id).unwrap() =
                            long_impl_id.clone().intern(self.db);
                    }
                    *value = long_impl_id;
                    return Ok(RewriteResult::Modified);
                }
            }
            ImplLongId::ImplImpl(impl_impl_id) => {
                let impl_impl_id_rewrite_result = self.internal_rewrite(impl_impl_id)?;
                let impl_id = impl_impl_id.impl_id();
                return Ok(match impl_id.lookup_intern(self.db) {
                    ImplLongId::GenericParameter(_)
                    | ImplLongId::SelfImpl(_)
                    | ImplLongId::GeneratedImpl(_)
                    | ImplLongId::ImplImpl(_) => impl_impl_id_rewrite_result,
                    ImplLongId::Concrete(_) => {
                        if let Ok(imp) = self.db.impl_impl_concrete_implized(*impl_impl_id) {
                            *value = self.rewrite(imp).no_err().lookup_intern(self.db);
                            RewriteResult::Modified
                        } else {
                            impl_impl_id_rewrite_result
                        }
                    }
                    ImplLongId::ImplVar(var) => {
                        if let Ok(concrete_trait_impl) =
                            impl_impl_id.concrete_trait_impl_id(self.db)
                        {
                            *value = self
                                .rewritten_impl_impl(var, concrete_trait_impl)
                                .lookup_intern(self.db);
                            return Ok(RewriteResult::Modified);
                        } else {
                            impl_impl_id_rewrite_result
                        }
                    }
                });
            }

            _ => {}
        }
        if value.is_var_free(self.db) {
            return Ok(RewriteResult::NoChange);
        }
        value.default_rewrite(self)
    }
}

struct InferenceIdReplacer<'a> {
    db: &'a dyn SemanticGroup,
    from_inference_id: InferenceId<'a>,
    to_inference_id: InferenceId<'a>,
}
impl<'a> InferenceIdReplacer<'a> {
    fn new(
        db: &'a dyn SemanticGroup,
        from_inference_id: InferenceId<'a>,
        to_inference_id: InferenceId<'a>,
    ) -> Self {
        Self { db, from_inference_id, to_inference_id }
    }
}
impl<'a> HasDb<&'a dyn SemanticGroup> for InferenceIdReplacer<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, InferenceIdReplacer<'a>, NoError, @exclude InferenceId);
add_expr_rewrites!(<'a>, InferenceIdReplacer<'a>, NoError, @exclude);
add_rewrite!(<'a>, InferenceIdReplacer<'a>, NoError, Ambiguity<'a>);
impl<'a> SemanticRewriter<InferenceId<'a>, NoError> for InferenceIdReplacer<'a> {
    fn internal_rewrite(&mut self, value: &mut InferenceId<'a>) -> Result<RewriteResult, NoError> {
        if value == &self.from_inference_id {
            *value = self.to_inference_id;
            Ok(RewriteResult::Modified)
        } else {
            Ok(RewriteResult::NoChange)
        }
    }
}
