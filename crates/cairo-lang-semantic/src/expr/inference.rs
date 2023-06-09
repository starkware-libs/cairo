//! Bidirectional type inference.

use std::collections::{HashMap, VecDeque};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LocalVarId, MemberId, ParamId,
    StructId, TraitFunctionId, TraitId, VarId, VariantId,
};
use cairo_lang_diagnostics::DiagnosticAdded;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{define_short_id, extract_matches};
use itertools::Itertools;

use self::canonic::CanonicalTrait;
use self::solver::{enrich_lookup_context, SolutionSet};
use crate::corelib::{core_felt252_ty, get_core_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::expr::inference::conform::InferenceConform;
use crate::expr::objects::*;
use crate::expr::pattern::*;
use crate::items::constant::Constant;
use crate::items::functions::{
    ConcreteFunctionWithBody, ConcreteFunctionWithBodyId, GenericFunctionId,
    GenericFunctionWithBodyId, ImplGenericFunctionId, ImplGenericFunctionWithBodyId,
};
use crate::items::generics::{GenericParamConst, GenericParamImpl, GenericParamType};
use crate::items::imp::{ImplId, ImplLookupContext, UninferredImpl};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::literals::LiteralId;
use crate::substitution::{GenericSubstitution, HasDb, SemanticRewriter, SubstitutionRewriter};
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId};
use crate::{
    add_basic_rewrites, add_expr_rewrites, semantic_object_for_id, ConcreteEnumId,
    ConcreteExternTypeId, ConcreteFunction, ConcreteImplId, ConcreteImplLongId, ConcreteStructId,
    ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, ConcreteVariant, ExprLiteral, FunctionId,
    FunctionLongId, GenericArgumentId, GenericParam, LocalVariable, Member, Parameter, Pattern,
    SemanticObject, Signature, TypeId, TypeLongId,
};

pub mod canonic;
pub mod conform;
pub mod solver;

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: LocalTypeVarId,
}

/// An impl variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplVar {
    #[dont_rewrite]
    pub id: LocalImplVarId,
    pub concrete_trait_id: ConcreteTraitId,
    #[dont_rewrite]
    pub lookup_context: ImplLookupContext,
}
impl ImplVar {
    pub fn intern(&self, db: &dyn SemanticGroup) -> ImplVarId {
        db.intern_impl_var(self.clone())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject)]
pub struct LocalTypeVarId(pub usize);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, SemanticObject)]
pub struct LocalImplVarId(pub usize);

define_short_id!(ImplVarId, ImplVar, SemanticGroup, lookup_intern_impl_var);
impl ImplVarId {
    pub fn get(&self, db: &dyn SemanticGroup) -> ImplVar {
        db.lookup_intern_impl_var(*self)
    }
    pub fn id(&self, db: &dyn SemanticGroup) -> LocalImplVarId {
        self.get(db).id
    }
    pub fn concrete_trait_id(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        self.get(db).concrete_trait_id
    }
    pub fn lookup_context(&self, db: &dyn SemanticGroup) -> ImplLookupContext {
        self.get(db).lookup_context
    }
}
semantic_object_for_id!(ImplVarId, lookup_intern_impl_var, intern_impl_var, ImplVar);

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceVar {
    Type(LocalTypeVarId),
    Impl(LocalImplVarId),
}

// TODO(spapini): Add to diagnostics.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceError {
    Failed(DiagnosticAdded),
    Cycle { var: InferenceVar },
    TypeKindMismatch { ty0: TypeId, ty1: TypeId },
    ImplKindMismatch { impl0: ImplId, impl1: ImplId },
    GenericArgMismatch { garg0: GenericArgumentId, garg1: GenericArgumentId },
    TraitMismatch { trt0: TraitId, trt1: TraitId },
    ConstInferenceNotSupported,

    // TODO: These are only used for external interface.
    NoImplsFound { concrete_trait_id: ConcreteTraitId },
    MultipleImplsFound { concrete_trait_id: ConcreteTraitId, impls: Vec<UninferredImpl> },
    TypeNotInferred { ty: TypeId },
}
impl InferenceError {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            InferenceError::Failed(_) => "Inference error occurred".into(),
            InferenceError::Cycle { var: _ } => "Inference cycle detected".into(),
            InferenceError::TypeKindMismatch { ty0, ty1 } => {
                format!("Type mismatch: {:?} and {:?}", ty0.debug(db), ty1.debug(db))
            }
            InferenceError::ImplKindMismatch { impl0, impl1 } => {
                format!("Impl mismatch: {:?} and {:?}", impl0.debug(db), impl1.debug(db))
            }
            InferenceError::GenericArgMismatch { garg0, garg1 } => {
                format!("Generic arg mismatch: {:?} and {:?}", garg0.debug(db), garg1.debug(db))
            }
            InferenceError::TraitMismatch { trt0, trt1 } => {
                format!("Trait mismatch: {:?} and {:?}", trt0.debug(db), trt1.debug(db))
            }
            InferenceError::ConstInferenceNotSupported => {
                "Const generic inference not yet supported.".into()
            }
            InferenceError::NoImplsFound { concrete_trait_id } => {
                format!("Trait has no implementation in context: {:?}", concrete_trait_id.debug(db))
            }
            InferenceError::MultipleImplsFound { concrete_trait_id, impls } => {
                let impls_str =
                    impls.iter().map(|imp| format!("{:?}", imp.debug(db.upcast()))).join(", ");
                format!(
                    "Trait `{:?}` has multiple implementations, in: {impls_str}",
                    concrete_trait_id.debug(db)
                )
            }
            InferenceError::TypeNotInferred { ty } => {
                format!("Type annotations needed. Failed to infer {:?}", ty.debug(db))
            }
        }
    }
}

pub type InferenceResult<T> = Result<T, InferenceError>;

impl From<DiagnosticAdded> for InferenceError {
    fn from(value: DiagnosticAdded) -> Self {
        InferenceError::Failed(value)
    }
}
impl InferenceError {
    pub fn report(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
    ) -> DiagnosticAdded {
        match self {
            InferenceError::Failed(diagnostic_added) => *diagnostic_added,
            // TODO(spapini): Better save the DiagnosticAdded on the variable.
            // InferenceError::AlreadyReported => skip_diagnostic(),
            _ => diagnostics.report_by_ptr(
                stable_ptr,
                SemanticDiagnosticKind::InternalInferenceError(self.clone()),
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ImplVarData {
    candidates: Option<OrderedHashSet<UninferredImpl>>,
}

/// State of inference.
#[derive(Clone, Debug, DebugWithDb, Default, PartialEq, Eq)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceData {
    /// Current inferred assignment for type variables.
    pub type_assignment: HashMap<LocalTypeVarId, TypeId>,
    /// Current inferred assignment for impl variables.
    pub impl_assignment: HashMap<LocalImplVarId, ImplId>,
    /// Type variables.
    pub type_vars: Vec<TypeVar>,
    /// Impl variables.
    pub impl_vars: Vec<ImplVar>,
    pub stable_ptrs: HashMap<InferenceVar, SyntaxStablePtrId>,

    pending: VecDeque<LocalImplVarId>,
    refuted: Vec<LocalImplVarId>,
    solved: Vec<LocalImplVarId>,
    ambiguous: Vec<LocalImplVarId>,
}
impl InferenceData {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn inference<'db, 'b: 'db>(&'db mut self, db: &'b dyn SemanticGroup) -> Inference<'db> {
        Inference { db, data: self }
    }
}

/// State of inference.
pub struct Inference<'db> {
    db: &'db dyn SemanticGroup,
    pub data: &'db mut InferenceData,
}

impl Deref for Inference<'_> {
    type Target = InferenceData;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}
impl DerefMut for Inference<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<'db> std::fmt::Debug for Inference<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = self.data.debug(self.db.elongate());
        write!(f, "{x:?}")
    }
}

impl<'db> Inference<'db> {
    /// Creates a new [Inference] instance with the given [InferenceData].
    pub fn with_data(db: &'db dyn SemanticGroup, data: &'db mut InferenceData) -> Self {
        Self { db, data }
    }

    pub fn clone_data(&self) -> InferenceData {
        self.data.clone()
    }

    fn impl_var(&self, var_id: LocalImplVarId) -> &ImplVar {
        &self.impl_vars[var_id.0]
    }
    pub fn impl_assignment(&self, var_id: LocalImplVarId) -> Option<ImplId> {
        self.impl_assignment.get(&var_id).copied()
    }
    fn type_assignment(&self, var_id: LocalTypeVarId) -> Option<TypeId> {
        self.type_assignment.get(&var_id).copied()
    }

    /// Allocated a new [TypeVar] for an unknown type that needs to be inferred,
    pub fn new_type_var(&mut self, stable_ptr: Option<SyntaxStablePtrId>) -> TypeId {
        let var = self.new_type_var_raw(stable_ptr);
        self.db.intern_type(TypeLongId::Var(var))
    }

    pub fn new_type_var_raw(&mut self, stable_ptr: Option<SyntaxStablePtrId>) -> TypeVar {
        let var = TypeVar { id: LocalTypeVarId(self.type_vars.len()) };
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Type(var.id), stable_ptr);
        }
        self.type_vars.push(var);
        var
    }

    /// Allocated a new [ImplVar] for an unknown type that needs to be inferred,
    pub fn new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: Option<SyntaxStablePtrId>,
        mut lookup_context: ImplLookupContext,
    ) -> InferenceResult<ImplId> {
        enrich_lookup_context(self.db, concrete_trait_id, &mut lookup_context);
        let var = self.new_impl_var_raw(lookup_context, concrete_trait_id, stable_ptr);
        Ok(ImplId::ImplVar(self.impl_var(var).intern(self.db)))
    }

    fn new_impl_var_raw(
        &mut self,
        lookup_context: ImplLookupContext,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> LocalImplVarId {
        let mut lookup_context = lookup_context;
        lookup_context
            .insert_module(concrete_trait_id.trait_id(self.db).module_file_id(self.db.upcast()).0);

        let id = LocalImplVarId(self.impl_vars.len());
        if let Some(stable_ptr) = stable_ptr {
            self.stable_ptrs.insert(InferenceVar::Impl(id), stable_ptr);
        }
        let var = ImplVar { id, concrete_trait_id, lookup_context };
        self.impl_vars.push(var);
        self.pending.push_back(id);
        id
    }

    pub fn solve(&mut self) -> InferenceResult<()> {
        let mut ambiguous = std::mem::take(&mut self.ambiguous);
        self.pending.extend(ambiguous.drain(..));
        while let Some(var) = self.pending.pop_front() {
            let solution = match self.impl_var_solution_set(var)? {
                SolutionSet::None => {
                    self.refuted.push(var);
                    continue;
                }
                SolutionSet::Ambiguous => {
                    self.ambiguous.push(var);
                    continue;
                }
                SolutionSet::Unique(solution) => solution,
            };

            // Solution found. Assign it.
            self.assign_impl(var, solution).unwrap();

            // Something changed.
            self.solved.push(var);
            let mut ambiguous = std::mem::take(&mut self.ambiguous);
            self.pending.extend(ambiguous.drain(..));
        }
        Ok(())
    }

    pub fn solution_set(&mut self) -> InferenceResult<SolutionSet<()>> {
        self.solve()?;
        if !self.refuted.is_empty() {
            return Ok(SolutionSet::None);
        }
        if !self.ambiguous.is_empty() {
            return Ok(SolutionSet::Ambiguous);
        }
        assert!(self.pending.is_empty(), "solution() called on an unsolved solver");
        Ok(SolutionSet::Unique(()))
    }

    pub fn finalize(&mut self) -> Option<(Option<SyntaxStablePtrId>, InferenceError)> {
        let numeric_trait_id = get_core_trait(self.db, "NumericLiteral".into());
        let felt_ty = core_felt252_ty(self.db);

        // Conform all uninferred numeric literals to felt252.
        loop {
            let mut changed = false;
            if let Err(err) = self.solve() {
                return Some((None, err));
            }
            for var in self.ambiguous.clone() {
                let impl_var = self.impl_var(var).clone();
                if impl_var.concrete_trait_id.trait_id(self.db) != numeric_trait_id {
                    continue;
                }
                // Uninferred numeric trait. Resolve as felt252.
                let ty = extract_matches!(
                    impl_var.concrete_trait_id.generic_args(self.db)[0],
                    GenericArgumentId::Type
                );
                if self.rewrite(ty) == Ok(felt_ty) {
                    continue;
                }
                if let Err(err) = self.conform_ty(ty, felt_ty) {
                    return Some((
                        self.stable_ptrs.get(&InferenceVar::Impl(impl_var.id)).copied(),
                        err,
                    ));
                }
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
        let (var, err) = self.first_undetermined_variable()?;
        Some((self.stable_ptrs.get(&var).copied(), err))
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn first_undetermined_variable(&mut self) -> Option<(InferenceVar, InferenceError)> {
        for (id, var) in self.type_vars.iter().enumerate() {
            if self.type_assignment(LocalTypeVarId(id)).is_none() {
                let ty = self.db.intern_type(TypeLongId::Var(*var));
                return Some((
                    InferenceVar::Type(LocalTypeVarId(id)),
                    InferenceError::TypeNotInferred { ty },
                ));
            }
        }
        if let Some(var) = self.refuted.first().copied() {
            let impl_var = self.impl_var(var).clone();
            let concrete_trait_id = impl_var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).unwrap_or(concrete_trait_id);
            return Some((
                InferenceVar::Impl(var),
                InferenceError::NoImplsFound { concrete_trait_id },
            ));
        }
        if let Some(var) = self.ambiguous.first().copied() {
            let impl_var = self.impl_var(var).clone();
            let concrete_trait_id = impl_var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).unwrap_or(concrete_trait_id);
            // TODO: Populate candidates.
            let impls = vec![];
            return Some((
                InferenceVar::Impl(var),
                InferenceError::MultipleImplsFound { concrete_trait_id, impls },
            ));
        }
        None
    }

    /// Assigns a value to an [ImplVar]. Return the assigned impl, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_impl(&mut self, var: LocalImplVarId, impl_id: ImplId) -> InferenceResult<ImplId> {
        self.conform_traits(
            self.impl_var(var).concrete_trait_id,
            impl_id.concrete_trait(self.db)?,
        )?;
        if let Some(other_impl) = self.impl_assignment(var) {
            return self.conform_impl(impl_id, other_impl);
        }
        if self.impl_contains_var(&impl_id, InferenceVar::Impl(var))? {
            return Err(InferenceError::Cycle { var: InferenceVar::Impl(var) });
        }
        self.impl_assignment.insert(var, impl_id);
        Ok(impl_id)
    }

    /// Assigns a value to a [TypeVar]. Return the assigned type, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_ty(&mut self, var: TypeVar, ty: TypeId) -> InferenceResult<TypeId> {
        assert!(!self.type_assignment.contains_key(&var.id), "Cannot reassign variable.");
        let inference_var = InferenceVar::Type(var.id);
        if self.ty_contains_var(ty, inference_var)? {
            return Err(InferenceError::Cycle { var: inference_var });
        }
        self.type_assignment.insert(var.id, ty);
        Ok(ty)
    }

    /// Determines if an assignment to `generic_params` can be chosen s.t. `generic_args` will be
    /// substituted to `expected_generic_args`.
    // TODO(spapini): Fail gracefully on infinite loops.
    pub fn can_infer_generics(
        &self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> bool {
        if generic_args.len() != expected_generic_args.len() {
            return false;
        }
        let mut inference_data = self.clone_data();
        let mut inference = inference_data.inference(self.db);
        let res = inference.infer_generic_assignment(
            generic_params,
            generic_args,
            expected_generic_args,
            lookup_context,
            stable_ptr,
        );
        res.is_ok()
    }

    /// Infers all the variables required to make an uninferred impl provide a concrete trait.
    fn infer_impl(
        &mut self,
        uninferred_impl: UninferredImpl,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<ImplId> {
        let impl_id = match uninferred_impl {
            UninferredImpl::Def(impl_def_id) => {
                self.infer_impl_def(impl_def_id, concrete_trait_id, lookup_context, stable_ptr)?
            }
            UninferredImpl::ImplAlias(impl_alias_id) => {
                self.infer_impl_alias(impl_alias_id, concrete_trait_id, lookup_context, stable_ptr)?
            }
            UninferredImpl::GenericParam(param_id) => {
                let param =
                    self.db.generic_param_semantic(param_id).map_err(InferenceError::Failed)?;
                let param = extract_matches!(param, GenericParam::Impl);
                let imp_concrete_trait_id = param.concrete_trait.unwrap();
                self.conform_traits(concrete_trait_id, imp_concrete_trait_id)?;
                ImplId::GenericParameter(param_id)
            }
        };
        Ok(impl_id)
    }

    /// Infers all the variables required to make an impl (possibly with free generic params)
    /// provide a concrete trait.
    pub fn infer_impl_def(
        &mut self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> Result<ImplId, InferenceError> {
        let imp_generic_params = self.db.impl_def_generic_params(impl_def_id)?;
        let imp_concrete_trait = self.db.impl_def_concrete_trait(impl_def_id)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generic_assignment(
            &imp_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;
        Ok(ImplId::Concrete(
            self.db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args }),
        ))
    }

    /// Infers all the variables required to make an impl alias (possibly with free generic params)
    /// provide a concrete trait.
    pub fn infer_impl_alias(
        &mut self,
        impl_alias_id: ImplAliasId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> Result<ImplId, InferenceError> {
        let impl_alias_generic_params = self.db.impl_alias_generic_params(impl_alias_id)?;
        let impl_id = self.db.impl_alias_resolved_impl(impl_alias_id)?;
        let imp_concrete_trait = impl_id.concrete_trait(self.db)?;
        if imp_concrete_trait.trait_id(self.db) != concrete_trait_id.trait_id(self.db) {
            return Err(InferenceError::TraitMismatch {
                trt0: imp_concrete_trait.trait_id(self.db),
                trt1: concrete_trait_id.trait_id(self.db),
            });
        }

        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(imp_concrete_trait);
        let generic_args = self.infer_generic_assignment(
            &impl_alias_generic_params,
            &long_imp_concrete_trait.generic_args,
            &long_concrete_trait.generic_args,
            lookup_context,
            stable_ptr,
        )?;

        Ok(SubstitutionRewriter {
            db: self.db,
            substitution: &GenericSubstitution::new(&impl_alias_generic_params, &generic_args),
        }
        .rewrite(impl_id)?)
    }

    /// Chooses and assignment to generic_params s.t. generic_args will be substituted to
    /// expected_generic_args.
    /// Returns the generic_params assignment.
    pub fn infer_generic_assignment(
        &mut self,
        generic_params: &[GenericParam],
        generic_args: &[GenericArgumentId],
        expected_generic_args: &[GenericArgumentId],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        let new_generic_args =
            self.infer_generic_args(generic_params, lookup_context, stable_ptr)?;
        let substitution = GenericSubstitution::new(generic_params, &new_generic_args);
        let mut rewriter = SubstitutionRewriter { db: self.db, substitution: &substitution };
        let generic_args = rewriter.rewrite(generic_args.iter().copied().collect_vec())?;
        self.conform_generic_args(&generic_args, expected_generic_args)?;
        self.rewrite(new_generic_args)
    }

    /// Infers all generic_arguments given the parameters.
    pub fn infer_generic_args(
        &mut self,
        generic_params: &[GenericParam],
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<Vec<GenericArgumentId>> {
        let mut generic_args = vec![];
        let mut substitution = GenericSubstitution::default();
        for generic_param in generic_params {
            let generic_param = SubstitutionRewriter { db: self.db, substitution: &substitution }
                .rewrite(*generic_param)
                .map_err(InferenceError::Failed)?;
            let generic_arg =
                self.infer_generic_arg(&generic_param, lookup_context.clone(), stable_ptr)?;
            generic_args.push(generic_arg);
            substitution.0.insert(generic_param.id(), generic_arg);
        }
        Ok(generic_args)
    }

    /// Tries to infer a trait function as a method for `self_ty`.
    /// Supports snapshot snapshot coercions.
    ///
    /// Returns the deduced type and the number of snapshots that need to be added to it.
    pub fn infer_concrete_trait_by_self(
        &mut self,
        trait_function: TraitFunctionId,
        self_ty: TypeId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> Option<(ConcreteTraitId, usize)> {
        let trait_id = trait_function.trait_id(self.db.upcast());
        let signature = self.db.trait_function_signature(trait_function).ok()?;
        let first_param = signature.params.into_iter().next()?;
        if first_param.name != "self" {
            return None;
        }
        let generic_params = self.db.trait_generic_params(trait_id).ok()?;
        let generic_args =
            self.infer_generic_args(&generic_params, lookup_context, stable_ptr).ok()?;
        let substitution = GenericSubstitution::new(&generic_params, &generic_args);
        let mut rewriter = SubstitutionRewriter { db: self.db, substitution: &substitution };

        let fixed_param_ty = rewriter.rewrite(first_param.ty).ok()?;
        let (_, n_snapshots) = self.conform_ty_ex(self_ty, fixed_param_ty, true).ok()?;
        let generic_args = self.rewrite(generic_args).ok()?;

        Some((
            self.db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }),
            n_snapshots,
        ))
    }

    /// Infers a generic argument to be passed as a generic paramter.
    /// Allocates a new inference variable of the correct kind, and wraps in a generic argument.
    pub fn infer_generic_arg(
        &mut self,
        param: &GenericParam,
        lookup_context: ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<GenericArgumentId> {
        match param {
            GenericParam::Type(_) => Ok(GenericArgumentId::Type(self.new_type_var(stable_ptr))),
            GenericParam::Impl(param) => Ok(GenericArgumentId::Impl(self.new_impl_var(
                param.concrete_trait?,
                stable_ptr,
                lookup_context,
            )?)),
            GenericParam::Const(_) => Err(InferenceError::ConstInferenceNotSupported),
        }
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function,
    /// and the generic arguments to be passed to the function.
    /// Returns the resulting impl function.
    pub fn infer_trait_function(
        &mut self,
        concrete_trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<FunctionId> {
        let generic_function =
            self.infer_trait_generic_function(concrete_trait_function, lookup_context, stable_ptr)?;
        self.infer_generic_function(generic_function, lookup_context, stable_ptr)
    }

    /// Infers generic arguments to be passed to a generic function.
    /// Returns the resulting specialized function.
    pub fn infer_generic_function(
        &mut self,
        generic_function: GenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<FunctionId> {
        let generic_params = generic_function.generic_params(self.db)?;
        let generic_args = self.infer_generic_args(&generic_params, lookup_context, stable_ptr)?;
        Ok(self.db.intern_function(FunctionLongId {
            function: ConcreteFunction { generic_function, generic_args },
        }))
    }

    /// Infers the impl to be substituted instead of a trait for a given trait function.
    /// Returns the resulting impl generic function.
    pub fn infer_trait_generic_function(
        &mut self,
        trait_function: ConcreteTraitGenericFunctionId,
        lookup_context: &ImplLookupContext,
        stable_ptr: Option<SyntaxStablePtrId>,
    ) -> InferenceResult<GenericFunctionId> {
        let impl_id = self.new_impl_var(
            trait_function.concrete_trait_id(self.db),
            stable_ptr,
            lookup_context.clone(),
        )?;
        Ok(GenericFunctionId::Impl(ImplGenericFunctionId {
            impl_id,
            function: trait_function.function_id(self.db),
        }))
    }

    fn impl_var_solution_set(
        &mut self,
        var: LocalImplVarId,
    ) -> InferenceResult<SolutionSet<ImplId>> {
        let impl_var = self.impl_var(var).clone();
        // Update the concrete trait of the impl var.
        let concrete_trait_id = self.rewrite(impl_var.concrete_trait_id)?;
        let mut lookup_context = impl_var.lookup_context;
        enrich_lookup_context(self.db, concrete_trait_id, &mut lookup_context);
        self.impl_vars[impl_var.id.0].concrete_trait_id = concrete_trait_id;
        self.impl_vars[impl_var.id.0].lookup_context = lookup_context.clone();

        // Don't try to resolve impls if the first generic param is a variable.
        let generic_args = concrete_trait_id.generic_args(self.db);
        match generic_args.get(0) {
            Some(GenericArgumentId::Type(ty)) => {
                if let TypeLongId::Var(_) = self.db.lookup_intern_type(*ty) {
                    // Don't try to infer such impls.
                    return Ok(SolutionSet::Ambiguous);
                }
            }
            Some(GenericArgumentId::Impl(ImplId::ImplVar(_))) => {
                // Don't try to infer such impls.
                return Ok(SolutionSet::Ambiguous);
            }
            _ => {}
        };

        let (canonical_trait, canonicalizer) =
            CanonicalTrait::canonicalize(self.db, concrete_trait_id);
        let solution_set = self.db.canonic_trait_solutions(canonical_trait, lookup_context)?;
        Ok(match solution_set {
            SolutionSet::None => SolutionSet::None,
            SolutionSet::Unique(canonical_impl) => {
                SolutionSet::Unique(canonical_impl.embed(self, &canonicalizer))
            }
            SolutionSet::Ambiguous => SolutionSet::Ambiguous,
        })
    }
}

impl<'a> HasDb<&'a dyn SemanticGroup> for Inference<'a> {
    fn get_db(&self) -> &'a dyn SemanticGroup {
        self.db
    }
}
add_basic_rewrites!(<'a>, Inference<'a>, InferenceError, @exclude TypeLongId ImplId);
add_expr_rewrites!(<'a>, Inference<'a>, InferenceError, @exclude);
impl<'a> SemanticRewriter<TypeLongId, InferenceError> for Inference<'a> {
    fn rewrite(&mut self, value: TypeLongId) -> Result<TypeLongId, InferenceError> {
        if let TypeLongId::Var(var) = value {
            if let Some(type_id) = self.type_assignment.get(&var.id) {
                return self.rewrite(self.db.lookup_intern_type(*type_id));
            }
        }
        value.default_rewrite(self)
    }
}
impl<'a> SemanticRewriter<ImplId, InferenceError> for Inference<'a> {
    fn rewrite(&mut self, value: ImplId) -> InferenceResult<ImplId> {
        if let ImplId::ImplVar(var) = value {
            // Relax the candidates.
            if let Some(impl_id) = self.impl_assignment(var.get(self.db).id) {
                return self.rewrite(impl_id);
            }
        }
        value.default_rewrite(self)
    }
}
