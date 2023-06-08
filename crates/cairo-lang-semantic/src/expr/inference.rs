//! Bidirectional type inference.

use std::collections::{HashMap, VecDeque};
use std::ops::{Deref, DerefMut};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LocalVarId, MemberId, ParamId,
    StructId, TraitFunctionId, TraitId, VarId, VariantId,
};
use cairo_lang_diagnostics::{skip_diagnostic, DiagnosticAdded, Maybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::Itertools;

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
use crate::items::imp::{
    find_possible_impls_at_context, ImplId, ImplLookupContext, UninferredImpl,
};
use crate::items::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::literals::LiteralId;
use crate::substitution::{GenericSubstitution, HasDb, SemanticRewriter, SubstitutionRewriter};
use crate::types::{ConcreteEnumLongId, ConcreteExternTypeLongId, ConcreteStructLongId};
use crate::{
    add_basic_rewrites, add_expr_rewrites, ConcreteEnumId, ConcreteExternTypeId, ConcreteFunction,
    ConcreteImplId, ConcreteImplLongId, ConcreteStructId, ConcreteTraitId, ConcreteTraitLongId,
    ConcreteTypeId, ConcreteVariant, ExprLiteral, FunctionId, FunctionLongId, GenericArgumentId,
    GenericParam, LocalVariable, Member, Parameter, Pattern, SemanticObject, Signature, TypeId,
    TypeLongId,
};

pub mod conform;

/// A type variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
    pub stable_ptr: SyntaxStablePtrId,
}

/// An impl variable, created when a generic type argument is not passed, and thus is not known
/// yet and needs to be inferred.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplVar {
    pub id: usize,
    pub concrete_trait_id: ConcreteTraitId,
    pub stable_ptr: SyntaxStablePtrId,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum InferenceVar {
    Type(usize),
    Impl(usize),
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
    NoImplsFound { concrete_trait_id: ConcreteTraitId },
    MultipleImplsFound { concrete_trait_id: ConcreteTraitId, impls: Vec<UninferredImpl> },
    TypeNotInferred { ty: TypeId },
    WillNotInfer { concrete_trait_id: ConcreteTraitId },
    AlreadyReported,
}
impl InferenceError {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            InferenceError::Failed(_) => "Inference error occurred".into(),
            InferenceError::AlreadyReported => "Inference error occurred again".into(),
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
            InferenceError::WillNotInfer { concrete_trait_id } => format!(
                "Cannot infer trait {:?}. First generic argument must be known.",
                concrete_trait_id.debug(db)
            ),
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
            InferenceError::AlreadyReported => skip_diagnostic(),
            _ => diagnostics.report_by_ptr(
                stable_ptr,
                SemanticDiagnosticKind::InternalInferenceError(self.clone()),
            ),
        }
    }
}

#[derive(Clone)]
pub enum SolutionSet<T> {
    None,
    Unique(T),
    // TODO: Add info.
    Ambiguous,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ImplVarData {
    lookup_context: ImplLookupContext,
    candidates: Option<OrderedHashSet<UninferredImpl>>,
}

/// State of inference.
#[derive(Clone, Debug, DebugWithDb, Default, PartialEq, Eq)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct InferenceData {
    /// Current inferred assignment for type variables.
    pub type_assignment: HashMap<usize, TypeId>,
    /// Current inferred assignment for impl variables.
    pub impl_assignment: HashMap<usize, ImplId>,
    /// Type variables.
    pub type_vars: Vec<TypeVar>,
    /// Impl variables.
    pub impl_vars: Vec<ImplVar>,
    // TODOL: Remove.
    /// Inference state for impl variables.
    impl_var_data: Vec<ImplVarData>,

    pending: VecDeque<ImplVar>,
    refuted: Vec<ImplVar>,
    solved: Vec<ImplVar>,
    ambiguous: Vec<ImplVar>,
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

    /// Allocated a new [TypeVar] for an unknown type that needs to be inferred,
    pub fn new_type_var(&mut self, stable_ptr: SyntaxStablePtrId) -> TypeId {
        let var = TypeVar { id: self.type_vars.len(), stable_ptr };
        self.type_vars.push(var);
        self.db.intern_type(TypeLongId::Var(var))
    }

    /// Allocated a new [ImplVar] for an unknown type that needs to be inferred,
    pub fn new_impl_var(
        &mut self,
        concrete_trait_id: ConcreteTraitId,
        stable_ptr: SyntaxStablePtrId,
        lookup_context: ImplLookupContext,
    ) -> InferenceResult<ImplId> {
        let mut lookup_context = lookup_context;
        lookup_context
            .extra_modules
            .push(concrete_trait_id.trait_id(self.db).module_file_id(self.db.upcast()).0);

        self.impl_var_data.push(ImplVarData { lookup_context, candidates: None });

        let var = ImplVar { id: self.impl_vars.len(), concrete_trait_id, stable_ptr };
        self.impl_vars.push(var);
        self.pending.push_back(var);
        self.relax_impl_var(var).ok();
        Ok(ImplId::ImplVar(var))
    }

    pub fn solve(&mut self) {
        let mut ambiguous = std::mem::take(&mut self.ambiguous);
        self.pending.extend(ambiguous.drain(..));
        if self.pending.is_empty() {
            return;
        }
        // eprintln!("Solving");
        let mut next_pending = VecDeque::new();
        while let Some(mut impl_var) = self.pending.pop_front() {
            // eprintln!("While");
            impl_var = self.rewrite(impl_var).unwrap_or(impl_var);
            self.relax_impl_var(impl_var).ok();
            let Some(candidates) = &self.impl_var_data[impl_var.id].candidates else {
                next_pending.push_back(impl_var);
                continue;
            };
            if candidates.len() > 1 {
                self.ambiguous.push(impl_var);
                continue;
            }
            if candidates.is_empty() {
                self.refuted.push(impl_var);
                continue;
            }
            // Solution found. Assign it.
            let candidate = candidates.iter().next().unwrap();
            let lookup_context = self.impl_var_data[impl_var.id].lookup_context.clone();
            let var_concrete_trait_id = impl_var.concrete_trait_id;
            let impl_id = self
                .infer_impl(*candidate, var_concrete_trait_id, &lookup_context, impl_var.stable_ptr)
                .unwrap();
            let impl_id = self.rewrite(impl_id).unwrap_or(impl_id);
            self.assign_impl(impl_var, impl_id).ok();

            // Something changed.
            // Move next_pending into pending.
            self.solved.push(impl_var);
            self.pending.append(&mut next_pending);
            let mut ambiguous = std::mem::take(&mut self.ambiguous);
            self.pending.extend(ambiguous.drain(..));
        }
        self.pending = next_pending;
        // eprintln!("Done. Status:");
        // eprintln!(
        //     "  Solved: {:?}",
        //     self.impl_assignment.values().collect::<Vec<_>>().debug(self.db.elongate())
        // );
        // eprintln!("  Refuted: {:?}", self.refuted.debug(self.db.elongate()));
        // eprintln!("  Ambiguous: {:?}", self.ambiguous.debug(self.db.elongate()));
        // eprintln!(
        //     "  Pending: {:?}",
        //     self.pending.iter().cloned().collect::<Vec<_>>().debug(self.db.elongate())
        // );
    }

    pub fn solution(&self) -> SolutionSet<()> {
        if !self.refuted.is_empty() {
            return SolutionSet::None;
        }
        if !self.ambiguous.is_empty() {
            return SolutionSet::Ambiguous;
        }
        assert!(self.pending.is_empty(), "solution() called on an unsolved solver");
        SolutionSet::Unique(())
    }

    pub fn finalize(&mut self) -> Option<(SyntaxStablePtrId, InferenceError)> {
        let numeric_trait_id = get_core_trait(self.db, "NumericLiteral".into());
        let felt_ty = core_felt252_ty(self.db);
        // eprintln!("Finalizing");
        loop {
            let mut changed = false;
            self.solve();
            for var in self.impl_vars.clone().into_iter() {
                if self.impl_assignment.contains_key(&var.id) {
                    continue;
                }
                if var.concrete_trait_id.trait_id(self.db) != numeric_trait_id {
                    continue;
                }
                // Uninferred numeric trait. Resolve as felt252.
                let ty = extract_matches!(
                    var.concrete_trait_id.generic_args(self.db)[0],
                    GenericArgumentId::Type
                );
                if self.rewrite(ty) == Ok(felt_ty) {
                    continue;
                }
                if let Err(err) = self.conform_ty(ty, felt_ty) {
                    return Some((var.stable_ptr, err));
                }
                changed = true;
                break;
            }
            if !changed {
                break;
            }
        }
        // eprintln!("Done finalizing.");
        self.first_undetermined_variable()
    }

    /// Retrieves the first variable that is still not inferred, or None, if everything is
    /// inferred.
    pub fn first_undetermined_variable(&mut self) -> Option<(SyntaxStablePtrId, InferenceError)> {
        for (id, var) in self.type_vars.iter().enumerate() {
            if !self.type_assignment.contains_key(&id) {
                let ty = self.db.intern_type(TypeLongId::Var(*var));
                return Some((var.stable_ptr, InferenceError::TypeNotInferred { ty }));
            }
        }
        if let Some(id) = self.refuted.first() {
            let var = self.impl_vars[id.id];
            let concrete_trait_id = var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).unwrap_or(concrete_trait_id);
            return Some((var.stable_ptr, InferenceError::NoImplsFound { concrete_trait_id }));
        }
        if let Some(id) = self.ambiguous.first() {
            let id = *id;
            let var = self.impl_vars[id.id];
            let concrete_trait_id = var.concrete_trait_id;
            let concrete_trait_id = self.rewrite(concrete_trait_id).unwrap_or(concrete_trait_id);
            // TODO: Populate candidates.
            let impls = self.impl_var_data[id.id].candidates.clone().unwrap();
            let impls =
                impls.into_iter().map(|impl_id| self.rewrite(impl_id).unwrap_or(impl_id)).collect();
            return Some((
                var.stable_ptr,
                InferenceError::MultipleImplsFound { concrete_trait_id, impls },
            ));
        }
        None
    }

    /// Assigns a value to an [ImplVar]. Return the assigned impl, or an error.
    /// Assumes the variable is not already assigned.
    fn assign_impl(&mut self, var: ImplVar, impl_id: ImplId) -> InferenceResult<ImplId> {
        if let Some(other_impl) = self.impl_assignment.get(&var.id) {
            return self.conform_impl(impl_id, *other_impl);
        }
        assert!(!self.impl_assignment.contains_key(&var.id), "Cannot reassign variable.");
        if self.impl_contains_var(&impl_id, InferenceVar::Impl(var.id))? {
            return Err(InferenceError::Cycle { var: InferenceVar::Impl(var.id) });
        }
        self.impl_assignment.insert(var.id, impl_id);
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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

    /// Check if it possible to infer an impl to provide a concrete trait. See infer_impl.
    pub fn can_infer_impl(
        &self,
        uninferred_impl: UninferredImpl,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<bool> {
        // eprintln!(
        //     "can_infer_impl {:?} {:?}",
        //     uninferred_impl.debug(self.db.elongate()),
        //     concrete_trait_id.debug(self.db.elongate())
        // );
        let mut inference_data = self.clone_data();
        let mut inference = inference_data.inference(self.db);
        match inference.infer_impl(uninferred_impl, concrete_trait_id, lookup_context, stable_ptr) {
            Err(InferenceError::Failed(diag_added)) => return Err(diag_added),
            Ok(_) => {}
            Err(_) => return Ok(false),
        }
        inference.solve();
        Ok(inference.finalize().is_none())
    }

    /// Infers all the variables required to make an impl (possibly with free generic params)
    /// provide a concrete trait.
    pub fn infer_impl_def(
        &mut self,
        impl_def_id: ImplDefId,
        concrete_trait_id: ConcreteTraitId,
        lookup_context: &ImplLookupContext,
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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
        stable_ptr: SyntaxStablePtrId,
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

    // TODO: Remove these two functions.
    /// Resumes inference for an impl var.
    pub fn try_to_resume_impl_var(&mut self, var: ImplVar) -> InferenceResult<()> {
        if self.impl_var_data[var.id].candidates.is_some() {
            return Ok(());
        }
        let mut lookup_context = self.impl_var_data[var.id].lookup_context.clone();

        let concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        let generic_args = concrete_trait_id.generic_args(self.db);
        // Don't try to resolve impls if the first generic param is a variable.
        match generic_args.get(0) {
            Some(GenericArgumentId::Type(ty)) => {
                if let TypeLongId::Var(_) = self.db.lookup_intern_type(*ty) {
                    // Don't try to infer such impls.
                    return Ok(());
                }
            }
            Some(GenericArgumentId::Impl(ImplId::ImplVar(_))) => {
                // Don't try to infer such impls.
                return Ok(());
            }
            _ => {}
        };
        // Add the defining module of the generic params to the lookup.
        for generic_arg in &generic_args {
            if let GenericArgumentId::Type(ty) = generic_arg {
                if let TypeLongId::Concrete(concrete) = self.db.lookup_intern_type(*ty) {
                    lookup_context
                        .extra_modules
                        .push(concrete.generic_type(self.db).module_file_id(self.db.upcast()).0);
                }
            }
        }
        let candidates = find_possible_impls_at_context(
            self.db,
            self,
            &lookup_context,
            concrete_trait_id,
            var.stable_ptr,
        )
        .map_err(InferenceError::Failed)?;
        // eprintln!("candidates: {:?}", candidates.debug(self.db.elongate()));
        self.impl_var_data[var.id].candidates = Some(candidates.clone());
        log::trace!(
            "Impl inference candidates for {:?} at {:?}: {:?}",
            concrete_trait_id.debug(self.db.elongate()),
            lookup_context.debug(self.db.elongate()),
            candidates.iter().collect_vec().debug(self.db.elongate()),
        );
        if candidates.is_empty() {
            return Err(InferenceError::NoImplsFound { concrete_trait_id });
        }
        Ok(())
    }

    /// Relaxes the information about an [ImplVar]. Prunes the current candidate impls, and assigns
    /// if only a single candidate is left.
    fn relax_impl_var(&mut self, var: ImplVar) -> InferenceResult<()> {
        // TODO(spapini): Beware of cycles.
        if self.impl_assignment.get(&var.id).is_some() {
            return Ok(());
        }
        let var_concrete_trait_id = self.rewrite(var.concrete_trait_id)?;
        self.try_to_resume_impl_var(var)?;
        let mut inference_data = self.clone_data();
        let inference = inference_data.inference(self.db);
        let lookup_context = self.impl_var_data[var.id].lookup_context.clone();
        // let db = self.db;
        let Some(candidates) = &mut self.impl_var_data[var.id].candidates else {
            return Ok(())
        };
        if candidates.is_empty() {
            return Err(InferenceError::AlreadyReported);
        }
        for candidate in candidates.clone() {
            let can_infer = inference.can_infer_impl(
                candidate,
                var_concrete_trait_id,
                &lookup_context,
                var.stable_ptr,
            )?;
            if !can_infer {
                // eprintln!("removing candidate {:?}", candidate.debug(db.elongate()));
                candidates.swap_remove(&candidate);
            } else {
                // eprintln!("keeping candidate {:?}", candidate.debug(db.elongate()));
            }
        }
        Ok(())
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
            if let Some(impl_id) = self.impl_assignment.get(&var.id) {
                return self.rewrite(*impl_id);
            }
        }
        value.default_rewrite(self)
    }
}
