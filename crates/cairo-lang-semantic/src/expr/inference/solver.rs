use std::collections::BTreeMap;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_utils::LookupIntern;
use itertools::Itertools;

use super::canonic::{CanonicalImpl, CanonicalTrait, MapperError, ResultNoErrEx};
use super::conform::InferenceConform;
use super::infers::InferenceEmbeddings;
use super::{
    ImplVarTraitItemMappings, InferenceData, InferenceError, InferenceId, InferenceResult,
    InferenceVar, LocalImplVarId,
};
use crate::db::SemanticGroup;
use crate::items::constant::ImplConstantId;
use crate::items::imp::{
    ImplId, ImplImplId, ImplLookupContext, ImplLookupContextId, UninferredImpl,
    find_closure_generated_candidate,
};
use crate::substitution::SemanticRewriter;
use crate::types::{ImplTypeById, ImplTypeId};
use crate::{ConcreteTraitId, GenericArgumentId, TypeId, TypeLongId};

/// A generic solution set for an inference constraint system.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SolutionSet<T> {
    None,
    Unique(T),
    Ambiguous(Ambiguity),
}

/// Describes the kinds of inference ambiguities.
#[derive(Clone, Debug, Eq, Hash, PartialEq, SemanticObject)]
pub enum Ambiguity {
    MultipleImplsFound {
        concrete_trait_id: ConcreteTraitId,
        impls: Vec<ImplId>,
    },
    FreeVariable {
        impl_id: ImplId,
        #[dont_rewrite]
        var: InferenceVar,
    },
    WillNotInfer(ConcreteTraitId),
    NegativeImplWithUnresolvedGenericArgs {
        impl_id: ImplId,
        ty: TypeId,
    },
}
impl Ambiguity {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            Ambiguity::MultipleImplsFound { concrete_trait_id, impls } => {
                let impls_str = impls.iter().map(|imp| format!("`{}`", imp.format(db))).join(", ");
                format!(
                    "Trait `{:?}` has multiple implementations, in: {impls_str}",
                    concrete_trait_id.debug(db)
                )
            }
            Ambiguity::FreeVariable { impl_id, var: _ } => {
                format!("Candidate impl {:?} has an unused generic parameter.", impl_id.debug(db),)
            }
            Ambiguity::WillNotInfer(concrete_trait_id) => {
                format!(
                    "Cannot infer trait {:?}. First generic argument must be known.",
                    concrete_trait_id.debug(db)
                )
            }
            Ambiguity::NegativeImplWithUnresolvedGenericArgs { impl_id, ty } => format!(
                "Cannot infer negative impl in `{}` as it contains the unresolved type `{}`",
                impl_id.format(db),
                ty.format(db)
            ),
        }
    }
}

/// Query implementation of [SemanticGroup::canonic_trait_solutions].
/// Assumes the lookup context is already enriched by [enrich_lookup_context].
pub fn canonic_trait_solutions(
    db: &dyn SemanticGroup,
    canonical_trait: CanonicalTrait,
    lookup_context: ImplLookupContextId,
    impl_type_bounds: BTreeMap<ImplTypeById, TypeId>,
) -> Result<SolutionSet<CanonicalImpl>, InferenceError> {
    let mut concrete_trait_id = canonical_trait.id;
    let impl_type_bounds = Arc::new(impl_type_bounds);
    // If the trait is not fully concrete, we might be able to use the trait's items to find a
    // more concrete trait.
    if !concrete_trait_id.is_fully_concrete(db) {
        let mut solver = Solver::new(db, canonical_trait, lookup_context, impl_type_bounds.clone());
        match solver.solution_set(db) {
            SolutionSet::None => {}
            SolutionSet::Unique(imp) => {
                concrete_trait_id =
                    imp.0.concrete_trait(db).expect("A solved impl must have a concrete trait");
            }
            SolutionSet::Ambiguous(ambiguity) => {
                return Ok(SolutionSet::Ambiguous(ambiguity));
            }
        }
    }
    // Solve the trait without the trait items, so we'd be able to find conflicting impls.
    let mut solver = Solver::new(
        db,
        CanonicalTrait { id: concrete_trait_id, mappings: ImplVarTraitItemMappings::default() },
        lookup_context,
        impl_type_bounds,
    );

    Ok(solver.solution_set(db))
}

/// Cycle handling for [canonic_trait_solutions].
pub fn canonic_trait_solutions_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    _canonical_trait: &CanonicalTrait,
    _lookup_context: &ImplLookupContextId,
    _impl_type_bounds: &BTreeMap<ImplTypeById, TypeId>,
) -> Result<SolutionSet<CanonicalImpl>, InferenceError> {
    Err(InferenceError::Cycle(InferenceVar::Impl(LocalImplVarId(0))))
}

/// Adds the defining module of the trait and the generic arguments to the lookup context.
pub fn enrich_lookup_context(
    db: &dyn SemanticGroup,
    concrete_trait_id: ConcreteTraitId,
    lookup_context: &mut ImplLookupContext,
) {
    lookup_context.insert_module(concrete_trait_id.trait_id(db).module_file_id(db.upcast()).0, db);
    let generic_args = concrete_trait_id.generic_args(db);
    // Add the defining module of the generic args to the lookup.
    for generic_arg in &generic_args {
        if let GenericArgumentId::Type(ty) = generic_arg {
            enrich_lookup_context_with_ty(db, *ty, lookup_context);
        }
    }
}

/// Adds the defining module of the type to the lookup context.
pub fn enrich_lookup_context_with_ty(
    db: &dyn SemanticGroup,
    ty: TypeId,
    lookup_context: &mut ImplLookupContext,
) {
    match ty.lookup_intern(db) {
        TypeLongId::ImplType(impl_type_id) => {
            lookup_context.insert_impl(impl_type_id.impl_id(), db);
        }
        long_ty => {
            if let Some(module_id) = long_ty.module_id(db) {
                lookup_context.insert_module(module_id, db);
            }
        }
    }
}

/// A canonical trait solver.
#[derive(Debug)]
pub struct Solver {
    pub canonical_trait: CanonicalTrait,
    candidate_solvers: Vec<CandidateSolver>,
    impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
}
impl Solver {
    fn new(
        db: &dyn SemanticGroup,
        canonical_trait: CanonicalTrait,
        lookup_context: ImplLookupContextId,
        impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
    ) -> Self {
        let filter = canonical_trait.id.filter(db);
        let mut candidates =
            db.find_candidates_at_context(lookup_context, filter).unwrap_or_default();
        find_closure_generated_candidate(db, canonical_trait.id)
            .map(|candidate| candidates.insert(candidate));
        let candidate_solvers = candidates
            .into_iter()
            .map(|candidate| CandidateSolver::new(candidate, lookup_context))
            .collect();

        Self { canonical_trait, candidate_solvers, impl_type_bounds }
    }

    pub fn solution_set(&mut self, db: &dyn SemanticGroup) -> SolutionSet<CanonicalImpl> {
        let mut unique_solution: Option<CanonicalImpl> = None;
        for candidate_solver in &mut self.candidate_solvers {
            let Ok(candidate_solution_set) = db.candidate_solution_set(
                candidate_solver.clone(),
                self.canonical_trait.clone(),
                self.impl_type_bounds.clone(),
            ) else {
                continue;
            };

            let candidate_solution = match candidate_solution_set {
                SolutionSet::None => continue,
                SolutionSet::Unique(candidate_solution) => candidate_solution,
                SolutionSet::Ambiguous(ambiguity) => return SolutionSet::Ambiguous(ambiguity),
            };
            if let Some(unique_solution) = unique_solution {
                // There might be multiple unique solutions from different candidates that are
                // solved to the same impl id (e.g. finding it near the trait, and
                // through an impl alias). This is valid.
                if unique_solution.0 != candidate_solution.0 {
                    return SolutionSet::Ambiguous(Ambiguity::MultipleImplsFound {
                        concrete_trait_id: self.canonical_trait.id,
                        impls: vec![unique_solution.0, candidate_solution.0],
                    });
                }
            }
            unique_solution = Some(candidate_solution);
        }
        unique_solution.map(SolutionSet::Unique).unwrap_or(SolutionSet::None)
    }
}

/// A solver for a candidate to a canonical trait.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct CandidateSolver {
    pub candidate: UninferredImpl,
    pub lookup_context: ImplLookupContextId,
}
impl CandidateSolver {
    fn new(candidate: UninferredImpl, lookup_context: ImplLookupContextId) -> CandidateSolver {
        CandidateSolver {
            candidate,
            lookup_context,
        }
    }
    fn solution_set(
        &mut self,
        db: &dyn SemanticGroup,
        canonical_trait: CanonicalTrait,
        impl_type_bounds: BTreeMap<ImplTypeById, TypeId>,
    ) -> InferenceResult<SolutionSet<CanonicalImpl>> {
        let mut inference_data: InferenceData = InferenceData::new(InferenceId::Canonical);
        let mut inference = inference_data.inference(db);
        inference.data.impl_type_bounds = Arc::new(impl_type_bounds);
        let (canonical_trait, canonical_embedding) = canonical_trait.embed(&mut inference);

        // If the closure params are not var free, we cannot infer the negative impl.
        // We use the canonical trait concretize the closure params.
        if let UninferredImpl::GeneratedImpl(imp) = self.candidate {
            inference.conform_traits(imp.lookup_intern(db).concrete_trait, canonical_trait.id)?;
        }

        // Add the defining module of the candidate to the lookup.
        let lookup_context =
            db.impl_lookup_context_insert_lookup_scope(self.lookup_context, self.candidate);
        // Instantiate the candidate in the inference table.
        let candidate_impl =
            inference.infer_impl(self.candidate, canonical_trait.id, lookup_context, None)?;
        for (trait_type, ty) in canonical_trait.mappings.types.iter() {
            let mapped_ty =
                inference.reduce_impl_ty(ImplTypeId::new(candidate_impl, *trait_type, db))?;

            // Conform the candidate's type to the trait's type.
            inference.conform_ty(mapped_ty, *ty)?;
        }
        for (trait_const, const_id) in canonical_trait.mappings.constants.iter() {
            let mapped_const_id = inference.reduce_impl_constant(ImplConstantId::new(
                candidate_impl,
                *trait_const,
                db,
            ))?;
            // Conform the candidate's constant to the trait's constant.
            inference.conform_const(mapped_const_id, *const_id)?;
        }

        for (trait_impl, impl_id) in canonical_trait.mappings.impls.iter() {
            let mapped_impl_id =
                inference.reduce_impl_impl(ImplImplId::new(candidate_impl, *trait_impl, db))?;
            // Conform the candidate's impl to the trait's impl.
            inference.conform_impl(mapped_impl_id, *impl_id)?;
        }

        let mut inference = inference_data.inference(db);
        let solution_set = inference.solution_set()?;
        Ok(match solution_set {
            SolutionSet::None => SolutionSet::None,
            SolutionSet::Ambiguous(ambiguity) => SolutionSet::Ambiguous(ambiguity),
            SolutionSet::Unique(_) => {
                let candidate_impl = inference.rewrite(candidate_impl).no_err();
                match CanonicalImpl::canonicalize(db, candidate_impl, &canonical_embedding) {
                    Ok(canonical_impl) => {
                        inference.validate_neg_impls(self.lookup_context, canonical_impl)?
                    }
                    Err(MapperError(var)) => {
                        return Ok(SolutionSet::Ambiguous(Ambiguity::FreeVariable {
                            impl_id: candidate_impl,
                            var,
                        }));
                    }
                }
            }
        })
    }
}

pub fn candidate_solution_set(
    db: &dyn SemanticGroup,
    mut candidate: CandidateSolver,
    canonical_trait: CanonicalTrait,
    impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
) -> InferenceResult<SolutionSet<CanonicalImpl>> {
    candidate.solution_set(db, canonical_trait, (*impl_type_bounds).clone())
}
