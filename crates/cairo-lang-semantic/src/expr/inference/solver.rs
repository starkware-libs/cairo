use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_proc_macros::SemanticObject;
use itertools::Itertools;

use super::canonic::{CanonicalImpl, CanonicalMapping, CanonicalTrait, MapperError, ResultNoErrEx};
use super::infers::InferenceEmbeddings;
use super::{
    InferenceData, InferenceError, InferenceId, InferenceResult, InferenceVar, LocalImplVarId,
};
use crate::db::SemanticGroup;
use crate::items::imp::{find_candidates_at_context, ImplId, ImplLookupContext, UninferredImpl};
use crate::substitution::SemanticRewriter;
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
    WillNotInfer {
        concrete_trait_id: ConcreteTraitId,
    },
    NegativeImplWithUnresolvedGenericArgs {
        impl_id: ImplId,
        ty: TypeId,
    },
}
impl Ambiguity {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            Ambiguity::MultipleImplsFound { concrete_trait_id, impls } => {
                let impls_str =
                    impls.iter().map(|imp| format!("`{}`", imp.format(db.upcast()))).join(", ");
                format!(
                    "Trait `{:?}` has multiple implementations, in: {impls_str}",
                    concrete_trait_id.debug(db)
                )
            }
            Ambiguity::FreeVariable { impl_id, var: _ } => {
                format!("Candidate impl {:?} has an unused generic parameter.", impl_id.debug(db),)
            }
            Ambiguity::WillNotInfer { concrete_trait_id } => {
                format!(
                    "Cannot infer trait {:?}. First generic argument must be known.",
                    concrete_trait_id.debug(db)
                )
            }
            Ambiguity::NegativeImplWithUnresolvedGenericArgs { impl_id, ty } => format!(
                "Cannot infer negative impl in `{}` as it contains the unresolved type `{}`",
                { impl_id }.format(db),
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
    lookup_context: ImplLookupContext,
) -> Result<SolutionSet<CanonicalImpl>, InferenceError> {
    let mut solver = Solver::new(db, canonical_trait, lookup_context);
    Ok(solver.solution_set(db))
}

/// Cycle handling for [canonic_trait_solutions].
pub fn canonic_trait_solutions_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &[String],
    _canonical_trait: &CanonicalTrait,
    _lookup_context: &ImplLookupContext,
) -> Result<SolutionSet<CanonicalImpl>, InferenceError> {
    Err(InferenceError::Cycle { var: InferenceVar::Impl(LocalImplVarId(0)) })
}

/// Adds the defining module of the trait and the generic arguments to the lookup context.
pub fn enrich_lookup_context(
    db: &dyn SemanticGroup,
    concrete_trait_id: ConcreteTraitId,
    lookup_context: &mut ImplLookupContext,
) {
    lookup_context.insert_module(concrete_trait_id.trait_id(db).module_file_id(db.upcast()).0);
    let generic_args = concrete_trait_id.generic_args(db);
    // Add the defining module of the generic params to the lookup.
    for generic_arg in &generic_args {
        if let GenericArgumentId::Type(ty) = generic_arg {
            match db.lookup_intern_type(*ty) {
                TypeLongId::Concrete(concrete) => {
                    lookup_context
                        .insert_module(concrete.generic_type(db).module_file_id(db.upcast()).0);
                }
                TypeLongId::Coupon(function_id) => {
                    if let Some(module_file_id) =
                        function_id.get_concrete(db).generic_function.module_file_id(db)
                    {
                        lookup_context.insert_module(module_file_id.0);
                    }
                }
                _ => (),
            }
        }
    }
}

/// A canonical trait solver.
#[derive(Debug)]
pub struct Solver {
    pub canonical_trait: CanonicalTrait,
    pub lookup_context: ImplLookupContext,
    candidate_solvers: Vec<CandidateSolver>,
}
impl Solver {
    fn new(
        db: &dyn SemanticGroup,
        canonical_trait: CanonicalTrait,
        lookup_context: ImplLookupContext,
    ) -> Self {
        let filter = canonical_trait.0.filter(db);
        let candidates =
            find_candidates_at_context(db, &lookup_context, filter).unwrap_or_default();
        let candidate_solvers = candidates
            .into_iter()
            .filter_map(|candidate| {
                CandidateSolver::new(db, canonical_trait, candidate, &lookup_context).ok()
            })
            .collect();

        Self { canonical_trait, lookup_context, candidate_solvers }
    }

    pub fn solution_set(&mut self, db: &dyn SemanticGroup) -> SolutionSet<CanonicalImpl> {
        let mut unique_solution: Option<CanonicalImpl> = None;
        for candidate_solver in &mut self.candidate_solvers {
            let Ok(candidate_solution_set) = candidate_solver.solution_set(db) else {
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
                        concrete_trait_id: self.canonical_trait.0,
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
#[derive(Debug)]
pub struct CandidateSolver {
    pub candidate: UninferredImpl,
    inference_data: InferenceData,
    canonical_embedding: CanonicalMapping,
    candidate_impl: ImplId,
    pub lookup_context: ImplLookupContext,
}
impl CandidateSolver {
    fn new(
        db: &dyn SemanticGroup,
        canonical_trait: CanonicalTrait,
        candidate: UninferredImpl,
        lookup_context: &ImplLookupContext,
    ) -> InferenceResult<CandidateSolver> {
        let mut inference_data = InferenceData::new(InferenceId::Canonical);
        let mut inference = inference_data.inference(db);
        let (concrete_trait_id, canonical_embedding) = canonical_trait.embed(&mut inference);
        // Add the defining module of the candidate to the lookup.
        let mut lookup_context = lookup_context.clone();
        lookup_context.insert_module(candidate.module_id(db.upcast()));
        // Instantiate the candidate in the inference table.
        let candidate_impl =
            inference.infer_impl(candidate, concrete_trait_id, &lookup_context, None)?;

        Ok(CandidateSolver {
            candidate,
            inference_data,
            canonical_embedding,
            candidate_impl,
            lookup_context,
        })
    }
    fn solution_set(
        &mut self,
        db: &dyn SemanticGroup,
    ) -> InferenceResult<SolutionSet<CanonicalImpl>> {
        let mut inference = self.inference_data.inference(db);
        let solution_set = inference.solution_set()?;
        Ok(match solution_set {
            SolutionSet::None => SolutionSet::None,
            SolutionSet::Ambiguous(ambiguity) => SolutionSet::Ambiguous(ambiguity),
            SolutionSet::Unique(_) => {
                let candidate_impl = inference.rewrite(self.candidate_impl).no_err();
                match CanonicalImpl::canonicalize(db, candidate_impl, &self.canonical_embedding) {
                    Ok(canonical_impl) => {
                        inference.validate_neg_impls(&self.lookup_context, canonical_impl)?
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
