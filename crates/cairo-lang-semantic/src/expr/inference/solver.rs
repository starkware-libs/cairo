use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_proc_macros::SemanticObject;
use itertools::Itertools;

use super::canonic::{CanonicalImpl, CanonicalMapping, CanonicalTrait, MapperError, ResultNoErrEx};
use super::{InferenceData, InferenceError, InferenceResult, InferenceVar, LocalImplVarId};
use crate::db::SemanticGroup;
use crate::items::imp::{find_candidates_at_context, ImplId, ImplLookupContext, UninferredImpl};
use crate::substitution::SemanticRewriter;
use crate::{ConcreteTraitId, GenericArgumentId, TypeLongId};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SolutionSet<T> {
    None,
    Unique(T),
    Ambiguous(Ambiguity),
}

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
}
impl Ambiguity {
    pub fn format(&self, db: &(dyn SemanticGroup + 'static)) -> String {
        match self {
            Ambiguity::MultipleImplsFound { concrete_trait_id, impls } => {
                let impls_str =
                    impls.iter().map(|imp| format!("{:?}", imp.debug(db.upcast()))).join(", ");
                format!(
                    "Trait `{:?}` has multiple implementations, in: {impls_str}",
                    concrete_trait_id.debug(db)
                )
            }
            Ambiguity::FreeVariable { impl_id, var: _ } => {
                format!("Candidate impl {:?} has a free variable", impl_id.debug(db),)
            }
            Ambiguity::WillNotInfer { concrete_trait_id } => {
                format!(
                    "Cannot infer trait {:?}. First generic argument must be known.",
                    concrete_trait_id.debug(db)
                )
            }
        }
    }
}

// Assumes the lookup context is already enriched
pub fn canonic_trait_solutions(
    db: &dyn SemanticGroup,
    canonical_trait: CanonicalTrait,
    lookup_context: ImplLookupContext,
) -> InferenceResult<SolutionSet<CanonicalImpl>> {
    let mut solver = Solver::new(db, canonical_trait, lookup_context);
    solver.solution_set(db)
}

pub fn canonic_trait_solutions_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &[String],
    _canonical_trait: &CanonicalTrait,
    _lookup_context: &ImplLookupContext,
) -> InferenceResult<SolutionSet<CanonicalImpl>> {
    Err(InferenceError::Cycle { var: InferenceVar::Impl(LocalImplVarId(0)) })
}

pub fn enrich_lookup_context(
    db: &dyn SemanticGroup,
    concrete_trait_id: ConcreteTraitId,
    lookup_context: &mut ImplLookupContext,
) {
    let generic_args = concrete_trait_id.generic_args(db);
    // Add the defining module of the generic params to the lookup.
    for generic_arg in &generic_args {
        if let GenericArgumentId::Type(ty) = generic_arg {
            if let TypeLongId::Concrete(concrete) = db.lookup_intern_type(*ty) {
                lookup_context
                    .insert_module(concrete.generic_type(db).module_file_id(db.upcast()).0);
            }
        }
    }
}

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

    pub fn solution_set(
        &mut self,
        db: &dyn SemanticGroup,
    ) -> InferenceResult<SolutionSet<CanonicalImpl>> {
        let mut unique_solution: Option<CanonicalImpl> = None;
        for candidate_solver in &mut self.candidate_solvers {
            let candidate_solution_set = candidate_solver.solution_set(db)?;
            let candidate_solution = match candidate_solution_set {
                SolutionSet::None => continue,
                SolutionSet::Unique(candidate_solution) => candidate_solution,
                SolutionSet::Ambiguous(ambiguity) => return Ok(SolutionSet::Ambiguous(ambiguity)),
            };
            if let Some(unique_solution) = unique_solution {
                return Ok(SolutionSet::Ambiguous(Ambiguity::MultipleImplsFound {
                    concrete_trait_id: self.canonical_trait.0,
                    impls: vec![unique_solution.0, candidate_solution.0],
                }));
            }
            unique_solution = Some(candidate_solution);
        }
        Ok(unique_solution.map(SolutionSet::Unique).unwrap_or(SolutionSet::None))
    }
}

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
        let mut inference_data = InferenceData::new();
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
            SolutionSet::Unique(_) => {
                let candidate_impl = inference.rewrite(self.candidate_impl).no_err();
                let canonical_impl =
                    CanonicalImpl::canonicalize(db, candidate_impl, &self.canonical_embedding);
                match canonical_impl {
                    Ok(canonical_impl) => SolutionSet::Unique(canonical_impl),
                    Err(MapperError(var)) => {
                        // Free variable.
                        SolutionSet::Ambiguous(Ambiguity::FreeVariable {
                            impl_id: self.candidate_impl,
                            var,
                        })
                    }
                }
            }
            SolutionSet::Ambiguous(ambiguity) => SolutionSet::Ambiguous(ambiguity),
        })
    }
}

// Current issue: Assigning values to params. Params are unassignable.
