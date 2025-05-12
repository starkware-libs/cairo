use std::collections::BTreeMap;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_utils::LookupIntern;
use itertools::{Itertools, zip_eq};

use super::canonic::{CanonicalImpl, CanonicalMapping, CanonicalTrait, MapperError, ResultNoErrEx};
use super::conform::InferenceConform;
use super::infers::InferenceEmbeddings;
use super::{
    ImplVarTraitItemMappings, InferenceData, InferenceError, InferenceId, InferenceResult,
    InferenceVar, LocalImplVarId,
};
use crate::db::SemanticGroup;
use crate::items::constant::{ConstValueId, ImplConstantId};
use crate::items::imp::{
    ImplId, ImplImplId, ImplLongId, ImplLookupContext, UninferredImpl, find_candidates_at_context,
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
    lookup_context: ImplLookupContext,
    impl_type_bounds: BTreeMap<ImplTypeById, TypeId>,
) -> Result<SolutionSet<CanonicalImpl>, InferenceError> {
    let mut concrete_trait_id = canonical_trait.id;
    let impl_type_bounds = Arc::new(impl_type_bounds);
    // If the trait is not fully concrete, we might be able to use the trait's items to find a
    // more concrete trait.
    if !concrete_trait_id.is_fully_concrete(db) {
        let mut solver =
            Solver::new(db, canonical_trait, lookup_context.clone(), impl_type_bounds.clone());
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
    _lookup_context: &ImplLookupContext,
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
    lookup_context.insert_module(concrete_trait_id.trait_id(db).module_file_id(db).0);
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
            lookup_context.insert_impl(impl_type_id.impl_id());
        }
        long_ty => {
            if let Some(module_id) = long_ty.module_id(db) {
                lookup_context.insert_module(module_id);
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
        impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
    ) -> Self {
        let filter = canonical_trait.id.filter(db);
        let mut candidates =
            find_candidates_at_context(db, &lookup_context, &filter).unwrap_or_default();
        find_closure_generated_candidate(db, canonical_trait.id)
            .map(|candidate| candidates.insert(candidate));
        let candidate_solvers = candidates
            .into_iter()
            .filter_map(|candidate| {
                CandidateSolver::new(
                    db,
                    &canonical_trait,
                    candidate,
                    &lookup_context,
                    impl_type_bounds.clone(),
                )
                .ok()
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
        canonical_trait: &CanonicalTrait,
        candidate: UninferredImpl,
        lookup_context: &ImplLookupContext,
        impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
    ) -> InferenceResult<CandidateSolver> {
        if !can_conform_generic_args(
            &candidate.concrete_trait(db).unwrap().generic_args(db),
            &canonical_trait.id.generic_args(db),
            &impl_type_bounds,
            db,
        ) {
            return Err(super::ErrorSet);
        }
        if let UninferredImpl::GenericParam(_) = candidate {
            for (trait_args, candidate_args) in canonical_trait
                .id
                .generic_args(db)
                .iter()
                .zip_eq(candidate.concrete_trait(db).unwrap().generic_args(db))
            {
                if let (GenericArgumentId::Type(ty0), GenericArgumentId::Type(ty1)) =
                    (trait_args, candidate_args)
                {
                    if !ty0.is_var_free(db) || !ty1.is_var_free(db) {
                        continue;
                    }

                    if *ty0 != ty1 {
                        return Err(super::ErrorSet);
                    }
                }
            }
        }

        let mut inference_data: InferenceData = InferenceData::new(InferenceId::Canonical);
        let mut inference = inference_data.inference(db);
        inference.data.impl_type_bounds = impl_type_bounds;
        let (canonical_trait, canonical_embedding) = canonical_trait.embed(&mut inference);

        // If the closure params are not var free, we cannot infer the negative impl.
        // We use the canonical trait concretize the closure params.
        if let UninferredImpl::GeneratedImpl(imp) = candidate {
            inference.conform_traits(imp.lookup_intern(db).concrete_trait, canonical_trait.id)?;
        }

        // Add the defining module of the candidate to the lookup.
        let mut lookup_context = lookup_context.clone();
        lookup_context.insert_lookup_scope(db, &candidate);
        // Instantiate the candidate in the inference table.
        let candidate_impl =
            inference.infer_impl(candidate, canonical_trait.id, &lookup_context, None)?;
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


/// Checks if the generic arguments of the candidate could be conformed to the generic args of the trait.
fn can_conform_generic_args(
    gargs_candidate: &[GenericArgumentId],
    gargs1: &[GenericArgumentId],
    impl_type_bounds: &Arc<BTreeMap<ImplTypeById, TypeId>>,
    db: &dyn SemanticGroup,
) -> bool {
    zip_eq(gargs_candidate, gargs1).all(|(garg_candidate, garg1)| {
        can_conform_generic_arg(*garg_candidate, *garg1, impl_type_bounds, db)
    })
}

/// Checks if a [GenericArgumentId] of the candidate could be conformed to a [GenericArgumentId] of the trait.
fn can_conform_generic_arg(
    garg_candidate: GenericArgumentId,
    garg_canonical_trait: GenericArgumentId,
    impl_type_bounds: &Arc<BTreeMap<ImplTypeById, TypeId>>,
    db: &dyn SemanticGroup,
) -> bool {
    if garg_candidate == garg_canonical_trait {
        return true;
    }
    match garg_candidate {
        GenericArgumentId::Type(gty_candidate) => {
            let GenericArgumentId::Type(gty_trait) = garg_canonical_trait else {
                return false;
            };
            can_conform_ty(gty_candidate, gty_trait, impl_type_bounds, db)
        }
        GenericArgumentId::Constant(gc_candidate) => {
            let GenericArgumentId::Constant(gc_trait) = garg_canonical_trait else {
                return false;
            };

            can_conform_const(gc_candidate, gc_trait, impl_type_bounds, db)
        }
        GenericArgumentId::Impl(impl_candidate) => {
            let GenericArgumentId::Impl(impl_trait) = garg_canonical_trait else {
                return false;
            };
            can_conform_impl(impl_candidate, impl_trait, impl_type_bounds, db)
        }
        GenericArgumentId::NegImpl => {
            matches!(garg_canonical_trait, GenericArgumentId::NegImpl)
        }
    }
}

/// Checks if a generic arg [ImplId] of the candidate could be conformed to a generic arg [ImplId] of the trait.
fn can_conform_ty(
    ty_candidate: TypeId,
    ty_canonical_trait: TypeId,
    impl_type_bounds: &Arc<BTreeMap<ImplTypeById, TypeId>>,
    db: &dyn SemanticGroup,
) -> bool {
    if ty_candidate == ty_canonical_trait {
        return true;
    }
    let long_ty_trait = ty_canonical_trait.lookup_intern(db);

    if let TypeLongId::Var(_) = long_ty_trait {
        return true;
    }
    let long_ty_candidate = ty_candidate.lookup_intern(db);

    match long_ty_candidate {
        TypeLongId::Concrete(concrete_candidate) => {
            let TypeLongId::Concrete(concrete_canonical_trait) = long_ty_trait else {
                return false;
            };
            if concrete_candidate.generic_type(db) != concrete_canonical_trait.generic_type(db) {
                return false;
            }
            let gargs_candidate = concrete_candidate.generic_args(db);
            let gargs_trait = concrete_canonical_trait.generic_args(db);
            can_conform_generic_args(&gargs_candidate, &gargs_trait, impl_type_bounds, db)
        }
        TypeLongId::Tuple(tys_candidate) => {
            let TypeLongId::Tuple(tys_trait) = long_ty_trait else {
                return false;
            };
            if tys_candidate.len() != tys_trait.len() {
                return false;
            }
            zip_eq(tys_candidate, tys_trait).all(|(subty_candidate, subty_trait)| {
                can_conform_ty(subty_candidate, subty_trait, impl_type_bounds, db)
            })
        }
        TypeLongId::Closure(closure_candidate) => {
            let TypeLongId::Closure(closure_trait) = long_ty_trait else {
                return false;
            };
            if closure_candidate.wrapper_location != closure_trait.wrapper_location {
                return false;
            }
            if !zip_eq(closure_candidate.param_tys, closure_trait.param_tys).all(
                |(subty_candidate, subty_trait)| {
                    can_conform_ty(subty_candidate, subty_trait, impl_type_bounds, db)
                },
            ) {
                return false;
            }
            if !zip_eq(closure_candidate.captured_types, closure_trait.captured_types).all(
                |(subty_candidate, subty_trait)| {
                    can_conform_ty(subty_candidate, subty_trait, impl_type_bounds, db)
                },
            ) {
                return false;
            }
            can_conform_ty(closure_candidate.ret_ty, closure_trait.ret_ty, impl_type_bounds, db)
        }
        TypeLongId::FixedSizeArray { type_id, size } => {
            let TypeLongId::FixedSizeArray { type_id: type_id1, size: size1 } = long_ty_trait
            else {
                return false;
            };
            if !can_conform_const(size, size1, impl_type_bounds, db) {
                return false;
            }
            can_conform_ty(type_id, type_id1, impl_type_bounds, db)
        }
        TypeLongId::Snapshot(inner_ty_candidate) => {
            let TypeLongId::Snapshot(inner_ty1) = long_ty_trait else {
                return false;
            };
            can_conform_ty(inner_ty_candidate, inner_ty1, impl_type_bounds, db)
        }
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::ImplType(_)
        | TypeLongId::Missing(_)
        | TypeLongId::Coupon(_) => true,
    }
}

/// Checks if a generic arg [ImplId] of the candidate could be conformed to a generic arg [ImplId] of the trait.
fn can_conform_impl(
    impl_candidate: ImplId,
    impl_canonical_trait: ImplId,
    impl_type_bounds: &Arc<BTreeMap<ImplTypeById, TypeId>>,
    db: &dyn SemanticGroup,
) -> bool {
    let long_impl_trait = impl_canonical_trait.lookup_intern(db);
    if impl_candidate == impl_canonical_trait {
        return true;
    }
    if let ImplLongId::ImplVar(_) = long_impl_trait {
        return true;
    }
    match impl_candidate.lookup_intern(db) {
        ImplLongId::ImplVar(_) => true,
        ImplLongId::Concrete(concrete_candidate) => {
            let ImplLongId::Concrete(concrete_canonical_trait) = long_impl_trait else {
                return false;
            };
            let concrete_candidate = concrete_candidate.lookup_intern(db);
            let concrete_canonical_trait = concrete_canonical_trait.lookup_intern(db);
            if concrete_candidate.impl_def_id != concrete_canonical_trait.impl_def_id {
                return false;
            }
            let gargs_candidate = concrete_candidate.generic_args;
            let gargs_trait = concrete_canonical_trait.generic_args;
            can_conform_generic_args(&gargs_candidate, &gargs_trait, impl_type_bounds, db)
        }
        ImplLongId::GenericParameter(_)
        | ImplLongId::ImplImpl(_)
        | ImplLongId::SelfImpl(_)
        | ImplLongId::GeneratedImpl(_) => true,
    }
}

// TODO(Tomerstarkware) add consts to early candidate eliminiation
fn can_conform_const(
    _id_candidate: ConstValueId,
    _id_canonical_trait: ConstValueId,
    _impl_type_bounds: &Arc<BTreeMap<ImplTypeById, TypeId>>,
    _db: &dyn SemanticGroup,
) -> bool {
    true
}
