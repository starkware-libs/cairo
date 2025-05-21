use std::collections::BTreeMap;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_utils::{Intern, LookupIntern};
use itertools::{Itertools, zip_eq};

use super::canonic::{CanonicalImpl, CanonicalMapping, CanonicalTrait, MapperError, ResultNoErrEx};
use super::conform::InferenceConform;
use super::infers::InferenceEmbeddings;
use super::{
    ImplVarTraitItemMappings, InferenceData, InferenceError, InferenceId, InferenceResult,
    InferenceVar, LocalImplVarId,
};
use crate::db::SemanticGroup;
use crate::items::constant::{ConstValue, ConstValueId, ImplConstantId};
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
        let candidate_concrete_trait = candidate.concrete_trait(db).unwrap();
        // If the candidate is fully concrete, or its a generic which is var free, there is nothing
        // to substitute. A generic param may not be var free, if it contains impl types.
        let candidate_final = matches!(candidate, UninferredImpl::GenericParam(_))
            && candidate_concrete_trait.is_var_free(db)
            || candidate_concrete_trait.is_fully_concrete(db);
        let target_final = canonical_trait.id.is_var_free(db);
        if candidate_final && target_final {
            if candidate_concrete_trait != canonical_trait.id {
                return Err(super::ErrorSet);
            }
        } else if !can_conform_generic_args(
            db,
            (&candidate_concrete_trait.generic_args(db), candidate_final),
            (&canonical_trait.id.generic_args(db), target_final),
        ) {
            return Err(super::ErrorSet);
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

/// Checks if the generic arguments of the candidate could be conformed to the generic args of the
/// trait.
fn can_conform_generic_args(
    db: &dyn SemanticGroup,
    (candidate_args, candidate_final): (&[GenericArgumentId], bool),
    (target_args, target_final): (&[GenericArgumentId], bool),
) -> bool {
    zip_eq(candidate_args, target_args).all(|(candidate_arg, target_arg)| {
        can_conform_generic_arg(db, (*candidate_arg, candidate_final), (*target_arg, target_final))
    })
}

/// Checks if a [GenericArgumentId] of the candidate could be conformed to a [GenericArgumentId] of
/// the trait.
fn can_conform_generic_arg(
    db: &dyn SemanticGroup,
    (candidate_arg, mut candidate_final): (GenericArgumentId, bool),
    (target_arg, mut target_final): (GenericArgumentId, bool),
) -> bool {
    if candidate_arg == target_arg {
        return true;
    }
    candidate_final = candidate_final || candidate_arg.is_fully_concrete(db);
    target_final = target_final || target_arg.is_var_free(db);
    if candidate_final && target_final {
        return false;
    }
    match (candidate_arg, target_arg) {
        (GenericArgumentId::Type(candidate), GenericArgumentId::Type(target)) => {
            can_conform_ty(db, (candidate, candidate_final), (target, target_final))
        }
        (GenericArgumentId::Constant(candidate), GenericArgumentId::Constant(target)) => {
            can_conform_const(db, (candidate, candidate_final), (target, target_final))
        }
        (GenericArgumentId::Impl(candidate), GenericArgumentId::Impl(target)) => {
            can_conform_impl(db, (candidate, candidate_final), (target, target_final))
        }
        (GenericArgumentId::NegImpl, GenericArgumentId::NegImpl) => true,
        _ => false,
    }
}

/// Checks if a generic arg [TypeId] of the candidate could be conformed to a generic arg [TypeId]
/// of the trait.
fn can_conform_ty(
    db: &dyn SemanticGroup,
    (candidate_ty, mut candidate_final): (TypeId, bool),
    (target_ty, mut target_final): (TypeId, bool),
) -> bool {
    if candidate_ty == target_ty {
        return true;
    }
    candidate_final = candidate_final || candidate_ty.is_fully_concrete(db);
    target_final = target_final || target_ty.is_var_free(db);
    if candidate_final && target_final {
        return false;
    }
    let target_long_ty = target_ty.lookup_intern(db);

    if let TypeLongId::Var(_) = target_long_ty {
        return true;
    }

    let long_ty_candidate = candidate_ty.lookup_intern(db);

    match (long_ty_candidate, target_long_ty) {
        (TypeLongId::Concrete(candidate), TypeLongId::Concrete(target)) => {
            candidate.generic_type(db) == target.generic_type(db)
                && can_conform_generic_args(
                    db,
                    (&candidate.generic_args(db), candidate_final),
                    (&target.generic_args(db), target_final),
                )
        }
        (TypeLongId::Concrete(_), _) => false,
        (TypeLongId::Tuple(candidate_tys), TypeLongId::Tuple(target_tys)) => {
            candidate_tys.len() == target_tys.len()
                && zip_eq(candidate_tys, target_tys).all(|(candidate_subty, target_subty)| {
                    can_conform_ty(
                        db,
                        (candidate_subty, candidate_final),
                        (target_subty, target_final),
                    )
                })
        }
        (TypeLongId::Tuple(_), _) => false,
        (TypeLongId::Closure(candidate), TypeLongId::Closure(target)) => {
            if candidate.wrapper_location != target.wrapper_location {
                return false;
            }
            if !zip_eq(candidate.param_tys, target.param_tys).all(
                |(candidate_subty, target_subty)| {
                    can_conform_ty(
                        db,
                        (candidate_subty, candidate_final),
                        (target_subty, target_final),
                    )
                },
            ) {
                return false;
            }
            if !zip_eq(candidate.captured_types, target.captured_types).all(
                |(candidate_subty, target_subty)| {
                    can_conform_ty(
                        db,
                        (candidate_subty, candidate_final),
                        (target_subty, target_final),
                    )
                },
            ) {
                return false;
            }
            can_conform_ty(db, (candidate.ret_ty, candidate_final), (target.ret_ty, target_final))
        }
        (TypeLongId::Closure(_), _) => false,
        (
            TypeLongId::FixedSizeArray { type_id: candidate_type_id, size: candidate_size },
            TypeLongId::FixedSizeArray { type_id: target_type_id, size: target_size },
        ) => {
            can_conform_const(db, (candidate_size, candidate_final), (target_size, target_final))
                && can_conform_ty(
                    db,
                    (candidate_type_id, candidate_final),
                    (target_type_id, target_final),
                )
        }
        (TypeLongId::FixedSizeArray { type_id: _, size: _ }, _) => false,
        (TypeLongId::Snapshot(candidate_inner_ty), TypeLongId::Snapshot(target_inner_ty)) => {
            can_conform_ty(
                db,
                (candidate_inner_ty, candidate_final),
                (target_inner_ty, target_final),
            )
        }
        (TypeLongId::Snapshot(_), _) => false,
        (
            TypeLongId::GenericParameter(_)
            | TypeLongId::Var(_)
            | TypeLongId::ImplType(_)
            | TypeLongId::Missing(_)
            | TypeLongId::Coupon(_),
            _,
        ) => true,
    }
}

/// Checks if a generic arg [ImplId] of the candidate could be conformed to a generic arg [ImplId]
/// of the trait.
fn can_conform_impl(
    db: &dyn SemanticGroup,
    (candidate_impl, mut candidate_final): (ImplId, bool),
    (target_impl, mut target_final): (ImplId, bool),
) -> bool {
    let long_impl_trait = target_impl.lookup_intern(db);
    if candidate_impl == target_impl {
        return true;
    }
    candidate_final = candidate_final || candidate_impl.is_fully_concrete(db);
    target_final = target_final || target_impl.is_var_free(db);
    if candidate_final && target_final {
        return false;
    }
    if let ImplLongId::ImplVar(_) = long_impl_trait {
        return true;
    }
    match (candidate_impl.lookup_intern(db), long_impl_trait) {
        (ImplLongId::Concrete(candidate), ImplLongId::Concrete(target)) => {
            let candidate = candidate.lookup_intern(db);
            let target = target.lookup_intern(db);
            if candidate.impl_def_id != target.impl_def_id {
                return false;
            }
            let candidate_args = candidate.generic_args;
            let target_args = target.generic_args;
            can_conform_generic_args(
                db,
                (&candidate_args, candidate_final),
                (&target_args, target_final),
            )
        }
        (ImplLongId::Concrete(_), _) => false,
        (
            ImplLongId::GenericParameter(_)
            | ImplLongId::ImplVar(_)
            | ImplLongId::ImplImpl(_)
            | ImplLongId::SelfImpl(_)
            | ImplLongId::GeneratedImpl(_),
            _,
        ) => true,
    }
}

/// Checks if a generic arg [ConstValueId] of the candidate could be conformed to a generic arg
/// [ConstValueId] of the trait.
fn can_conform_const(
    db: &dyn SemanticGroup,
    (candidate_id, mut candidate_final): (ConstValueId, bool),
    (target_id, mut target_final): (ConstValueId, bool),
) -> bool {
    if candidate_id == target_id {
        return true;
    }
    candidate_final = candidate_final || candidate_id.is_fully_concrete(db);
    target_final = target_final || target_id.is_var_free(db);
    if candidate_final && target_final {
        return false;
    }
    let target_long_const = target_id.lookup_intern(db);
    if let ConstValue::Var(_, _) = target_long_const {
        return true;
    }
    match (candidate_id.lookup_intern(db), target_long_const) {
        (ConstValue::Int(big_int, type_id), ConstValue::Int(target_big_int, target_type_id)) => {
            if big_int != target_big_int {
                return false;
            }
            can_conform_ty(db, (type_id, candidate_final), (target_type_id, target_final))
        }
        (ConstValue::Int(_, _), _) => false,
        (
            ConstValue::Struct(const_values, type_id),
            ConstValue::Struct(target_const_values, target_type_id),
        ) => {
            if const_values.len() != target_const_values.len() {
                return false;
            }
            if !can_conform_ty(db, (type_id, candidate_final), (target_type_id, target_final)) {
                return false;
            }
            zip_eq(const_values, target_const_values).all(|(const_value, target_const_value)| {
                can_conform_const(
                    db,
                    (const_value.intern(db), candidate_final),
                    (target_const_value.intern(db), target_final),
                )
            })
        }
        (ConstValue::Struct(_, _), _) => false,

        (
            ConstValue::Enum(concrete_variant, const_value),
            ConstValue::Enum(target_concrete_variant, target_const_value),
        ) => {
            if !can_conform_ty(
                db,
                (concrete_variant.ty, candidate_final),
                (target_concrete_variant.ty, target_final),
            ) {
                return false;
            }
            can_conform_const(
                db,
                (const_value.intern(db), candidate_final),
                (target_const_value.intern(db), target_final),
            )
        }
        (ConstValue::Enum(_, _), _) => false,
        (ConstValue::NonZero(const_value), ConstValue::NonZero(target_const_value)) => {
            can_conform_const(
                db,
                (const_value.intern(db), candidate_final),
                (target_const_value.intern(db), target_final),
            )
        }
        (ConstValue::NonZero(_), _) => false,
        (ConstValue::Boxed(const_value), ConstValue::Boxed(target_const_value)) => {
            can_conform_const(
                db,
                (const_value.intern(db), candidate_final),
                (target_const_value.intern(db), target_final),
            )
        }
        (ConstValue::Boxed(_), _) => false,
        (
            ConstValue::Generic(_)
            | ConstValue::ImplConstant(_)
            | ConstValue::Var(_, _)
            | ConstValue::Missing(_),
            _,
        ) => true,
    }
}
