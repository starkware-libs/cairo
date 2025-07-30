use std::collections::BTreeMap;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_proc_macros::SemanticObject;
use cairo_lang_utils::ordered_hash_map::Entry;
use cairo_lang_utils::{Intern, LookupIntern};
use itertools::{Itertools, chain, zip_eq};

use super::canonic::{CanonicalImpl, CanonicalTrait, MapperError, ResultNoErrEx};
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
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{ImplTypeById, ImplTypeId};
use crate::{
    ConcreteImplLongId, ConcreteTraitId, GenericArgumentId, GenericParam, TypeId, TypeLongId,
};

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
    if !concrete_trait_id.is_fully_concrete(db) && !canonical_trait.mappings.is_empty() {
        match solve_canonical_trait(db, canonical_trait, lookup_context.clone(), impl_type_bounds.clone()) {
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
    Ok(solve_canonical_trait(
        db,
        CanonicalTrait { id: concrete_trait_id, mappings: ImplVarTraitItemMappings::default() },
        lookup_context,
        impl_type_bounds,
    ))
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

/// Attempts to solve a `canonical_trait`. Will try to find candidates in the given
/// `lookup_context`.
fn solve_canonical_trait(
    db: &dyn SemanticGroup,
    canonical_trait: CanonicalTrait,
    lookup_context: ImplLookupContext,
    impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
) -> SolutionSet<CanonicalImpl> {
    let filter = canonical_trait.id.filter(db);
    let mut candidates =
        find_candidates_at_context(db, &lookup_context, &filter).unwrap_or_default();
    find_closure_generated_candidate(db, canonical_trait.id)
        .map(|candidate| candidates.insert(candidate));

    let mut unique_solution: Option<CanonicalImpl> = None;
    for candidate in candidates.into_iter() {
        let Ok(candidate_solution_set) = solve_candidate(
            db,
            &canonical_trait,
            candidate,
            &lookup_context,
            impl_type_bounds.clone(),
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
                    concrete_trait_id: canonical_trait.id,
                    impls: vec![unique_solution.0, candidate_solution.0],
                });
            }
        }
        unique_solution = Some(candidate_solution);
    }
    unique_solution.map(SolutionSet::Unique).unwrap_or(SolutionSet::None)
}

/// Attempts to solve `candidate` as the requested `canonical_trait`.
fn solve_candidate(
    db: &dyn SemanticGroup,
    canonical_trait: &CanonicalTrait,
    candidate: UninferredImpl,
    lookup_context: &ImplLookupContext,
    impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
) -> InferenceResult<SolutionSet<CanonicalImpl>> {
    let candidate_concrete_trait = candidate.concrete_trait(db).unwrap();
    // If the candidate is fully concrete, or its a generic which is var free, there is nothing
    // to substitute. A generic param may not be var free, if it contains impl types.
    let candidate_final = matches!(candidate, UninferredImpl::GenericParam(_))
        && candidate_concrete_trait.is_var_free(db)
        || candidate_concrete_trait.is_fully_concrete(db);
    let target_final = canonical_trait.id.is_var_free(db);
    let mut lite_inference = LiteInference::new(db);
    if candidate_final && target_final && candidate_concrete_trait != canonical_trait.id {
        return Err(super::ErrorSet);
    }

    let mut res = lite_inference.can_conform_generic_args(
        (&candidate_concrete_trait.generic_args(db), candidate_final),
        (&canonical_trait.id.generic_args(db), target_final),
    );

    // If the candidate is a generic param, its trait is final and not substituted.
    if matches!(candidate, UninferredImpl::GenericParam(_))
        && !lite_inference.substitution.is_empty()
    {
        return Err(super::ErrorSet);
    }

    // If the trait has trait types, we default to using inference.
    if res == CanConformResult::Accepted {
        let Ok(trait_types) = db.trait_types(canonical_trait.id.trait_id(db)) else {
            return Err(super::ErrorSet);
        };
        if !trait_types.is_empty() && !canonical_trait.mappings.types.is_empty() {
            res = CanConformResult::InferenceRequired;
        }
    }

    // Add the defining module of the candidate to the lookup.
    let mut lookup_context = lookup_context.clone();
    lookup_context.insert_lookup_scope(db, &candidate);
    if res == CanConformResult::Rejected {
        return Err(super::ErrorSet);
    } else if CanConformResult::Accepted == res {
        match candidate {
            UninferredImpl::Def(impl_def_id) => {
                let imp_generic_params =
                    db.impl_def_generic_params(impl_def_id).map_err(|_| super::ErrorSet)?;

                match lite_inference.infer_generic_assignment(
                    imp_generic_params,
                    &lookup_context,
                    impl_type_bounds.clone(),
                ) {
                    Ok(SolutionSet::None) => {
                        return Ok(SolutionSet::None);
                    }
                    Ok(SolutionSet::Ambiguous(ambiguity)) => {
                        return Ok(SolutionSet::Ambiguous(ambiguity));
                    }
                    Ok(SolutionSet::Unique(generic_args)) => {
                        let concrete_impl =
                            ConcreteImplLongId { impl_def_id, generic_args }.intern(db);
                        let impl_id = ImplLongId::Concrete(concrete_impl).intern(db);
                        return Ok(SolutionSet::Unique(CanonicalImpl(impl_id)));
                    }
                    _ => {}
                }
            }
            UninferredImpl::GenericParam(generic_param_id) => {
                let impl_id = ImplLongId::GenericParameter(generic_param_id).intern(db);
                return Ok(SolutionSet::Unique(CanonicalImpl(impl_id)));
            }
            // TODO(TomerStarkware): Try to solve for impl alias without inference.
            UninferredImpl::ImplAlias(_) => {}
            UninferredImpl::ImplImpl(_) | UninferredImpl::GeneratedImpl(_) => {}
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

    let mut inference = inference_data.inference(db);
    let solution_set = inference.solution_set()?;
    Ok(match solution_set {
        SolutionSet::None => SolutionSet::None,
        SolutionSet::Ambiguous(ambiguity) => SolutionSet::Ambiguous(ambiguity),
        SolutionSet::Unique(_) => {
            let candidate_impl = inference.rewrite(candidate_impl).no_err();
            match CanonicalImpl::canonicalize(db, candidate_impl, &canonical_embedding) {
                Ok(canonical_impl) => {
                    inference.validate_neg_impls(&lookup_context, canonical_impl)?
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

/// Enum for the result of `can_conform`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CanConformResult {
    Accepted,
    InferenceRequired,
    Rejected,
}

impl CanConformResult {
    fn fold(iter: impl IntoIterator<Item = CanConformResult>) -> CanConformResult {
        let mut res = CanConformResult::Accepted; // Start with a default value of Accepted
        for item in iter {
            match item {
                CanConformResult::Rejected => return CanConformResult::Rejected,
                CanConformResult::Accepted => continue,
                CanConformResult::InferenceRequired => {
                    res = CanConformResult::InferenceRequired;
                }
            }
        }
        res // Return the final result
    }
}
/// An inference without 'vars' that can be used to solve canonical traits which do not contains
/// 'vars' or associated items.
struct LiteInference<'db> {
    db: &'db dyn SemanticGroup,
    substitution: GenericSubstitution,
}

impl<'db> LiteInference<'db> {
    fn new(db: &'db dyn SemanticGroup) -> Self {
        LiteInference { db, substitution: GenericSubstitution::default() }
    }

    /// Tries to infer the generic arguments of the trait from the given params.
    /// If the inference fails (i.e. requires full inferece), returns an error.
    fn infer_generic_assignment(
        &mut self,
        params: Vec<GenericParam>,
        lookup_context: &ImplLookupContext,
        impl_type_bounds: Arc<BTreeMap<ImplTypeById, TypeId>>,
    ) -> InferenceResult<SolutionSet<Vec<GenericArgumentId>>> {
        let mut generic_args = Vec::with_capacity(params.len());
        for param in params {
            match param {
                GenericParam::Type(generic_param_type) => {
                    if self.substitution.contains_key(&generic_param_type.id) {
                        generic_args.push(*self.substitution.get(&generic_param_type.id).unwrap());
                    } else {
                        // If the type is not in the substitution, we cannot solve it without
                        // inference.
                        return Err(super::ErrorSet);
                    }
                }
                GenericParam::Const(generic_param_const) => {
                    if self.substitution.contains_key(&generic_param_const.id) {
                        generic_args.push(*self.substitution.get(&generic_param_const.id).unwrap());
                    } else {
                        // If the const is not in the substitution, we cannot solve it without
                        // inference.
                        return Err(super::ErrorSet);
                    }
                }
                GenericParam::Impl(generic_param_impl) => {
                    if !generic_param_impl.type_constraints.is_empty() {
                        return Err(super::ErrorSet);
                    }
                    if self.substitution.contains_key(&generic_param_impl.id) {
                        generic_args.push(*self.substitution.get(&generic_param_impl.id).unwrap());
                        continue;
                    }

                    let Ok(Ok(imp_concrete_trait_id)) =
                        self.substitution.substitute(self.db, generic_param_impl.concrete_trait)
                    else {
                        return Err(super::ErrorSet);
                    };
                    let canonical_trait = CanonicalTrait {
                        id: imp_concrete_trait_id,
                        mappings: ImplVarTraitItemMappings::default(),
                    };
                    let mut inner_context = lookup_context.clone();
                    enrich_lookup_context(self.db, imp_concrete_trait_id, &mut inner_context);
                    let Ok(solution) = self.db.canonic_trait_solutions(
                        canonical_trait,
                        inner_context,
                        (*impl_type_bounds).clone(),
                    ) else {
                        return Err(super::ErrorSet);
                    };
                    match solution {
                        SolutionSet::None => return Ok(SolutionSet::None),
                        SolutionSet::Unique(imp) => {
                            self.substitution
                                .insert(generic_param_impl.id, GenericArgumentId::Impl(imp.0));
                            generic_args.push(GenericArgumentId::Impl(imp.0));
                        }
                        SolutionSet::Ambiguous(ambiguity) => {
                            return Ok(SolutionSet::Ambiguous(ambiguity));
                        }
                    }
                }
                GenericParam::NegImpl(_) => return Err(super::ErrorSet),
            }
        }
        Ok(SolutionSet::Unique(generic_args))
    }

    /// Checks if the generic arguments of the candidate could be conformed to the generic args of
    /// the trait and if the trait or the candidate contain vars (which would require solving
    /// using inference).
    fn can_conform_generic_args(
        &mut self,
        (candidate_args, candidate_final): (&[GenericArgumentId], bool),
        (target_args, target_final): (&[GenericArgumentId], bool),
    ) -> CanConformResult {
        CanConformResult::fold(zip_eq(candidate_args, target_args).map(
            |(candidate_arg, target_arg)| {
                self.can_conform_generic_arg(
                    (*candidate_arg, candidate_final),
                    (*target_arg, target_final),
                )
            },
        ))
    }

    /// Checks if a [GenericArgumentId] of the candidate could be conformed to a [GenericArgumentId]
    /// of the trait.
    fn can_conform_generic_arg(
        &mut self,
        (candidate_arg, mut candidate_final): (GenericArgumentId, bool),
        (target_arg, mut target_final): (GenericArgumentId, bool),
    ) -> CanConformResult {
        if candidate_arg == target_arg {
            return CanConformResult::Accepted;
        }
        candidate_final = candidate_final || candidate_arg.is_fully_concrete(self.db);
        target_final = target_final || target_arg.is_var_free(self.db);
        if candidate_final && target_final {
            return CanConformResult::Rejected;
        }
        match (candidate_arg, target_arg) {
            (GenericArgumentId::Type(candidate), GenericArgumentId::Type(target)) => {
                self.can_conform_ty((candidate, candidate_final), (target, target_final))
            }
            (GenericArgumentId::Constant(candidate), GenericArgumentId::Constant(target)) => {
                self.can_conform_const((candidate, candidate_final), (target, target_final))
            }
            (GenericArgumentId::Impl(candidate), GenericArgumentId::Impl(target)) => {
                self.can_conform_impl((candidate, candidate_final), (target, target_final))
            }
            (GenericArgumentId::NegImpl, GenericArgumentId::NegImpl) => {
                CanConformResult::InferenceRequired
            }
            _ => CanConformResult::Rejected,
        }
    }

    /// Checks if a generic arg [TypeId] of the candidate could be conformed to a generic arg
    /// [TypeId] of the trait.
    fn can_conform_ty(
        &mut self,
        (candidate_ty, mut candidate_final): (TypeId, bool),
        (target_ty, mut target_final): (TypeId, bool),
    ) -> CanConformResult {
        if candidate_ty == target_ty {
            return CanConformResult::Accepted;
        }
        candidate_final = candidate_final || candidate_ty.is_fully_concrete(self.db);
        target_final = target_final || target_ty.is_var_free(self.db);
        if candidate_final && target_final {
            return CanConformResult::Rejected;
        }
        let target_long_ty = target_ty.lookup_intern(self.db);

        if let TypeLongId::Var(_) = target_long_ty {
            return CanConformResult::InferenceRequired;
        }

        let long_ty_candidate = candidate_ty.lookup_intern(self.db);

        match (long_ty_candidate, target_long_ty) {
            (TypeLongId::Concrete(candidate), TypeLongId::Concrete(target)) => {
                if candidate.generic_type(self.db) != target.generic_type(self.db) {
                    return CanConformResult::Rejected;
                }

                self.can_conform_generic_args(
                    (&candidate.generic_args(self.db), candidate_final),
                    (&target.generic_args(self.db), target_final),
                )
            }
            (TypeLongId::Concrete(_), _) => CanConformResult::Rejected,
            (TypeLongId::Tuple(candidate_tys), TypeLongId::Tuple(target_tys)) => {
                if candidate_tys.len() != target_tys.len() {
                    return CanConformResult::Rejected;
                }

                CanConformResult::fold(zip_eq(candidate_tys, target_tys).map(
                    |(candidate_subty, target_subty)| {
                        self.can_conform_ty(
                            (candidate_subty, candidate_final),
                            (target_subty, target_final),
                        )
                    },
                ))
            }
            (TypeLongId::Tuple(_), _) => CanConformResult::Rejected,
            (TypeLongId::Closure(candidate), TypeLongId::Closure(target)) => {
                if candidate.wrapper_location != target.wrapper_location {
                    return CanConformResult::Rejected;
                }

                let params_check =
                    CanConformResult::fold(zip_eq(candidate.param_tys, target.param_tys).map(
                        |(candidate_subty, target_subty)| {
                            self.can_conform_ty(
                                (candidate_subty, candidate_final),
                                (target_subty, target_final),
                            )
                        },
                    ));
                if params_check == CanConformResult::Rejected {
                    return CanConformResult::Rejected;
                }
                let captured_types_check = CanConformResult::fold(
                    zip_eq(candidate.captured_types, target.captured_types).map(
                        |(candidate_subty, target_subty)| {
                            self.can_conform_ty(
                                (candidate_subty, candidate_final),
                                (target_subty, target_final),
                            )
                        },
                    ),
                );
                if captured_types_check == CanConformResult::Rejected {
                    return CanConformResult::Rejected;
                }
                let return_type_check = self.can_conform_ty(
                    (candidate.ret_ty, candidate_final),
                    (target.ret_ty, target_final),
                );
                if return_type_check == CanConformResult::Rejected {
                    return CanConformResult::Rejected;
                }
                if params_check == CanConformResult::InferenceRequired
                    || captured_types_check == CanConformResult::InferenceRequired
                    || return_type_check == CanConformResult::InferenceRequired
                {
                    return CanConformResult::InferenceRequired;
                }
                CanConformResult::Accepted
            }
            (TypeLongId::Closure(_), _) => CanConformResult::Rejected,
            (
                TypeLongId::FixedSizeArray { type_id: candidate_type_id, size: candidate_size },
                TypeLongId::FixedSizeArray { type_id: target_type_id, size: target_size },
            ) => CanConformResult::fold([
                self.can_conform_const(
                    (candidate_size, candidate_final),
                    (target_size, target_final),
                ),
                self.can_conform_ty(
                    (candidate_type_id, candidate_final),
                    (target_type_id, target_final),
                ),
            ]),
            (TypeLongId::FixedSizeArray { type_id: _, size: _ }, _) => CanConformResult::Rejected,
            (TypeLongId::Snapshot(candidate_inner_ty), TypeLongId::Snapshot(target_inner_ty)) => {
                self.can_conform_ty(
                    (candidate_inner_ty, candidate_final),
                    (target_inner_ty, target_final),
                )
            }
            (TypeLongId::Snapshot(_), _) => CanConformResult::Rejected,
            (TypeLongId::GenericParameter(param), _) => {
                let mut res = CanConformResult::Accepted;
                // if param not in substitution add it otherwise make sure it equal target_ty
                match self.substitution.entry(param) {
                    Entry::Occupied(entry) => {
                        if let GenericArgumentId::Type(existing_ty) = entry.get() {
                            if *existing_ty != target_ty {
                                res = CanConformResult::Rejected;
                            }
                            if !existing_ty.is_var_free(self.db) {
                                return CanConformResult::InferenceRequired;
                            }
                        } else {
                            res = CanConformResult::Rejected;
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(GenericArgumentId::Type(target_ty));
                    }
                }

                if target_ty.is_var_free(self.db) {
                    res
                } else {
                    CanConformResult::InferenceRequired
                }
            }
            (
                TypeLongId::Var(_)
                | TypeLongId::ImplType(_)
                | TypeLongId::Missing(_)
                | TypeLongId::Coupon(_),
                _,
            ) => CanConformResult::InferenceRequired,
        }
    }

    /// Checks if a generic arg [ImplId] of the candidate could be conformed to a generic arg
    /// [ImplId] of the trait.
    fn can_conform_impl(
        &mut self,
        (candidate_impl, mut candidate_final): (ImplId, bool),
        (target_impl, mut target_final): (ImplId, bool),
    ) -> CanConformResult {
        let long_impl_trait = target_impl.lookup_intern(self.db);
        if candidate_impl == target_impl {
            return CanConformResult::Accepted;
        }
        candidate_final = candidate_final || candidate_impl.is_fully_concrete(self.db);
        target_final = target_final || target_impl.is_var_free(self.db);
        if candidate_final && target_final {
            return CanConformResult::Rejected;
        }
        if let ImplLongId::ImplVar(_) = long_impl_trait {
            return CanConformResult::InferenceRequired;
        }
        match (candidate_impl.lookup_intern(self.db), long_impl_trait) {
            (ImplLongId::Concrete(candidate), ImplLongId::Concrete(target)) => {
                let candidate = candidate.lookup_intern(self.db);
                let target = target.lookup_intern(self.db);
                if candidate.impl_def_id != target.impl_def_id {
                    return CanConformResult::Rejected;
                }
                let candidate_args = candidate.generic_args;
                let target_args = target.generic_args;
                self.can_conform_generic_args(
                    (&candidate_args, candidate_final),
                    (&target_args, target_final),
                )
            }
            (ImplLongId::Concrete(_), _) => CanConformResult::Rejected,
            (ImplLongId::GenericParameter(param), _) => {
                let mut res = CanConformResult::Accepted;
                // if param not in substitution add it otherwise make sure it equal target_ty
                match self.substitution.entry(param) {
                    Entry::Occupied(entry) => {
                        if let GenericArgumentId::Impl(existing_impl) = entry.get() {
                            if *existing_impl != target_impl {
                                res = CanConformResult::Rejected;
                            }
                            if !existing_impl.is_var_free(self.db) {
                                return CanConformResult::InferenceRequired;
                            }
                        } else {
                            res = CanConformResult::Rejected;
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(GenericArgumentId::Impl(target_impl));
                    }
                }

                if target_impl.is_var_free(self.db) {
                    res
                } else {
                    CanConformResult::InferenceRequired
                }
            }
            (
                ImplLongId::ImplVar(_)
                | ImplLongId::ImplImpl(_)
                | ImplLongId::SelfImpl(_)
                | ImplLongId::GeneratedImpl(_),
                _,
            ) => CanConformResult::InferenceRequired,
        }
    }

    /// Checks if a generic arg [ConstValueId] of the candidate could be conformed to a generic arg
    /// [ConstValueId] of the trait.
    fn can_conform_const(
        &mut self,
        (candidate_id, mut candidate_final): (ConstValueId, bool),
        (target_id, mut target_final): (ConstValueId, bool),
    ) -> CanConformResult {
        if candidate_id == target_id {
            return CanConformResult::Accepted;
        }
        candidate_final = candidate_final || candidate_id.is_fully_concrete(self.db);
        target_final = target_final || target_id.is_var_free(self.db);
        if candidate_final && target_final {
            return CanConformResult::Rejected;
        }
        let target_long_const = target_id.lookup_intern(self.db);
        if let ConstValue::Var(_, _) = target_long_const {
            return CanConformResult::InferenceRequired;
        }
        match (candidate_id.lookup_intern(self.db), target_long_const) {
            (
                ConstValue::Int(big_int, type_id),
                ConstValue::Int(target_big_int, target_type_id),
            ) => {
                if big_int != target_big_int {
                    return CanConformResult::Rejected;
                }
                self.can_conform_ty((type_id, candidate_final), (target_type_id, target_final))
            }
            (ConstValue::Int(_, _), _) => CanConformResult::Rejected,
            (
                ConstValue::Struct(const_values, type_id),
                ConstValue::Struct(target_const_values, target_type_id),
            ) => {
                if const_values.len() != target_const_values.len() {
                    return CanConformResult::Rejected;
                };
                CanConformResult::fold(chain!(
                    [self.can_conform_ty(
                        (type_id, candidate_final),
                        (target_type_id, target_final)
                    )],
                    zip_eq(const_values, target_const_values).map(
                        |(const_value, target_const_value)| {
                            self.can_conform_const(
                                (const_value.intern(self.db), candidate_final),
                                (target_const_value.intern(self.db), target_final),
                            )
                        }
                    )
                ))
            }
            (ConstValue::Struct(_, _), _) => CanConformResult::Rejected,

            (
                ConstValue::Enum(concrete_variant, const_value),
                ConstValue::Enum(target_concrete_variant, target_const_value),
            ) => CanConformResult::fold([
                self.can_conform_ty(
                    (concrete_variant.ty, candidate_final),
                    (target_concrete_variant.ty, target_final),
                ),
                self.can_conform_const(
                    (const_value.intern(self.db), candidate_final),
                    (target_const_value.intern(self.db), target_final),
                ),
            ]),
            (ConstValue::Enum(_, _), _) => CanConformResult::Rejected,
            (ConstValue::NonZero(const_value), ConstValue::NonZero(target_const_value)) => self
                .can_conform_const(
                    (const_value.intern(self.db), candidate_final),
                    (target_const_value.intern(self.db), target_final),
                ),
            (ConstValue::NonZero(_), _) => CanConformResult::Rejected,
            (ConstValue::Boxed(const_value), ConstValue::Boxed(target_const_value)) => self
                .can_conform_const(
                    (const_value.intern(self.db), candidate_final),
                    (target_const_value.intern(self.db), target_final),
                ),
            (ConstValue::Boxed(_), _) => CanConformResult::Rejected,
            (ConstValue::Generic(param), _) => {
                let mut res = CanConformResult::Accepted;
                match self.substitution.entry(param) {
                    Entry::Occupied(entry) => {
                        if let GenericArgumentId::Constant(existing_const) = entry.get() {
                            if *existing_const != target_id {
                                res = CanConformResult::Rejected;
                            }

                            if !existing_const.is_var_free(self.db) {
                                return CanConformResult::InferenceRequired;
                            }
                        } else {
                            res = CanConformResult::Rejected;
                        }
                    }
                    Entry::Vacant(e) => {
                        e.insert(GenericArgumentId::Constant(target_id));
                    }
                }
                if target_id.is_var_free(self.db) {
                    res
                } else {
                    CanConformResult::InferenceRequired
                }
            }
            (ConstValue::ImplConstant(_) | ConstValue::Var(_, _) | ConstValue::Missing(_), _) => {
                CanConformResult::InferenceRequired
            }
        }
    }
}
