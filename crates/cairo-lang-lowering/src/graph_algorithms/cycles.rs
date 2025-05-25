use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::db::{LoweringGroup, get_direct_callees};
use crate::ids::{
    ConcreteFunctionWithBodyId, FunctionId, FunctionWithBodyId, GenericOrSpecialized,
};
use crate::{DependencyType, LoweringStage};

/// Query implementation of
/// [crate::db::LoweringGroup::function_with_body_direct_callees].
pub fn function_with_body_direct_callees(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<OrderedHashSet<FunctionId>> {
    let (lowered, block_extra_calls) =
        db.function_with_body_lowering_with_borrow_check(function_id)?;
    Ok(get_direct_callees(db, &lowered, dependency_type, &block_extra_calls).into_iter().collect())
}

/// Query implementation of
/// [crate::db::LoweringGroup::function_with_body_direct_function_with_body_callees].
pub fn function_with_body_direct_function_with_body_callees(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<OrderedHashSet<FunctionWithBodyId>> {
    Ok(db
        .function_with_body_direct_callees(function_id, dependency_type)?
        .into_iter()
        .map(|function_id| function_id.body(db))
        .collect::<Maybe<Vec<Option<_>>>>()?
        .into_iter()
        .flatten()
        .map(|x| match x.generic_or_specialized(db) {
            GenericOrSpecialized::Generic(id) => id,
            GenericOrSpecialized::Specialized(_) => {
                unreachable!("Specialization of functions only occurs post concretization.")
            }
        })
        .collect())
}

/// Query implementation of [LoweringGroup::final_contains_call_cycle].
pub fn final_contains_call_cycle(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    let direct_callees = db.lowered_direct_callees_with_body(
        function_id,
        DependencyType::Call,
        LoweringStage::Final,
    )?;
    for callee in direct_callees {
        if db.final_contains_call_cycle(callee)? {
            return Ok(true);
        }
    }

    Ok(false)
}

/// Cycle handling for [LoweringGroup::final_contains_call_cycle].
pub fn final_contains_call_cycle_handle_cycle(
    _db: &dyn LoweringGroup,
    _cycle: &salsa::Cycle,
    _function_id: &ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    Ok(true)
}

/// Query implementation of [LoweringGroup::in_cycle].
pub fn in_cycle(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<bool> {
    if db
        .function_with_body_direct_function_with_body_callees(function_id, dependency_type)?
        .contains(&function_id)
    {
        return Ok(true);
    }
    Ok(db.function_with_body_scc(function_id, dependency_type).len() > 1)
}

/// Query implementation of [LoweringGroup::concrete_in_cycle].
pub fn concrete_in_cycle(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> Maybe<bool> {
    if db
        .lowered_direct_callees_with_body(function_id, dependency_type, stage)?
        .contains(&function_id)
    {
        return Ok(true);
    }
    Ok(db.lowered_scc(function_id, dependency_type, stage).len() > 1)
}
