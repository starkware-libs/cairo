use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::db::{get_direct_callees, LoweringGroup};
use crate::ids::{ConcreteFunctionWithBodyId, FunctionId, FunctionWithBodyId};
use crate::DependencyType;

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
        .map(|x| x.function_with_body_id(db))
        .collect())
}

/// Query implementation of [LoweringGroup::contains_cycle].
pub fn contains_cycle(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<bool> {
    let direct_callees =
        db.concrete_function_with_body_direct_callees_with_body(function_id, dependency_type)?;
    for callee in direct_callees {
        if db.contains_cycle(callee, dependency_type)? {
            return Ok(true);
        }
    }

    Ok(false)
}

/// Cycle handling for [LoweringGroup::contains_cycle].
pub fn contains_cycle_handle_cycle(
    _db: &dyn LoweringGroup,
    _cycle: &[String],
    _function_id: &ConcreteFunctionWithBodyId,
    _dependency_type: &DependencyType,
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
