use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::ConcreteFunctionWithBodyId;

use crate::db::LoweringGroup;

/// Query implementation of [LoweringGroup::contains_cycle].
pub fn contains_cycle(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    let direct_callees = db.concrete_function_with_body_direct_callees_with_body(function_id)?;
    for callee in direct_callees {
        if db.contains_cycle(callee)? {
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
) -> Maybe<bool> {
    Ok(true)
}

/// Query implementation of [LoweringGroup::in_cycle].
pub fn in_cycle(db: &dyn LoweringGroup, function_id: FunctionWithBodyId) -> Maybe<bool> {
    if db.function_with_body_direct_function_with_body_callees(function_id)?.contains(&function_id)
    {
        return Ok(true);
    }
    Ok(db.function_with_body_scc(function_id).len() > 1)
}
