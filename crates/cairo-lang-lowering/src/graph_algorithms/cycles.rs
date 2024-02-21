use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, FunctionId, FunctionWithBodyId};
use crate::{FlatLowered, MatchInfo};

/// Given the lowering of a function, returns the set of direct callees of that function.
pub fn get_direct_callees(lowered_function: &FlatLowered) -> Vec<FunctionId> {
    let mut direct_callees = Vec::new();
    for (_, block) in lowered_function.blocks.iter() {
        for stmt in &block.statements {
            match stmt {
                crate::Statement::Call(stmt) => {
                    direct_callees.push(stmt.function);
                }
                crate::Statement::Literal(_)
                | crate::Statement::StructConstruct(_)
                | crate::Statement::StructDestructure(_)
                | crate::Statement::EnumConstruct(_)
                | crate::Statement::Snapshot(_)
                | crate::Statement::Desnap(_) => {}
            };
        }
        match &block.end {
            crate::FlatBlockEnd::Match { info: MatchInfo::Extern(s) } => {
                direct_callees.push(s.function);
            }
            crate::FlatBlockEnd::Match { info: MatchInfo::Value(_) }
            | crate::FlatBlockEnd::Match { info: MatchInfo::Enum(_) }
            | crate::FlatBlockEnd::NotSet
            | crate::FlatBlockEnd::Return(_)
            | crate::FlatBlockEnd::Panic(_)
            | crate::FlatBlockEnd::Goto(_, _) => {}
        }
    }
    direct_callees
}

/// Query implementation of
/// [crate::db::LoweringGroup::function_with_body_direct_function_with_body_callees].
pub fn function_with_body_direct_function_with_body_callees(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<OrderedHashSet<FunctionWithBodyId>> {
    let lowered = db.function_with_body_lowering(function_id)?;
    lowered.blocks.has_root()?;

    Ok(get_direct_callees(&lowered)
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
