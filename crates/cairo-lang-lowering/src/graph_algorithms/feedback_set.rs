use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::graph_algos::feedback_set::calc_feedback_set;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use super::concrete_function_postiniline_node::ConcreteFunctionWithBodyPostInlineNode;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::ConcreteFunctionWithBodyId;
use crate::DependencyType;

/// Query implementation of [crate::db::LoweringGroup::function_with_body_feedback_set].
pub fn function_with_body_feedback_set(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Maybe<OrderedHashSet<ConcreteFunctionWithBodyId>> {
    let r = db.concrete_function_with_body_scc_representative(function, dependency_type);
    db.priv_function_with_body_feedback_set_of_representative(r, dependency_type)
}

/// Query implementation of [crate::db::LoweringGroup::needs_withdraw_gas].
pub fn needs_withdraw_gas(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    if let Some(flag) = db.get_flag(FlagId::new(db.upcast(), "add_withdraw_gas")) {
        if !extract_matches!(*flag, Flag::AddWithdrawGas) {
            return Ok(false);
        }
    }

    Ok(db.function_with_body_feedback_set(function, DependencyType::Cost)?.contains(&function))
}

/// Query implementation of
/// [crate::db::LoweringGroup::priv_function_with_body_feedback_set_of_representative].
pub fn priv_function_with_body_feedback_set_of_representative(
    db: &dyn LoweringGroup,
    function: ConcreteSCCRepresentative,
    dependency_type: DependencyType,
) -> Maybe<OrderedHashSet<ConcreteFunctionWithBodyId>> {
    Ok(calc_feedback_set(
        &ConcreteFunctionWithBodyPostInlineNode { function_id: function.0, db, dependency_type }
            .into(),
    ))
}
