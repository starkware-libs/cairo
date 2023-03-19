use std::collections::HashSet;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::ConcreteFunctionWithBodyId;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::graph_algos::feedback_set::calc_feedback_set;

use super::concrete_function_node::ConcreteFunctionWithBodyNode;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};

/// Query implementation of [crate::db::LoweringGroup::function_with_body_feedback_set].
pub fn function_with_body_feedback_set(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<HashSet<ConcreteFunctionWithBodyId>> {
    let r = db.concrete_function_with_body_scc_representative(function);
    db.priv_function_with_body_feedback_set_of_representative(r)
}

/// Query implementation of [crate::db::LoweringGroup::needs_withdraw_gas].
pub fn needs_withdraw_gas(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    Ok(if let Some(flag) = db.get_flag(FlagId::new(db.upcast(), "add_withdraw_gas")) {
        extract_matches!(*flag, Flag::AddWithdrawGas)
            && db.function_with_body_feedback_set(function)?.contains(&function)
    } else {
        false
    })
}

/// Query implementation of
/// [crate::db::LoweringGroup::priv_function_with_body_feedback_set_of_representative].
pub fn priv_function_with_body_feedback_set_of_representative(
    db: &dyn LoweringGroup,
    function: ConcreteSCCRepresentative,
) -> Maybe<HashSet<ConcreteFunctionWithBodyId>> {
    Ok(calc_feedback_set(&ConcreteFunctionWithBodyNode { function_id: function.0, db }.into()))
}
