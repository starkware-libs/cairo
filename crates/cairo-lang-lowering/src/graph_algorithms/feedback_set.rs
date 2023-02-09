use std::collections::HashSet;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::ConcreteFunctionWithBodyId;
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

/// Query implementation of
/// [crate::db::LoweringGroup::priv_function_with_body_feedback_set_of_representative].
pub fn priv_function_with_body_feedback_set_of_representative(
    db: &dyn LoweringGroup,
    function: ConcreteSCCRepresentative,
) -> Maybe<HashSet<ConcreteFunctionWithBodyId>> {
    Ok(calc_feedback_set(&ConcreteFunctionWithBodyNode { function_id: function.0, db }.into()))
}
