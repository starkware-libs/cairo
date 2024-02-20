use cairo_lang_defs::ids::UnstableSalsaId;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;

use super::concrete_function_node::{
    ConcreteFunctionWithBodyNode, ConcreteFunctionWithBodyPostPanicNode,
};
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::ConcreteFunctionWithBodyId;

/// Query implementation of
/// [crate::db::LoweringGroup::concrete_function_with_body_scc_representative].
pub fn concrete_function_with_body_scc_representative(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> ConcreteSCCRepresentative {
    ConcreteSCCRepresentative(
        db.concrete_function_with_body_scc(function)
            .into_iter()
            .min_by(|x, y| x.get_internal_id().cmp(y.get_internal_id()))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::concrete_function_with_body_scc].
pub fn concrete_function_with_body_scc(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Vec<ConcreteFunctionWithBodyId> {
    compute_scc(&ConcreteFunctionWithBodyNode { function_id, db: db.upcast() })
}

/// Query implementation of
/// [crate::db::LoweringGroup::concrete_function_with_body_scc_postpanic_representative].
pub fn concrete_function_with_body_scc_postpanic_representative(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
) -> ConcreteSCCRepresentative {
    ConcreteSCCRepresentative(
        db.concrete_function_with_body_postpanic_scc(function)
            .into_iter()
            .min_by(|x, y| x.get_internal_id().cmp(y.get_internal_id()))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::concrete_function_with_body_postpanic_scc].
pub fn concrete_function_with_body_postpanic_scc(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Vec<ConcreteFunctionWithBodyId> {
    compute_scc(&ConcreteFunctionWithBodyPostPanicNode { function_id, db: db.upcast() })
}
