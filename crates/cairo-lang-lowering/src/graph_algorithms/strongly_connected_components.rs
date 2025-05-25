use cairo_lang_defs::ids::UnstableSalsaId;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;

use super::concrete_function_node::ConcreteFunctionWithBodyNode;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::ConcreteFunctionWithBodyId;
use crate::{DependencyType, LoweringStage};

/// Query implementation of
/// [crate::db::LoweringGroup::lowered_scc_representative].
pub fn lowered_scc_representative(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> ConcreteSCCRepresentative {
    ConcreteSCCRepresentative(
        db.lowered_scc(function, dependency_type, stage)
            .into_iter()
            .min_by(|x, y| x.get_internal_id().cmp(y.get_internal_id()))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::lowered_scc].
pub fn lowered_scc(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> Vec<ConcreteFunctionWithBodyId> {
    compute_scc(&ConcreteFunctionWithBodyNode { function_id, db, dependency_type, stage })
}
