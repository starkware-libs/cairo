use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;
use salsa::Database;

use super::concrete_function_node::ConcreteFunctionWithBodyNode;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::ConcreteFunctionWithBodyId;
use crate::{DependencyType, LoweringStage};

/// Query implementation of
/// [crate::db::LoweringGroup::lowered_scc_representative].
#[salsa::tracked(returns(clone))]
pub fn lowered_scc_representative<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> ConcreteSCCRepresentative<'db> {
    ConcreteSCCRepresentative(
        db.lowered_scc(function, dependency_type, stage)
            .into_iter()
            .min_by_key(|function| function.full_path(db))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::lowered_scc].
#[salsa::tracked(returns(clone))]
pub fn lowered_scc<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    stage: LoweringStage,
) -> Vec<ConcreteFunctionWithBodyId<'db>> {
    compute_scc(&ConcreteFunctionWithBodyNode { function_id, db, dependency_type, stage })
}
