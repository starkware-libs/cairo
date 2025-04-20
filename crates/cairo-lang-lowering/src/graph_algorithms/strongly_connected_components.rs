use cairo_lang_defs::ids::UnstableSalsaId;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;

use super::concrete_function_node::{
    ConcreteFunctionWithBodyInlinedNode, ConcreteFunctionWithBodyNode,
};
use crate::DependencyType;
use crate::db::{ConcreteSCCRepresentative, LoweringGroup};
use crate::ids::ConcreteFunctionWithBodyId;

/// Query implementation of
/// [crate::db::LoweringGroup::concrete_function_with_body_scc_representative].
pub fn concrete_function_with_body_scc_representative(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> ConcreteSCCRepresentative {
    ConcreteSCCRepresentative(
        db.concrete_function_with_body_scc(function, dependency_type)
            .into_iter()
            .min_by(|x, y| x.get_internal_id().cmp(y.get_internal_id()))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::concrete_function_with_body_scc].
pub fn concrete_function_with_body_scc(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Vec<ConcreteFunctionWithBodyId> {
    compute_scc(&ConcreteFunctionWithBodyNode { function_id, db: db.upcast(), dependency_type })
}

/// Query implementation of
/// [crate::db::LoweringGroup::concrete_function_with_body_scc_inlined_representative].
pub fn concrete_function_with_body_scc_inlined_representative(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> ConcreteSCCRepresentative {
    ConcreteSCCRepresentative(
        db.concrete_function_with_body_inlined_scc(function, dependency_type)
            .into_iter()
            .min_by(|x, y| x.get_internal_id().cmp(y.get_internal_id()))
            .unwrap_or(function),
    )
}

/// Query implementation of [crate::db::LoweringGroup::concrete_function_with_body_inlined_scc].
pub fn concrete_function_with_body_inlined_scc(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    dependency_type: DependencyType,
) -> Vec<ConcreteFunctionWithBodyId> {
    compute_scc(&ConcreteFunctionWithBodyInlinedNode {
        function_id,
        db: db.upcast(),
        dependency_type,
    })
}
