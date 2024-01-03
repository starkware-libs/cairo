use std::sync::Arc;

use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;
use salsa::InternKey;

use crate::db::LoweringGroup;
use crate::ids::FunctionWithBodyId;

/// Query implementation of [crate::db::LoweringGroup::function_with_body_scc].
pub fn function_with_body_scc(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Arc<Vec<FunctionWithBodyId>> {
    let mut scc =
        compute_scc(&FunctionWithBodyNode { function_with_body_id: function_id, db: db.upcast() });
    // Sort the SCCs by their lowest function ID, just for comparison purposes. This would not
    // necessarily be stable between runs.
    scc.sort_by_key(|id| id.as_intern_id());
    Arc::new(scc)
}

/// A node to use in the SCC computation.
#[derive(Clone)]
struct FunctionWithBodyNode<'a> {
    function_with_body_id: FunctionWithBodyId,
    db: &'a dyn LoweringGroup,
}
impl<'a> GraphNode for FunctionWithBodyNode<'a> {
    type NodeId = FunctionWithBodyId;

    fn get_neighbors(&self) -> Vec<Self> {
        self.db
            .function_with_body_direct_function_with_body_callees(self.function_with_body_id)
            .unwrap_or_default()
            .into_iter()
            .map(|function_with_body_id| FunctionWithBodyNode {
                function_with_body_id,
                db: self.db,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_with_body_id
    }
}
