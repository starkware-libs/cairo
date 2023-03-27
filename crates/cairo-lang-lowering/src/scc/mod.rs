use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;

use crate::db::LoweringGroup;

/// Query implementation of [crate::db::LoweringGroup::function_with_body_scc].
pub fn function_with_body_scc(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Vec<FunctionWithBodyId> {
    compute_scc(&FunctionWithBodyNode { function_with_body_id: function_id, db: db.upcast() })
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
