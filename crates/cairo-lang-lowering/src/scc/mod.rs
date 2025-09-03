use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::compute_scc;
use salsa::Database;

use crate::DependencyType;
use crate::db::LoweringGroup;
use crate::ids::FunctionWithBodyId;

/// Query implementation of [crate::db::LoweringGroup::function_with_body_scc].
#[salsa::tracked(returns(ref))]
pub fn function_with_body_scc<'db>(
    db: &'db dyn Database,
    function_id: FunctionWithBodyId<'db>,
    dependency_type: DependencyType,
) -> Vec<FunctionWithBodyId<'db>> {
    compute_scc(&FunctionWithBodyNode { function_with_body_id: function_id, dependency_type, db })
}

/// A node to use in the SCC computation.
#[derive(Clone)]
struct FunctionWithBodyNode<'db> {
    function_with_body_id: FunctionWithBodyId<'db>,
    dependency_type: DependencyType,
    db: &'db dyn Database,
}
impl<'db> GraphNode for FunctionWithBodyNode<'db> {
    type NodeId = FunctionWithBodyId<'db>;

    fn get_neighbors(&self) -> Vec<Self> {
        self.db
            .function_with_body_direct_function_with_body_callees(
                self.function_with_body_id,
                self.dependency_type,
            )
            .into_iter()
            .flatten()
            .map(|function_with_body_id| FunctionWithBodyNode {
                function_with_body_id: *function_with_body_id,
                dependency_type: self.dependency_type,
                db: self.db,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_with_body_id
    }
}
