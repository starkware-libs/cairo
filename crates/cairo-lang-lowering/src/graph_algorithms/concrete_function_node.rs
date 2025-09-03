use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::ComputeScc;
use salsa::Database;

use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::{DependencyType, LoweringStage};

/// A node to use in graph-algorithms.
#[derive(Clone)]
pub struct ConcreteFunctionWithBodyNode<'db> {
    pub function_id: ConcreteFunctionWithBodyId<'db>,
    pub db: &'db dyn Database,
    pub dependency_type: DependencyType,
    pub stage: LoweringStage,
}
impl<'db> GraphNode for ConcreteFunctionWithBodyNode<'db> {
    type NodeId = ConcreteFunctionWithBodyId<'db>;

    fn get_neighbors(&self) -> Vec<Self> {
        let Ok(direct_callees) = self.db.lowered_direct_callees_with_body(
            self.function_id,
            self.dependency_type,
            self.stage,
        ) else {
            return vec![];
        };
        direct_callees
            .iter()
            .map(|callee| ConcreteFunctionWithBodyNode {
                function_id: *callee,
                db: self.db,
                dependency_type: self.dependency_type,
                stage: self.stage,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_id
    }
}
impl ComputeScc for ConcreteFunctionWithBodyNode<'_> {
    fn compute_scc(&self) -> Vec<Self::NodeId> {
        self.db.lowered_scc(self.function_id, self.dependency_type, self.stage)
    }
}
