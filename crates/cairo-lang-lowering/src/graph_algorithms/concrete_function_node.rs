use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::ComputeScc;

use super::strongly_connected_components::concrete_function_with_body_scc;
use crate::DependencyType;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;

/// A node to use in graph-algorithms.
#[derive(Clone)]
pub struct ConcreteFunctionWithBodyNode<'a> {
    pub function_id: ConcreteFunctionWithBodyId,
    pub db: &'a dyn LoweringGroup,
    pub dependency_type: DependencyType,
}
impl GraphNode for ConcreteFunctionWithBodyNode<'_> {
    type NodeId = ConcreteFunctionWithBodyId;

    fn get_neighbors(&self) -> Vec<Self> {
        let Ok(direct_callees) = self.db.concrete_function_with_body_direct_callees_with_body(
            self.function_id,
            self.dependency_type,
        ) else {
            return vec![];
        };
        direct_callees
            .into_iter()
            .map(|callee| ConcreteFunctionWithBodyNode {
                function_id: callee,
                db: self.db,
                dependency_type: self.dependency_type,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_id
    }
}
impl ComputeScc for ConcreteFunctionWithBodyNode<'_> {
    fn compute_scc(&self) -> Vec<Self::NodeId> {
        concrete_function_with_body_scc(self.db, self.function_id, self.dependency_type)
    }
}

#[derive(Clone)]
pub struct ConcreteFunctionWithBodyInlinedNode<'a> {
    pub function_id: ConcreteFunctionWithBodyId,
    pub db: &'a dyn LoweringGroup,
    pub dependency_type: DependencyType,
}
impl GraphNode for ConcreteFunctionWithBodyInlinedNode<'_> {
    type NodeId = ConcreteFunctionWithBodyId;

    fn get_neighbors(&self) -> Vec<Self> {
        let Ok(direct_callees) =
            self.db.concrete_function_with_body_inlined_direct_callees_with_body(
                self.function_id,
                self.dependency_type,
            )
        else {
            return vec![];
        };
        direct_callees
            .into_iter()
            .map(|callee| ConcreteFunctionWithBodyInlinedNode {
                function_id: callee,
                db: self.db,
                dependency_type: self.dependency_type,
            })
            .collect()
    }

    fn get_id(&self) -> Self::NodeId {
        self.function_id
    }
}
impl ComputeScc for ConcreteFunctionWithBodyInlinedNode<'_> {
    fn compute_scc(&self) -> Vec<Self::NodeId> {
        self.db.concrete_function_with_body_inlined_scc(self.function_id, self.dependency_type)
    }
}
