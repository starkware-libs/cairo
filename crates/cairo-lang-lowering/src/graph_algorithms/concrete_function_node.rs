use cairo_lang_utils::graph_algos::graph_node::GraphNode;
use cairo_lang_utils::graph_algos::strongly_connected_components::ComputeScc;

use super::strongly_connected_components::concrete_function_with_body_scc;
use crate::db::LoweringGroup;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::DependencyType;

/// A node to use in graph-algorithms.
#[derive(Clone)]
pub struct ConcreteFunctionWithBodyNode<'a> {
    pub function_id: ConcreteFunctionWithBodyId,
    pub db: &'a dyn LoweringGroup,
    pub dependency_type: DependencyType,
}
impl<'a> GraphNode for ConcreteFunctionWithBodyNode<'a> {
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
impl<'a> ComputeScc for ConcreteFunctionWithBodyNode<'a> {
    fn compute_scc(&self) -> Vec<Self::NodeId> {
        concrete_function_with_body_scc(self.db, self.function_id, self.dependency_type)
    }
}

#[derive(Clone)]
pub struct ConcreteFunctionWithBodyPostPanicNode<'a> {
    pub function_id: ConcreteFunctionWithBodyId,
    pub db: &'a dyn LoweringGroup,
    pub dependency_type: DependencyType,
}
impl<'a> GraphNode for ConcreteFunctionWithBodyPostPanicNode<'a> {
    type NodeId = ConcreteFunctionWithBodyId;

    fn get_neighbors(&self) -> Vec<Self> {
        let Ok(direct_callees) =
            self.db.concrete_function_with_body_postpanic_direct_callees_with_body(
                self.function_id,
                self.dependency_type,
            )
        else {
            return vec![];
        };
        direct_callees
            .into_iter()
            .map(|callee| ConcreteFunctionWithBodyPostPanicNode {
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
impl<'a> ComputeScc for ConcreteFunctionWithBodyPostPanicNode<'a> {
    fn compute_scc(&self) -> Vec<Self::NodeId> {
        concrete_function_with_body_scc(self.db, self.function_id, self.dependency_type)
    }
}
