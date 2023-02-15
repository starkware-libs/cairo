use std::vec;

use crate::{BlockId, FlatBlockEnd, FlatLowered, Statement};

struct TopoSortContext {
    /// The stack of the nodes in the DFS.
    stack: Vec<BlockId>,

    /// Number of incoming edges to each block, indexed by block id.
    incoming_edges: Vec<usize>,

    // Toplogically sorted block ids.
    output: Vec<BlockId>,
}

/// Does a topological sort of the blocks graphs and adds missing Fallthroughs.
impl TopoSortContext {
    pub fn visit(&mut self, block_id: &BlockId) {
        let incoming_edges = &mut self.incoming_edges[block_id.0];
        *incoming_edges += 1;

        if *incoming_edges == 1 {
            self.stack.push(*block_id);
        }
    }

    pub fn topo_sort(&mut self, flat_lowered: &mut FlatLowered) {
        self.incoming_edges.resize(flat_lowered.blocks.len(), 0);
        let mut has_fallthorugh = vec![];
        has_fallthorugh.resize(flat_lowered.blocks.len(), false);

        while let Some(block_id) = self.stack.pop() {
            let block = &mut flat_lowered.blocks[block_id];

            if let Some(statement) = block.statements.last() {
                match statement {
                    Statement::MatchExtern(stmt) => {
                        for (_, target_block_id) in stmt.arms.iter() {
                            self.visit(target_block_id)
                        }
                    }
                    Statement::MatchEnum(stmt) => {
                        for (_, target_block_id) in stmt.arms.iter() {
                            self.visit(target_block_id)
                        }
                    }
                    Statement::Literal(_)
                    | Statement::Call(_)
                    | Statement::StructConstruct(_)
                    | Statement::StructDestructure(_)
                    | Statement::EnumConstruct(_)
                    | Statement::Snapshot(_)
                    | Statement::Desnap(_) => {}
                }
            }

            match &mut block.end {
                FlatBlockEnd::Fallthrough(target_block_id, _) => {
                    self.visit(target_block_id);

                    let has_fallthorugh = &mut has_fallthorugh[target_block_id.0];
                    assert!(!*has_fallthorugh, "Unexpected fallthrough");
                    *has_fallthorugh = true;
                }

                FlatBlockEnd::Goto(target_block_id, ref mut remapping) => {
                    self.visit(target_block_id);

                    let has_fallthorugh = &mut has_fallthorugh[target_block_id.0];
                    if !*has_fallthorugh {
                        block.end =
                            FlatBlockEnd::Fallthrough(*target_block_id, std::mem::take(remapping));
                        *has_fallthorugh = true;
                    }
                }
                FlatBlockEnd::Return(_)
                | FlatBlockEnd::Unreachable
                | FlatBlockEnd::Callsite(_)
                | FlatBlockEnd::NotSet => {}
            };

            self.output.push(block_id);
        }
    }
}

pub fn add_fallthroughs(flat_lowered: &mut FlatLowered) {
    if !flat_lowered.blocks.is_empty() {
        let mut ctx = TopoSortContext {
            stack: vec![BlockId::root()],
            incoming_edges: vec![],
            output: vec![],
        };
        ctx.topo_sort(flat_lowered);

        // TODO(ilya): Merge blocks that fallthrough into a block with a single incoming_edge.
    }
}
