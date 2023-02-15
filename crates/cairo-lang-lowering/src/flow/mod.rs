use std::vec;

use crate::{BlockId, FlatBlockEnd, FlatLowered, Statement};

struct AddFallthroughContext {
    /// The stack of discovered block_ids.
    stack: Vec<BlockId>,
    // Vector of visited blocks, indexed by blockId.
    visited: Vec<bool>,
}

/// Does a topological sort of the blocks graphs and adds missing Fallthroughs.
impl AddFallthroughContext {
    fn visit(&mut self, block_id: &BlockId) {
        let visited = &mut self.visited[block_id.0];
        if !*visited {
            self.stack.push(*block_id);
            *visited = true;
        }
    }

    fn add_fallthroughs(&mut self, flat_lowered: &mut FlatLowered) {
        let mut has_fallthorugh = vec![false; flat_lowered.blocks.len()];

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

            let block_end_is_fallthrough = matches!(&block.end, FlatBlockEnd::Fallthrough(..));
            match &mut block.end {
                FlatBlockEnd::Fallthrough(target_block_id, ref mut remapping)
                | FlatBlockEnd::Goto(target_block_id, ref mut remapping) => {
                    self.visit(target_block_id);

                    let has_fallthorugh = &mut has_fallthorugh[target_block_id.0];
                    if !*has_fallthorugh {
                        if block_end_is_fallthrough {
                            // TODO(ilya): Consider removing `FlatBlockEnd::Fallthrough` generation
                            // before this phase.
                            assert!(
                                !*has_fallthorugh,
                                "Unexpected fallthrough in blk{}",
                                block_id.0
                            );
                        } else {
                            block.end = FlatBlockEnd::Fallthrough(
                                *target_block_id,
                                std::mem::take(remapping),
                            );
                        }
                        *has_fallthorugh = true;
                    }
                }
                FlatBlockEnd::Return(_)
                | FlatBlockEnd::Unreachable
                | FlatBlockEnd::Callsite(_)
                | FlatBlockEnd::NotSet => {}
            };
        }
    }
}

/// Makes sure each block has an incoming fallthrough, by replacing one of the Goto's
/// with a fallthrough.
pub fn add_fallthroughs(flat_lowered: &mut FlatLowered) {
    if !flat_lowered.blocks.is_empty() {
        let mut ctx = AddFallthroughContext {
            stack: vec![BlockId::root()],
            visited: vec![false; flat_lowered.blocks.len()],
        };
        ctx.add_fallthroughs(flat_lowered);
    }
}
