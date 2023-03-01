use std::vec;

use crate::{BlockId, FlatBlockEnd, FlatLowered};

/// Makes sure each block has an incoming fallthrough, by replacing one of the Goto's
/// with a fallthrough.
/// Assumes the blocks are topologically sorted.
pub fn add_fallthroughs(flat_lowered: &mut FlatLowered) {
    let n_blocks = flat_lowered.blocks.len();
    let mut has_fallthorugh = vec![false; flat_lowered.blocks.len()];

    for block_id in (0..n_blocks).rev() {
        let block = &mut flat_lowered.blocks[BlockId(block_id)];

        let block_end_is_fallthrough = matches!(&block.end, FlatBlockEnd::Fallthrough(..));
        match &mut block.end {
            FlatBlockEnd::Fallthrough(target_block_id, ref mut remapping)
            | FlatBlockEnd::Goto(target_block_id, ref mut remapping) => {
                let has_fallthorugh = &mut has_fallthorugh[target_block_id.0];
                if !*has_fallthorugh {
                    if block_end_is_fallthrough {
                        // TODO(ilya): Consider removing `FlatBlockEnd::Fallthrough` generation
                        // before this phase.
                        assert!(!*has_fallthorugh, "Unexpected fallthrough in blk{}", block_id);
                    } else {
                        block.end =
                            FlatBlockEnd::Fallthrough(*target_block_id, std::mem::take(remapping));
                    }
                    *has_fallthorugh = true;
                }
            }
            FlatBlockEnd::Return(_) | FlatBlockEnd::Unreachable | FlatBlockEnd::NotSet => {}
        };
    }
}
