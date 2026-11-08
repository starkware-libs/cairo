//! Unit tests for dominator queries.

use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use super::Dominators;
use crate::BlockId;

#[test]
fn test_dominates() {
    // Double diamond CFG dominator sets:
    //   blk0:        {blk0}
    //   blk1, blk2:  {blk0, self}     (arms)
    //   blk3:        {blk0, blk3}     (merge)
    //   blk4, blk5:  {blk3, self, blk0}     (arms)
    //   blk6:        {blk3, blk0, blk6}          (merge)
    let dominators = Dominators {
        per_block: vec![
            OrderedHashSet::from_iter([BlockId(0)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(1)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(2)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(3)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(3), BlockId(4)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(3), BlockId(5)]),
            OrderedHashSet::from_iter([BlockId(0), BlockId(3), BlockId(6)]),
        ],
    };

    // Reflexivity: every block dominates itself.
    for i in 0..dominators.per_block.len() {
        assert!(dominators.dominates(BlockId(i), BlockId(i)));
    }

    // Root dominates every block.
    for i in 0..dominators.per_block.len() {
        assert!(dominators.dominates(BlockId(0), BlockId(i)));
    }

    // Merge block dominates its arms and final merge block.
    assert!(dominators.dominates(BlockId(3), BlockId(4)));
    assert!(dominators.dominates(BlockId(3), BlockId(5)));
    assert!(dominators.dominates(BlockId(3), BlockId(6)));

    // Diamond arms do not dominate each other.
    assert!(!dominators.dominates(BlockId(1), BlockId(2)));
    assert!(!dominators.dominates(BlockId(2), BlockId(1)));

    // Arms do not dominate the merge block.
    assert!(!dominators.dominates(BlockId(1), BlockId(3)));
    assert!(!dominators.dominates(BlockId(2), BlockId(3)));

    // Merge block does not dominate the arms.
    assert!(!dominators.dominates(BlockId(3), BlockId(1)));
    assert!(!dominators.dominates(BlockId(3), BlockId(2)));
}
