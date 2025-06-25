use std::ops::{Index, IndexMut};

use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_utils::require;

use crate::Block;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);
impl BlockId {
    pub fn root() -> Self {
        Self(0)
    }

    pub fn is_root(&self) -> bool {
        self.0 == 0
    }

    pub fn next_block_id(&self) -> BlockId {
        BlockId(self.0 + 1)
    }
}

/// A convenient wrapper around a vector of blocks.
/// This is used instead of id_arena, since the latter is harder to clone and modify.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct BlocksBuilder(pub Vec<Block>);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Blocks(Vec<Block>);

impl BlocksBuilder {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn alloc(&mut self, block: Block) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
    }
    /// Allocate a new block ID. The block itself should be populated later.
    pub fn alloc_empty(&mut self) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(Block::default());
        id
    }
    /// Sets an already-allocated block.
    pub fn set_block(&mut self, id: BlockId, block: Block) {
        self.0[id.0] = block;
    }

    /// Returns a mutable reference to an already-allocated block.
    pub fn get_mut_block(&mut self, id: BlockId) -> &mut Block {
        &mut self.0[id.0]
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn build(self) -> Option<Blocks> {
        require(!self.is_empty())?;
        Some(Blocks(self.0))
    }
}
impl Blocks {
    pub fn new_errored(_diag_added: DiagnosticAdded) -> Self {
        Self(vec![])
    }

    pub fn get(&self) -> &Vec<Block> {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> BlocksIter<'_> {
        self.into_iter()
    }

    // Note: It is safe to create DiagnosticAdded here, since BlocksBuilder::build() guarantees to
    // build a non empty Blocks. The only way to create an empty Blocks is using
    // `new_errored(DiagnosticAdded)`.
    pub fn root_block(&self) -> Maybe<&Block> {
        if self.is_empty() { Err(DiagnosticAdded) } else { Ok(&self.0[0]) }
    }

    pub fn has_root(&self) -> Maybe<()> {
        if self.is_empty() { Err(DiagnosticAdded) } else { Ok(()) }
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Block> {
        self.0.iter_mut()
    }

    pub fn push(&mut self, block: Block) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
    }

    pub fn reset_block(&mut self, block_id: BlockId, block: Block) {
        self.0[block_id.0] = block;
    }
}
impl Index<BlockId> for Blocks {
    type Output = Block;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index.0]
    }
}
impl IndexMut<BlockId> for Blocks {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}
impl<'a> IntoIterator for &'a Blocks {
    type Item = (BlockId, &'a Block);
    type IntoIter = BlocksIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BlocksIter { blocks: self, index: 0 }
    }
}
pub struct BlocksIter<'a> {
    pub blocks: &'a Blocks,
    pub index: usize,
}
impl<'a> Iterator for BlocksIter<'a> {
    type Item = (BlockId, &'a Block);

    fn next(&mut self) -> Option<Self::Item> {
        self.blocks.0.get(self.index).map(|b| {
            let res = (BlockId(self.index), b);
            self.index += 1;
            res
        })
    }
}
