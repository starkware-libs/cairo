use std::ops::{Index, IndexMut};

use cairo_lang_diagnostics::{DiagnosticAdded, Maybe};
use cairo_lang_utils::require;

use crate::Block;

#[derive(Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
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
impl core::fmt::Debug for BlockId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "blk{}", self.0)
    }
}

/// A convenient wrapper around a vector of blocks.
/// This is used instead of id_arena, since the latter is harder to clone and modify.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct BlocksBuilder<'db>(pub Vec<Block<'db>>);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Blocks<'db>(Vec<Block<'db>>);

impl<'db> BlocksBuilder<'db> {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn alloc(&mut self, block: Block<'db>) -> BlockId {
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
    pub fn set_block(&mut self, id: BlockId, block: Block<'db>) {
        self.0[id.0] = block;
    }

    /// Returns a mutable reference to an already-allocated block.
    pub fn get_mut_block<'m>(&'m mut self, id: BlockId) -> &'m mut Block<'db> {
        &mut self.0[id.0]
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn build(self) -> Option<Blocks<'db>> {
        require(!self.is_empty())?;
        Some(Blocks(self.0))
    }
}
impl<'db> Blocks<'db> {
    pub fn new_errored(_diag_added: DiagnosticAdded) -> Self {
        Self(vec![])
    }

    pub fn get(&self) -> &Vec<Block<'db>> {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(
        &self,
    ) -> impl DoubleEndedIterator<Item = (BlockId, &Block<'db>)> + ExactSizeIterator {
        self.0.iter().enumerate().map(|(i, b)| (BlockId(i), b))
    }

    // Note: It is safe to create DiagnosticAdded here, since BlocksBuilder::build() guarantees to
    // build a non empty Blocks. The only way to create an empty Blocks is using
    // `new_errored(DiagnosticAdded)`.
    pub fn root_block(&self) -> Maybe<&Block<'db>> {
        if self.is_empty() { Err(DiagnosticAdded) } else { Ok(&self.0[0]) }
    }

    pub fn has_root(&self) -> Maybe<()> {
        if self.is_empty() { Err(DiagnosticAdded) } else { Ok(()) }
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Block<'db>> {
        self.0.iter_mut()
    }

    pub fn push(&mut self, block: Block<'db>) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
    }

    pub fn reset_block(&mut self, block_id: BlockId, block: Block<'db>) {
        self.0[block_id.0] = block;
    }
}
impl<'db> Index<BlockId> for Blocks<'db> {
    type Output = Block<'db>;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index.0]
    }
}
impl<'db> IndexMut<BlockId> for Blocks<'db> {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}
