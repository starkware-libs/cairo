use std::ops::{Index, IndexMut};

use cairo_lang_diagnostics::{skip_diagnostic, Maybe};

use crate::FlatBlock;

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
pub struct Blocks<T>(pub Vec<T>);

impl<T: Default> Blocks<T> {
    pub fn new() -> Self {
        Blocks(vec![])
    }
    pub fn alloc(&mut self, block: T) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(block);
        id
    }
    /// Allocate a new block ID. The block itself should be populated later.
    pub fn alloc_empty(&mut self) -> BlockId {
        let id = BlockId(self.0.len());
        self.0.push(T::default());
        id
    }
    /// Sets an already-allocated block.
    pub fn set_block(&mut self, id: BlockId, block: T) {
        self.0[id.0] = block;
    }

    pub fn iter(&self) -> BlocksIter<'_, T> {
        self.into_iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn root_block(&self) -> Maybe<&T> {
        if self.is_empty() { Err(skip_diagnostic()) } else { Ok(&self.0[0]) }
    }

    pub fn has_root(&self) -> Maybe<()> {
        if self.is_empty() { Err(skip_diagnostic()) } else { Ok(()) }
    }
}
impl<T> Index<BlockId> for Blocks<T> {
    type Output = T;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[index.0]
    }
}
impl<T> IndexMut<BlockId> for Blocks<T> {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}
impl<'a, T> IntoIterator for &'a Blocks<T> {
    type Item = (BlockId, &'a T);
    type IntoIter = BlocksIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        BlocksIter { blocks: self, index: 0 }
    }
}
pub struct BlocksIter<'a, T> {
    pub blocks: &'a Blocks<T>,
    pub index: usize,
}
impl<'a, T> Iterator for BlocksIter<'a, T> {
    type Item = (BlockId, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.blocks.0.get(self.index).map(|b| {
            let res = (BlockId(self.index), b);
            self.index += 1;
            res
        })
    }
}

pub type FlatBlocks = Blocks<FlatBlock>;
