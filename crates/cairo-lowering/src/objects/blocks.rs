use std::ops::{Index, IndexMut};

use super::Block;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Blocks(pub Vec<Block>);
impl Blocks {
    pub fn alloc(&mut self, block: Block) -> BlockId {
        let res = BlockId(self.0.len());
        self.0.push(block);
        res
    }
    pub fn iter(&self) -> BlocksIter<'_> {
        self.into_iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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
