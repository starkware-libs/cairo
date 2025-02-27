#[cfg(test)]
#[path = "block_dedup_test.rs"]
mod test;

use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use cairo_lang_semantic::{TypeId};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::Itertools;


use crate::optimizations::remappings;
use crate::{BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, Variable, VariableId};

#[derive(Hash, PartialEq, Eq)]
struct CanonicBlock {
    stmts: Vec<(std::mem::Discriminant<Statement>, Vec<usize>, Vec<usize>)>,
    types: Vec<TypeId>,
}

impl CanonicBlock {
    fn from_block(variable: &Arena<Variable>, block: &FlatBlock) -> CanonicBlock {
        let mut vars: UnorderedHashMap<VariableId, usize> = Default::default();
        let mut types = vec![];

        let mut handle_var = |v| match vars.entry(v) {
            std::collections::hash_map::Entry::Occupied(e) => *e.get(),
            std::collections::hash_map::Entry::Vacant(e) => {
                types.push(variable[v].ty);
                *e.insert(types.len() - 1)
            }
        };
        let stmts = block
            .statements
            .iter()
            .map(|stmt| {
                (
                    std::mem::discriminant(stmt),
                    stmt.inputs().iter().map(|input| handle_var(input.var_id)).collect_vec(),
                    stmt.outputs().iter().map(|output| handle_var(*output)).collect_vec(),
                )
            })
            .collect_vec();

        CanonicBlock { stmts, types }

       
    }
}


pub fn dedup_blocks(lowered: &mut FlatLowered) {
    if lowered.blocks.has_root().is_err() {
        return;
    }

    let mut blocks: HashMap<CanonicBlock, BlockId> = Default::default();

    let mut duplicates: HashMap<BlockId, BlockId> = Default::default();

    for (block_id, block) in lowered.blocks.iter_mut().enumerate().rev() {

        match &mut block.end {
            FlatBlockEnd::Return(..) => {}
            FlatBlockEnd::Goto(target_block, remappings, ) if remappings.is_empty() => {
                if let Some(block_id) = duplicates.get(&target_block) {
                    *target_block = *block_id;
                    continue;
                }

            }
            _ => continue
        }

   

        let canonical_block = CanonicBlock::from_block(&lowered.variables, block);

        let mut hasher = DefaultHasher::new();
        canonical_block.hash(&mut hasher);

        println!("canonical_block hash: {}", hasher.finish());

        match blocks.entry(canonical_block) {
            std::collections::hash_map::Entry::Occupied(e) => {
                duplicates.insert(BlockId(block_id), *e.get());
                eprintln!("{:?} is a duplicate of {:?}", block_id, e.get());
            }
            std::collections::hash_map::Entry::Vacant(e) => {
               
                e.insert(BlockId(block_id));
            }
        }
    }


    // for block in lowered.blocks.iter_mut() {
    //     let FlatBlockEnd::Goto(, )
    // }
}
