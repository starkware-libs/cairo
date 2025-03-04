#[cfg(test)]
#[path = "dedup_blocks_test.rs"]
mod test;

use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;

use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{ConcreteVariant, TypeId};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::Itertools;

use crate::ids::FunctionId;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, VarRemapping, VarUsage, Variable,
    VariableId,
};

/// A canonic representation of a block (used to find duplicated blocks).
/// Currently only blocks that end with return are supported.
#[derive(Hash, PartialEq, Eq)]
struct CanonicBlock {
    /// Canonic representation of the statements in the block.
    stmts: Vec<CanonicStatement>,
    /// The types of variables introduced in the block.
    types: Vec<TypeId>,
    /// variables returned by the block.
    returns: Vec<CanonicVar>,
}

/// A canonic representation of a variable in a canonic block.
#[derive(Hash, PartialEq, Eq)]
enum CanonicVar {
    // A variable that was defined outside of the block.
    Global(usize),
    // A variable that was defined inside the block.
    Local(usize),
}

/// A canonic representation of a statement in a canonic block.
#[derive(Hash, PartialEq, Eq)]
enum CanonicStatement {
    Const {
        value: ConstValue,
        output: CanonicVar,
    },
    Call {
        function: FunctionId,
        inputs: Vec<CanonicVar>,
        with_coupon: bool,
        outputs: Vec<CanonicVar>,
    },
    StructConstruct {
        inputs: Vec<CanonicVar>,
        output: CanonicVar,
    },
    StructDestructure {
        input: CanonicVar,
        outputs: Vec<CanonicVar>,
    },
    EnumConstruct {
        variant: ConcreteVariant,
        input: CanonicVar,
        output: CanonicVar,
    },

    Snapshot {
        input: CanonicVar,
        outputs: [CanonicVar; 2],
    },
    Desnap {
        input: CanonicVar,
        output: CanonicVar,
    },
}

impl CanonicBlock {
    /// Tries to create a canonic block from a flat block.
    /// Blocks that do not end in return do not have a canonic representation.
    fn try_from_block(variable: &Arena<Variable>, block: &FlatBlock) -> Option<CanonicBlock> {
        let FlatBlockEnd::Return(returned_vars, _) = &block.end else {
            return None;
        };

        if block.statements.is_empty() {
            // Skip deduplication for empty blocks.
            return None;
        }

        let mut vars: UnorderedHashMap<VariableId, usize> = Default::default();
        let mut types = vec![];

        let handle_input = |vars: &UnorderedHashMap<VariableId, _>, var_usage: &VarUsage| match vars
            .get(&var_usage.var_id)
        {
            Some(local_idx) => CanonicVar::Local(*local_idx),
            None => CanonicVar::Global(var_usage.var_id.index()),
        };

        let mut handle_output = |vars: &mut UnorderedHashMap<VariableId, _>, v: &VariableId| {
            CanonicVar::Local(match vars.entry(*v) {
                std::collections::hash_map::Entry::Occupied(e) => *e.get(),
                std::collections::hash_map::Entry::Vacant(e) => {
                    types.push(variable[*v].ty);
                    *e.insert(types.len() - 1)
                }
            })
        };

        let stmts = block
            .statements
            .iter()
            .map(|stmt| match stmt {
                Statement::Const(statement_const) => CanonicStatement::Const {
                    value: statement_const.value.clone(),
                    output: handle_output(&mut vars, &statement_const.output),
                },
                Statement::Call(statement_call) => CanonicStatement::Call {
                    function: statement_call.function,
                    inputs: statement_call
                        .inputs
                        .iter()
                        .map(|input| handle_input(&vars, input))
                        .collect(),
                    with_coupon: statement_call.with_coupon,
                    outputs: statement_call
                        .outputs
                        .iter()
                        .map(|output| handle_output(&mut vars, output))
                        .collect(),
                },
                Statement::StructConstruct(statement_struct_construct) => {
                    CanonicStatement::StructConstruct {
                        inputs: statement_struct_construct
                            .inputs
                            .iter()
                            .map(|input| handle_input(&vars, input))
                            .collect(),
                        output: handle_output(&mut vars, &statement_struct_construct.output),
                    }
                }
                Statement::StructDestructure(statement_struct_destructure) => {
                    CanonicStatement::StructDestructure {
                        input: handle_input(&vars, &statement_struct_destructure.input),
                        outputs: statement_struct_destructure
                            .outputs
                            .iter()
                            .map(|output| handle_output(&mut vars, output))
                            .collect(),
                    }
                }
                Statement::EnumConstruct(statement_enum_construct) => {
                    CanonicStatement::EnumConstruct {
                        variant: statement_enum_construct.variant.clone(),
                        input: handle_input(&vars, &statement_enum_construct.input),
                        output: handle_output(&mut vars, &statement_enum_construct.output),
                    }
                }
                Statement::Snapshot(statement_snapshot) => CanonicStatement::Snapshot {
                    input: handle_input(&vars, &statement_snapshot.input),
                    outputs: statement_snapshot
                        .outputs
                        .map(|output| handle_output(&mut vars, &output)),
                },
                Statement::Desnap(statement_desnap) => CanonicStatement::Desnap {
                    input: handle_input(&vars, &statement_desnap.input),
                    output: handle_output(&mut vars, &statement_desnap.output),
                },
            })
            .collect_vec();

        Some(CanonicBlock {
            stmts,
            types,
            returns: returned_vars.iter().map(|input| handle_input(&vars, input)).collect(),
        })
    }
}

/// Deduplicates blocks by redirecting goto's and match arms to one of the duplicates.
/// The duplicate blocks will be remove later by `reorganize_blocks`.
pub fn dedup_blocks(lowered: &mut FlatLowered) {
    if lowered.blocks.has_root().is_err() {
        return;
    }

    let mut blocks: HashMap<CanonicBlock, BlockId> = Default::default();
    let mut duplicates: HashMap<BlockId, BlockId> = Default::default();

    for (block_id, block) in lowered.blocks.iter() {
        let Some(canonical_block) = CanonicBlock::try_from_block(&lowered.variables, block) else {
            continue;
        };

        let mut hasher = DefaultHasher::new();
        canonical_block.hash(&mut hasher);

        let opt_mark_dup = match blocks.entry(canonical_block) {
            std::collections::hash_map::Entry::Occupied(e) => {
                duplicates.insert(block_id, *e.get());
                Some(*e.get())
            }
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(block_id);
                None
            }
        };

        if let Some(dup_block) = opt_mark_dup {
            duplicates.entry(dup_block).or_insert_with(|| dup_block);
        }
    }

    let mut new_blocks = vec![];
    let mut next_block_id = BlockId(lowered.blocks.len());

    // Note that the loop below cant be merged with the loop above as a block might be marked as dup
    // after we already visiting an arm that goes to it.
    for block in lowered.blocks.iter_mut() {
        match &mut block.end {
            FlatBlockEnd::Goto(target_block, remappings) if remappings.is_empty() => {
                if let Some(block_id) = duplicates.get(target_block) {
                    *target_block = *block_id;
                    continue;
                }
            }

            FlatBlockEnd::Match { info } => {
                for arm in info.arms_mut() {
                    if let Some(block_id) = duplicates.get(&arm.block_id) {
                        new_blocks.push(FlatBlock {
                            statements: vec![],
                            end: FlatBlockEnd::Goto(*block_id, VarRemapping::default()),
                        });

                        arm.block_id = next_block_id;
                        next_block_id = next_block_id.next_block_id();
                    }
                }
            }
            _ => {}
        }
    }

    for block in new_blocks.into_iter() {
        lowered.blocks.push(block);
    }
}
