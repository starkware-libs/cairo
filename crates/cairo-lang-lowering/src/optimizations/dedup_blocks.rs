#[cfg(test)]
#[path = "dedup_blocks_test.rs"]
mod test;

use std::collections::HashMap;

use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{ConcreteVariant, TypeId};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::Itertools;

use crate::ids::FunctionId;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, StatementCall, StatementConst,
    StatementDesnap, StatementEnumConstruct, StatementSnapshot, StatementStructConstruct,
    StatementStructDestructure, VarRemapping, VarUsage, Variable, VariableId,
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
    /// A variable that was defined outside of the block.
    Global(usize),
    /// A variable that was defined inside the block.
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

struct CanonicBlockBuilder<'a> {
    variable: &'a Arena<Variable>,
    vars: UnorderedHashMap<VariableId, usize>,
    types: Vec<TypeId>,
}

impl CanonicBlockBuilder<'_> {
    fn new(variable: &Arena<Variable>) -> CanonicBlockBuilder<'_> {
        CanonicBlockBuilder { variable, vars: Default::default(), types: vec![] }
    }

    /// Converts an input var to a CanonicVar.
    fn handle_input(&mut self, var_usage: &VarUsage) -> CanonicVar {
        match self.vars.get(&var_usage.var_id) {
            Some(local_idx) => CanonicVar::Local(*local_idx),
            None => CanonicVar::Global(var_usage.var_id.index()),
        }
    }

    /// Converts an output var to a CanonicVar.
    fn handle_output(&mut self, v: &VariableId) -> CanonicVar {
        CanonicVar::Local(match self.vars.entry(*v) {
            std::collections::hash_map::Entry::Occupied(e) => *e.get(),
            std::collections::hash_map::Entry::Vacant(e) => {
                self.types.push(self.variable[*v].ty);
                *e.insert(self.types.len() - 1)
            }
        })
    }

    /// Converts a statement to a cononic statement.
    fn handle_statement(&mut self, statement: &Statement) -> CanonicStatement {
        match statement {
            Statement::Const(StatementConst { value, output }) => {
                CanonicStatement::Const { value: value.clone(), output: self.handle_output(output) }
            }
            Statement::Call(StatementCall {
                function,
                inputs,
                with_coupon,
                outputs,
                location: _,
            }) => CanonicStatement::Call {
                function: *function,
                inputs: inputs.iter().map(|input| self.handle_input(input)).collect(),
                with_coupon: *with_coupon,
                outputs: outputs.iter().map(|output| self.handle_output(output)).collect(),
            },
            Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                CanonicStatement::StructConstruct {
                    inputs: inputs.iter().map(|input| self.handle_input(input)).collect(),
                    output: self.handle_output(output),
                }
            }
            Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                CanonicStatement::StructDestructure {
                    input: self.handle_input(input),
                    outputs: outputs.iter().map(|output| self.handle_output(output)).collect(),
                }
            }
            Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                CanonicStatement::EnumConstruct {
                    variant: variant.clone(),
                    input: self.handle_input(input),
                    output: self.handle_output(output),
                }
            }
            Statement::Snapshot(StatementSnapshot { input, outputs }) => {
                CanonicStatement::Snapshot {
                    input: self.handle_input(input),
                    outputs: outputs.map(|output| self.handle_output(&output)),
                }
            }
            Statement::Desnap(StatementDesnap { input, output }) => CanonicStatement::Desnap {
                input: self.handle_input(input),
                output: self.handle_output(output),
            },
        }
    }
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

        let mut builder = CanonicBlockBuilder::new(variable);

        let stmts = block
            .statements
            .iter()
            .map(|statement| builder.handle_statement(statement))
            .collect_vec();

        let returns = returned_vars.iter().map(|input| builder.handle_input(input)).collect();

        Some(CanonicBlock { stmts, types: builder.types, returns })
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

    // Note that the loop below can't be merged with the loop above as a block might be marked as
    // dup after we already visiting an arm that goes to it.
    for block in lowered.blocks.iter_mut() {
        match &mut block.end {
            FlatBlockEnd::Goto(target_block, remappings) if remappings.is_empty() => {
                if let Some(block_id) = duplicates.get(target_block) {
                    *target_block = *block_id;
                }
            }
            FlatBlockEnd::Match { info } => {
                for arm in info.arms_mut() {
                    let Some(block_id) = duplicates.get(&arm.block_id) else {
                        continue;
                    };
                    new_blocks.push(FlatBlock {
                        statements: vec![],
                        end: FlatBlockEnd::Goto(*block_id, VarRemapping::default()),
                    });

                    arm.block_id = next_block_id;
                    next_block_id = next_block_id.next_block_id();
                }
            }
            _ => {}
        }
    }

    for block in new_blocks {
        lowered.blocks.push(block);
    }
}
