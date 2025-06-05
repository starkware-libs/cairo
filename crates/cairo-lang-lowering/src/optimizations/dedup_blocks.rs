#[cfg(test)]
#[path = "dedup_blocks_test.rs"]
mod test;

use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{ConcreteVariant, TypeId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::{self, UnorderedHashMap};
use id_arena::Arena;
use itertools::{Itertools, zip_eq};

use crate::ids::FunctionId;
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    Block, BlockEnd, BlockId, Lowered, Statement, StatementCall, StatementConst, StatementDesnap,
    StatementEnumConstruct, StatementSnapshot, StatementStructConstruct,
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
struct CanonicVar(usize);

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
    inputs: Vec<VarUsage>,
}

impl CanonicBlockBuilder<'_> {
    fn new(variable: &Arena<Variable>) -> CanonicBlockBuilder<'_> {
        CanonicBlockBuilder {
            variable,
            vars: Default::default(),
            types: vec![],
            inputs: Default::default(),
        }
    }

    /// Converts an input var to a CanonicVar.
    fn handle_input(&mut self, var_usage: &VarUsage) -> CanonicVar {
        let v = var_usage.var_id;

        CanonicVar(match self.vars.entry(v) {
            std::collections::hash_map::Entry::Occupied(e) => *e.get(),
            std::collections::hash_map::Entry::Vacant(e) => {
                self.types.push(self.variable[v].ty);
                let new_id = *e.insert(self.types.len() - 1);
                self.inputs.push(*var_usage);
                new_id
            }
        })
    }

    /// Converts an output var to a CanonicVar.
    fn handle_output(&mut self, v: &VariableId) -> CanonicVar {
        CanonicVar(match self.vars.entry(*v) {
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
                    variant: *variant,
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
    /// Return the canonic representation of the block and the external inputs used in the block.
    /// Blocks that do not end in return do not have a canonic representation.
    fn try_from_block(
        variable: &Arena<Variable>,
        block: &Block,
    ) -> Option<(CanonicBlock, Vec<VarUsage>)> {
        let BlockEnd::Return(returned_vars, _) = &block.end else {
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

        Some((CanonicBlock { stmts, types: builder.types, returns }, builder.inputs))
    }
}
/// Helper class to reassign variable ids.
pub struct VarReassigner<'a> {
    pub variables: &'a mut Arena<Variable>,

    // Maps old var_id to new_var_id
    pub vars: UnorderedHashMap<VariableId, VariableId>,
}

impl<'a> VarReassigner<'a> {
    pub fn new(variables: &'a mut Arena<Variable>) -> Self {
        Self { variables, vars: UnorderedHashMap::default() }
    }
}

impl Rebuilder for VarReassigner<'_> {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        *self.vars.entry(var).or_insert_with(|| self.variables.alloc(self.variables[var].clone()))
    }
}

#[derive(Default)]
struct DedupContext {
    /// Maps a CanonicBlock to a reference block that matches it.
    canonic_blocks: UnorderedHashMap<CanonicBlock, BlockId>,

    /// Maps a block to the inputs that are needed for it to be shared by multiple flows.
    block_id_to_inputs: UnorderedHashMap<BlockId, Vec<VarUsage>>,
}

/// Given a block and a set of inputs, assigns new ids to all the variables in the block, returning
/// the new block and the new inputs.
fn rebuild_block_and_inputs(
    variables: &mut Arena<Variable>,
    block: &Block,
    inputs: &[VarUsage],
) -> (Block, Vec<VarUsage>) {
    let mut var_reassigner = VarReassigner::new(variables);
    (
        var_reassigner.rebuild_block(block),
        inputs.iter().map(|var_usage| var_reassigner.map_var_usage(*var_usage)).collect(),
    )
}

/// Deduplicates blocks by redirecting goto's and match arms to one of the duplicates.
/// The duplicate blocks will be removed later by `reorganize_blocks`.
pub fn dedup_blocks(lowered: &mut Lowered) {
    if lowered.blocks.has_root().is_err() {
        return;
    }

    let mut ctx = DedupContext::default();
    // Maps duplicated blocks to the new shared block and the inputs that need to be remapped for
    // the block.
    let mut duplicates: UnorderedHashMap<BlockId, (BlockId, Vec<VarUsage>)> = Default::default();

    let mut new_blocks = vec![];
    let mut next_block_id = BlockId(lowered.blocks.len());

    for (block_id, block) in lowered.blocks.iter() {
        let Some((canonical_block, inputs)) =
            CanonicBlock::try_from_block(&lowered.variables, block)
        else {
            continue;
        };

        match ctx.canonic_blocks.entry(canonical_block) {
            unordered_hash_map::Entry::Occupied(e) => {
                let block_and_inputs = duplicates
                    .entry(*e.get())
                    .or_insert_with(|| {
                        let (block, new_inputs) =
                            rebuild_block_and_inputs(&mut lowered.variables, block, &inputs);
                        new_blocks.push(block);
                        let new_block_id = next_block_id;
                        next_block_id = next_block_id.next_block_id();

                        (new_block_id, new_inputs)
                    })
                    .clone();

                duplicates.insert(block_id, block_and_inputs);
            }
            unordered_hash_map::Entry::Vacant(e) => {
                e.insert(block_id);
            }
        };

        ctx.block_id_to_inputs.insert(block_id, inputs);
    }

    let mut new_goto_block = |block_id, inputs: &Vec<VarUsage>, target_inputs: &Vec<VarUsage>| {
        new_blocks.push(Block {
            statements: vec![],
            end: BlockEnd::Goto(
                block_id,
                VarRemapping {
                    remapping: OrderedHashMap::from_iter(zip_eq(
                        target_inputs.iter().map(|var_usage| var_usage.var_id),
                        inputs.iter().cloned(),
                    )),
                },
            ),
        });

        let new_block_id = next_block_id;
        next_block_id = next_block_id.next_block_id();
        new_block_id
    };

    // Note that the loop below can't be merged with the loop above as a block might be marked as
    // dup after we already visiting an arm that goes to it.
    for block in lowered.blocks.iter_mut() {
        match &mut block.end {
            BlockEnd::Goto(target_block, remappings) => {
                let Some((block_id, target_inputs)) = duplicates.get(target_block) else {
                    continue;
                };

                let inputs = ctx.block_id_to_inputs.get(target_block).unwrap();
                let mut inputs_remapping = VarRemapping {
                    remapping: OrderedHashMap::from_iter(zip_eq(
                        target_inputs.iter().map(|var_usage| var_usage.var_id),
                        inputs.iter().cloned(),
                    )),
                };
                for (_, src) in inputs_remapping.iter_mut() {
                    if let Some(src_before_remapping) = remappings.get(&src.var_id) {
                        *src = *src_before_remapping;
                    }
                }

                *target_block = *block_id;
                *remappings = inputs_remapping;
            }
            BlockEnd::Match { info } => {
                for arm in info.arms_mut() {
                    let Some((block_id, target_inputs)) = duplicates.get(&arm.block_id) else {
                        continue;
                    };

                    let inputs = &ctx.block_id_to_inputs[&arm.block_id];
                    arm.block_id = new_goto_block(*block_id, inputs, target_inputs);
                }
            }
            _ => {}
        }
    }

    for block in new_blocks {
        lowered.blocks.push(block);
    }
}
