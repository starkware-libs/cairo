use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::context::LoweringContext;
use super::StructuredLowered;
use crate::{
    BlockId, Statement, StatementCall, StatementCallBlock, StatementEnumConstruct,
    StatementLiteral, StatementMatchEnum, StatementMatchExtern, StatementStructConstruct,
    StatementStructDestructure, StructuredBlock, StructuredBlockEnd, VariableId,
};

/// Inlines a function and returns the StructuredBlock that needs to be called.
pub fn inline_function(ctx: &mut LoweringContext<'_>, lowered: &StructuredLowered) -> BlockId {
    // TODO: check that the function has no ref arguments.

    let mut renamed_vars = HashMap::new();
    let mut rename_var = |old_var_id: &VariableId| match renamed_vars.entry(*old_var_id) {
        Entry::Occupied(e) => *e.get(),
        Entry::Vacant(e) => {
            let new_var_id = ctx.variables.alloc(lowered.variables[*old_var_id].clone());
            e.insert(new_var_id);
            new_var_id
        }
    };

    let root_block_id = lowered.root.unwrap();

    let mut renamed_blocks = HashMap::<BlockId, BlockId, _>::new();
    for (block_id, block) in lowered.blocks.iter() {
        let mut statements = vec![];
        for stmt in &block.statements {
            statements.push(match stmt {
                Statement::Literal(stmt) => Statement::Literal(StatementLiteral {
                    ty: stmt.ty,
                    value: stmt.value.clone(),
                    output: rename_var(&stmt.output),
                }),
                Statement::Call(stmt) => Statement::Call(StatementCall {
                    function: stmt.function,
                    inputs: stmt.inputs.iter().map(&mut rename_var).collect(),
                    outputs: stmt.outputs.iter().map(&mut rename_var).collect(),
                }),

                Statement::CallBlock(stmt) => Statement::CallBlock(StatementCallBlock {
                    block: stmt.block,
                    outputs: stmt.outputs.iter().map(&mut rename_var).collect(),
                }),
                Statement::MatchExtern(stmt) => Statement::MatchExtern(StatementMatchExtern {
                    function: stmt.function,
                    inputs: stmt.inputs.iter().map(&mut rename_var).collect(),
                    arms: stmt
                        .arms
                        .iter()
                        .map(|(concrete_variant, block_id)| {
                            (concrete_variant.clone(), renamed_blocks[block_id])
                        })
                        .collect(),
                    outputs: stmt.outputs.iter().map(&mut rename_var).collect(),
                }),
                Statement::StructConstruct(stmt) => {
                    Statement::StructConstruct(StatementStructConstruct {
                        inputs: stmt.inputs.iter().map(&mut rename_var).collect(),
                        output: rename_var(&stmt.output),
                    })
                }
                Statement::StructDestructure(stmt) => {
                    Statement::StructDestructure(StatementStructDestructure {
                        input: rename_var(&stmt.input),
                        outputs: stmt.outputs.iter().map(&mut rename_var).collect(),
                    })
                }
                Statement::EnumConstruct(stmt) => {
                    Statement::EnumConstruct(StatementEnumConstruct {
                        variant: stmt.variant.clone(),
                        input: rename_var(&stmt.input),
                        output: rename_var(&stmt.output),
                    })
                }
                Statement::MatchEnum(stmt) => Statement::MatchEnum(StatementMatchEnum {
                    concrete_enum: stmt.concrete_enum,
                    input: rename_var(&stmt.input),
                    arms: stmt
                        .arms
                        .iter()
                        .map(|(concrete_variant, block_id)| {
                            (concrete_variant.clone(), renamed_blocks[block_id])
                        })
                        .collect(),
                    outputs: stmt.outputs.iter().map(&mut rename_var).collect(),
                }),
            });
        }

        let end = match &block.end {
            StructuredBlockEnd::Callsite(_) => block.end.clone(),
            StructuredBlockEnd::Return { refs: _, returns } if block_id == root_block_id => {
                StructuredBlockEnd::Callsite(returns.to_vec())
            }
            _ => panic!("Unexpected block end."),
        };

        let new_block_block_id = ctx.blocks.alloc(StructuredBlock {
            inputs: block.inputs.iter().map(&mut rename_var).collect(),
            statements,
            end,
            initial_refs: block.initial_refs.clone(),
        });

        renamed_blocks.insert(block_id, new_block_block_id);
    }

    renamed_blocks[&root_block_id]
}
