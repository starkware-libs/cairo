use std::collections::HashMap;

use cairo_lang_semantic as semantic;

use super::context::{LoweringContext, LoweringFlowError};
use super::StructuredLowered;
use crate::diagnostic::LoweringDiagnosticKind::InliningFailed;
use crate::{
    BlockId, Statement, StatementCall, StatementCallBlock, StatementEnumConstruct,
    StatementLiteral, StatementMatchEnum, StatementMatchExtern, StatementStructConstruct,
    StatementStructDestructure, StructuredBlock, StructuredBlockEnd, VariableId,
};

/// Inlines a function and returns the StructuredBlock that needs to be called.
pub fn inline_function(
    ctx: &mut LoweringContext<'_>,
    expr: &semantic::ExprFunctionCall,
    lowered: &StructuredLowered,
) -> Result<BlockId, LoweringFlowError> {
    let root_block_id = lowered.root.map_err(LoweringFlowError::Failed)?;

    let mut renamed_vars = HashMap::new();

    let mut renamed_blocks = HashMap::new();
    for (block_id, block) in lowered.blocks.iter() {
        let mut statements = vec![];

        let new_block = {
            let mut rename_var = |old_var_id: &VariableId| {
                *renamed_vars
                    .entry(*old_var_id)
                    .or_insert_with(|| ctx.new_var(lowered.variables[*old_var_id].ty))
            };
            for stmt in &block.statements {
                statements.push(rebuild_statement(stmt, &mut rename_var, &renamed_blocks));
            }

            let end = match &block.end {
                StructuredBlockEnd::Callsite(outputs) => {
                    StructuredBlockEnd::Callsite(outputs.iter().map(&mut rename_var).collect())
                }
                StructuredBlockEnd::Return { refs, returns } => {
                    if block_id != root_block_id {
                        return Err(LoweringFlowError::Failed(ctx.diagnostics.report(
                            expr.stable_ptr.untyped(),
                            InliningFailed {
                                reason: "Cannot inline a function with ref arguments.".to_string(),
                            },
                        )));
                    }
                    if !refs.is_empty() {
                        panic!("Can't inline funcitons with refs.")
                    }

                    StructuredBlockEnd::Callsite(returns.iter().map(&mut rename_var).collect())
                }
                _ => panic!("Unexpected block end."),
            };

            StructuredBlock {
                inputs: block.inputs.iter().map(&mut rename_var).collect(),
                statements,
                end,
                initial_refs: block.initial_refs.clone(),
            }
        };

        let new_block_block_id = ctx.blocks.alloc(new_block);

        renamed_blocks.insert(block_id, new_block_block_id);
    }

    Ok(renamed_blocks[&root_block_id])
}

// Rebuilds the statement with renamed var and block ids.
fn rebuild_statement<F>(
    stmt: &Statement,
    mut rename_var: F,
    renamed_blocks: &HashMap<BlockId, BlockId>,
) -> Statement
where
    F: FnMut(&VariableId) -> VariableId,
{
    let stmt = match stmt {
        Statement::Literal(stmt) => Statement::Literal(StatementLiteral {
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
        Statement::StructConstruct(stmt) => Statement::StructConstruct(StatementStructConstruct {
            inputs: stmt.inputs.iter().map(&mut rename_var).collect(),
            output: rename_var(&stmt.output),
        }),
        Statement::StructDestructure(stmt) => {
            Statement::StructDestructure(StatementStructDestructure {
                input: rename_var(&stmt.input),
                outputs: stmt.outputs.iter().map(&mut rename_var).collect(),
            })
        }
        Statement::EnumConstruct(stmt) => Statement::EnumConstruct(StatementEnumConstruct {
            variant: stmt.variant.clone(),
            input: rename_var(&stmt.input),
            output: rename_var(&stmt.output),
        }),
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
    };
    stmt
}
