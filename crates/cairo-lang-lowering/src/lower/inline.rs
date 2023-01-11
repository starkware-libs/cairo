use std::collections::HashMap;

use cairo_lang_defs::ids::{FreeFunctionId, LanguageElementId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::context::{LoweringContext, LoweringContextBuilder};
use super::StructuredLowered;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::InliningFailed;
use crate::lower::Maybe;
use crate::{
    BlockId, Statement, StatementCall, StatementCallBlock, StatementEnumConstruct,
    StatementLiteral, StatementMatchEnum, StatementMatchExtern, StatementStructConstruct,
    StatementStructDestructure, StructuredBlock, StructuredBlockEnd, StructuredStatement,
    VariableId,
};

// TODO(ilya): Add Rewritter trait.

// A rewriter that inlines functions annotated with #[inline].
pub struct FunctionInlinerRewritter<'db> {
    // The LoweringContext were we are building the new blocks.
    ctx: LoweringContext<'db>,
}

impl<'db> FunctionInlinerRewritter<'db> {
    fn rewrite(&mut self, statement: &StructuredStatement) -> Maybe<StructuredStatement> {
        if let Statement::Call(stmt) = &statement.statement {
            if let Some(free_function_id) =
                stmt.function.try_get_free_function_id(self.ctx.db.upcast())
            {
                let inline = self
                    .ctx
                    .db
                    .free_function_declaration_attributes(free_function_id)?
                    .iter()
                    .any(|attr| attr.id == "inline");

                if inline {
                    let block = self.inline_function(free_function_id)?;

                    return Ok(StructuredStatement {
                        statement: Statement::CallBlock(StatementCallBlock {
                            block,
                            outputs: stmt.outputs.clone(),
                        }),
                        ref_updates: statement.ref_updates.clone(),
                    });
                }
            }
        }
        Ok(statement.clone())
    }

    /// Inlines a function and returns the StructuredBlock that needs to be called.
    pub fn inline_function(&mut self, free_function_id: FreeFunctionId) -> Maybe<BlockId> {
        let lowered = self.ctx.db.free_function_lowered_structured(free_function_id)?;
        let root_block_id = lowered.root?;

        if !lowered.blocks[root_block_id].initial_refs.is_empty() {
            return Err(self.ctx.diagnostics.report(
                free_function_id.untyped_stable_ptr(self.ctx.db.upcast()),
                InliningFailed {
                    reason: "Cannot inline a function with ref arguments.".to_string(),
                },
            ));
        }

        let mut renamed_vars = HashMap::new();
        let mut renamed_blocks = HashMap::new();
        for (block_id, block) in lowered.blocks.iter() {
            let mut statements = vec![];

            let new_block = {
                let mut rename_var = |old_var_id: &VariableId| {
                    *renamed_vars
                        .entry(*old_var_id)
                        .or_insert_with(|| self.ctx.new_var(lowered.variables[*old_var_id].ty))
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
                            return Err(self.ctx.diagnostics.report(
                                free_function_id.untyped_stable_ptr(self.ctx.db.upcast()),
                                InliningFailed {
                                    reason: "Cannot inline a function an early return.".to_string(),
                                },
                            ));
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

            let new_block_block_id = self.ctx.blocks.alloc(new_block);
            renamed_blocks.insert(block_id, new_block_block_id);
        }

        Ok(renamed_blocks[&root_block_id])
    }

    fn apply(
        lowering_info: &'db LoweringContextBuilder<'db>,
        structured_lower: &StructuredLowered,
    ) -> Maybe<StructuredLowered> {
        let mut rewritter = Self { ctx: lowering_info.ctx()? };
        rewritter.ctx.variables = structured_lower.variables.clone();
        let orig_root = structured_lower.root?;
        let mut new_root = None;

        for (_block_id, block) in &structured_lower.blocks {
            let mut statements = vec![];
            for stmt in &block.statements {
                if let Ok(stmt) = rewritter.rewrite(stmt) {
                    statements.push(stmt);
                }
            }
            let new_block_id = rewritter.ctx.blocks.alloc(StructuredBlock {
                inputs: block.inputs.clone(),
                statements,
                end: block.end.clone(),
                initial_refs: block.initial_refs.clone(),
            });

            if _block_id == orig_root {
                new_root = Some(new_block_id)
            }
        }

        Ok(StructuredLowered {
            diagnostics: rewritter.ctx.diagnostics.build(),
            root: Ok(new_root.unwrap()),
            variables: rewritter.ctx.variables,
            blocks: rewritter.ctx.blocks,
        })
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    free_function_id: FreeFunctionId,
    structured_lower: &StructuredLowered,
) -> Maybe<StructuredLowered> {
    // let structured_lower = db.free_function_lowered_structured(free_function_id)?;

    let lowering_info = LoweringContextBuilder::new(db, free_function_id)?;
    FunctionInlinerRewritter::apply(&lowering_info, structured_lower)
}

// fn inline_functions(
//     db: &dyn LoweringGroup,
//     free_function_id: FreeFunctionId,
// ) -> Maybe<Arc<StructuredLowered>> {
//     let structured_lower = db.free_function_lowered_structured(free_function_id)?;

//     Ok(structured_lower)
// }

// Rebuilds the statement with renamed var and block ids.
fn rebuild_statement<F>(
    stmt: &StructuredStatement,
    mut rename_var: F,
    renamed_blocks: &HashMap<BlockId, BlockId>,
) -> StructuredStatement
where
    F: FnMut(&VariableId) -> VariableId,
{
    if !stmt.ref_updates.is_empty() {
        panic!("Cannot inline code with ref updates.")
    }
    let statement = match &stmt.statement {
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
    StructuredStatement { statement, ref_updates: OrderedHashMap::default() }
}
