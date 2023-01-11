use std::collections::{HashMap, VecDeque};

use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::DiagnosticAdded;
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

// TODO(ilya): Add Rewriter trait.

// A rewriter that inlines functions annotated with #[inline].
pub struct FunctionInlinerRewriter<'db> {
    // The LoweringContext were we are building the new blocks.
    ctx: LoweringContext<'db>,

    // A Queue of structured blocks on which we want to apply the FunctionInlinerRewriter.
    block_queue: VecDeque<StructuredBlock>,
}

impl<'db> FunctionInlinerRewriter<'db> {
    fn rewrite(&mut self, statement: &StructuredStatement) -> Maybe<StructuredStatement> {
        if let Statement::Call(stmt) = &statement.statement {
            if let Some(free_function_id) =
                stmt.function.try_get_free_function_id(self.ctx.db.upcast())
            {
                let inline = self
                    .ctx
                    .db
                    .function_with_body_attributes(FunctionWithBodyId::Free(free_function_id))?
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

    /// Enqueue the block for processing and return the block_id that this
    /// block is going to get in self.ctx.blocks.
    fn enqueue_block(&mut self, block: StructuredBlock) -> BlockId {
        self.block_queue.push_back(block);
        BlockId(self.ctx.blocks.len() + self.block_queue.len())
    }

    /// Inlines a function and returns the block_id that should be called instead of the function.
    pub fn inline_function(&mut self, free_function_id: FreeFunctionId) -> Maybe<BlockId> {
        let lowered = self
            .ctx
            .db
            .function_with_body_lowered_structured(FunctionWithBodyId::Free(free_function_id))?;
        let root_block_id = lowered.root?;

        if !lowered.blocks[root_block_id].initial_refs.is_empty() {
            return Err(self.ctx.diagnostics.report(
                free_function_id.untyped_stable_ptr(self.ctx.db.upcast()),
                InliningFailed {
                    reason: "Cannot inline a function with implicit/ref parameters.".to_string(),
                },
            ));
        }

        // As the block_ids and variable_ids are per function, we need to rename all
        // the blocks and variables before we enqueue the blocks from the function that
        // we are inlining.
        let mut renamed_vars = HashMap::new();
        let mut renamed_blocks = HashMap::new();
        for (block_id, block) in lowered.blocks.iter() {
            let mut statements = vec![];

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
                        panic!("Can't inline function with refs.")
                    }

                    StructuredBlockEnd::Callsite(returns.iter().map(&mut rename_var).collect())
                }
                _ => panic!("Unexpected block end."),
            };

            let new_block = StructuredBlock {
                inputs: block.inputs.iter().map(&mut rename_var).collect(),
                statements,
                end,
                initial_refs: block.initial_refs.clone(),
            };
            renamed_blocks.insert(block_id, self.enqueue_block(new_block));
        }

        Ok(renamed_blocks[&root_block_id])
    }

    fn apply(
        lowering_info: &'db LoweringContextBuilder<'db>,
        structured_lower: &StructuredLowered,
    ) -> Maybe<StructuredLowered> {
        let orig_root = structured_lower.root?;
        let mut rewriter = Self {
            ctx: lowering_info.ctx()?,
            block_queue: VecDeque::from(structured_lower.blocks.0.clone()),
        };

        rewriter.ctx.variables = structured_lower.variables.clone();
        while let Some(block) = rewriter.block_queue.pop_front() {
            let mut statements = vec![];
            for stmt in &block.statements {
                if let Ok(stmt) = rewriter.rewrite(stmt) {
                    statements.push(stmt);
                }
            }
            rewriter.ctx.blocks.alloc(StructuredBlock {
                inputs: block.inputs.clone(),
                statements,
                end: block.end.clone(),
                initial_refs: block.initial_refs.clone(),
            });
        }

        let diagnostics = rewriter.ctx.diagnostics.build();
        let root = if diagnostics.is_empty() { Ok(orig_root) } else { Err(DiagnosticAdded) };

        // TODO(ilya): Consider Returning empty variables and blocks if there are diagnostics?
        Ok(StructuredLowered {
            diagnostics,
            root,
            variables: rewriter.ctx.variables,
            blocks: rewriter.ctx.blocks,
        })
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    structured_lower: &StructuredLowered,
) -> Maybe<StructuredLowered> {
    let lowering_info = LoweringContextBuilder::new(db, function_id)?;
    FunctionInlinerRewriter::apply(&lowering_info, structured_lower)
}

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
