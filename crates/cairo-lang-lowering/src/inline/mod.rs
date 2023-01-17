#[cfg(test)]
mod test;

use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_syntax::node::ast;
use itertools::izip;

use crate::blocks::FlatBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics};
use crate::lower::context::{LoweringContext, LoweringContextBuilder};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, StatementCall, StatementCallBlock,
    StatementEnumConstruct, StatementLiteral, StatementMatchEnum, StatementMatchExtern,
    StatementStructConstruct, StatementStructDestructure, VariableId,
};

#[derive(Debug, PartialEq, Eq)]
pub enum InlineConfiguration {
    // The user did not specify any inlining preferences.
    None,
    Always,
}

/// data about inlining.
#[derive(Debug, PartialEq, Eq)]
pub struct PrivInlineData {
    /// Diagnostics produced while collecting inlining Info.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    pub config: InlineConfiguration,
    pub is_inlineable: bool,
}

pub fn priv_inline_data(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<PrivInlineData>> {
    let mut diagnostics = LoweringDiagnostics::new(function_id.module_file(db.upcast()));
    let config = parse_inline_attribute(db, &mut diagnostics, function_id)?;

    // If the the function is marked as #[inline(always)], we need to report
    // inlining problems.
    let report_diagnostics = config == InlineConfiguration::Always;
    let is_inlineable = check_inlinable(db, &mut diagnostics, report_diagnostics, function_id)?;

    Ok(Arc::new(PrivInlineData { diagnostics: diagnostics.build(), config, is_inlineable }))
}

// Checks if the given function can be inlined.
// If report_diagnostics is true, adds a diagnostics with the reason that prevents inlining.
fn check_inlinable(
    db: &dyn LoweringGroup,
    diagnostics: &mut LoweringDiagnostics,
    report_diagnostics: bool,
    function_id: FunctionWithBodyId,
) -> Maybe<bool> {
    let defs_db = db.upcast();
    if db
            .function_with_body_direct_function_with_body_callees(function_id)?
            .contains(&function_id)
            // TODO(ilya): Relax requirement, if one of the functions is does not have
            //  #[inline(always)] than we can inline it.
            || db.function_with_body_scc(function_id).len() > 1
    {
        if report_diagnostics {
            diagnostics.report(
                function_id.untyped_stable_ptr(defs_db),
                LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
            );
        }
        return Ok(false);
    }

    let lowered = db.function_with_body_lowered_flat(function_id)?;
    let root_block_id = lowered.root?;
    for (block_id, block) in lowered.blocks.iter() {
        match block.end {
            FlatBlockEnd::Return { .. } if block_id != root_block_id => {
                if report_diagnostics {
                    diagnostics.report(
                        function_id.untyped_stable_ptr(defs_db),
                        LoweringDiagnosticKind::InliningFunctionWithEarlyReturnNotSupported,
                    );
                }
                return Ok(false);
            }
            _ => {}
        };
    }

    Ok(true)
}

// Parses the inline attributes for a given function.
fn parse_inline_attribute(
    db: &dyn LoweringGroup,
    diagnostics: &mut LoweringDiagnostics,
    function_id: FunctionWithBodyId,
) -> Maybe<InlineConfiguration> {
    let mut config = InlineConfiguration::None;
    let mut seen_inline_attr = false;
    for attr in db.function_with_body_attributes(function_id)?.iter() {
        if attr.id != "inline" {
            continue;
        }

        match &attr.args[..] {
            [ast::Expr::Path(path)] if &path.node.get_text(db.upcast()) == "always" => {
                config = InlineConfiguration::Always;
            }
            [] => {
                diagnostics.report(
                    attr.id_stable_ptr.untyped(),
                    LoweringDiagnosticKind::InlineWithoutArgumentNotSupported,
                );
            }
            _ => {
                diagnostics.report(
                    attr.args_stable_ptr.untyped(),
                    LoweringDiagnosticKind::UnsupportedInlineArguments,
                );
            }
        }

        if seen_inline_attr {
            diagnostics.report(
                attr.id_stable_ptr.untyped(),
                LoweringDiagnosticKind::RedundantInlineAttribute,
            );
            // If we have multiple inline attributes revert to InlineConfiguration::None.
            config = InlineConfiguration::None;
        }

        seen_inline_attr = true;
    }
    Ok(config)
}

// TODO(ilya): Add Rewriter trait.

/// A rewriter that inlines functions annotated with #[inline(always)].
pub struct FunctionInlinerRewriter<'db> {
    /// The LoweringContext were we are building the new blocks.
    ctx: LoweringContext<'db>,
    /// A Queue of structured blocks on which we want to apply the FunctionInlinerRewriter.
    block_queue: VecDeque<FlatBlock>,
    /// The new blocks that are were created during the inlining.
    flat_blocks: FlatBlocks,
}

impl<'db> FunctionInlinerRewriter<'db> {
    fn apply(ctx: LoweringContext<'db>, flat_lower: &FlatLowered) -> Maybe<FlatLowered> {
        let orig_root = flat_lower.root?;
        let mut rewriter = Self {
            ctx,
            block_queue: VecDeque::from(flat_lower.blocks.0.clone()),
            flat_blocks: FlatBlocks::new(),
        };

        rewriter.ctx.variables = flat_lower.variables.clone();
        while let Some(block) = rewriter.block_queue.pop_front() {
            let mut statements = vec![];
            for stmt in &block.statements {
                if let Ok(stmt) = rewriter.rewrite(stmt) {
                    statements.push(stmt);
                }
            }
            rewriter.flat_blocks.alloc(FlatBlock {
                inputs: block.inputs.clone(),
                statements,
                end: block.end.clone(),
            });
        }

        assert!(rewriter.ctx.diagnostics.build().is_empty());

        // TODO(ilya): Consider returning empty variables and blocks if there are diagnostics?
        Ok(FlatLowered {
            diagnostics: flat_lower.diagnostics.clone(),
            root: Ok(orig_root),
            variables: rewriter.ctx.variables,
            blocks: rewriter.flat_blocks,
        })
    }

    fn rewrite(&mut self, statement: &Statement) -> Maybe<Statement> {
        if let Statement::Call(stmt) = statement {
            if let Some(function_id) =
                stmt.function.try_get_function_with_body_id(self.ctx.db.upcast())
            {
                let inline_data = self.ctx.db.priv_inline_data(function_id)?;

                if inline_data.config == InlineConfiguration::Always && inline_data.is_inlineable {
                    let block = self.inline_function(function_id, &stmt.inputs)?;

                    return Ok(Statement::CallBlock(StatementCallBlock {
                        block,
                        outputs: stmt.outputs.clone(),
                    }));
                }
            }
        }
        Ok(statement.clone())
    }

    /// Enqueues the block for processing and returns the block_id that this
    /// block is going to get in self.ctx.blocks.
    fn enqueue_block(&mut self, block: FlatBlock) -> BlockId {
        self.block_queue.push_back(block);
        BlockId(self.flat_blocks.len() + self.block_queue.len())
    }

    /// Inlines the given function, with the given input variables.
    ///
    /// Returns the block_id that should be called instead of the function.
    pub fn inline_function(
        &mut self,
        function_id: FunctionWithBodyId,
        inputs: &[VariableId],
    ) -> Maybe<BlockId> {
        let lowered = self.ctx.db.function_with_body_lowered_flat(function_id)?;
        let root_block_id = lowered.root?;

        // As the block_ids and variable_ids are per function, we need to rename all
        // the blocks and variables before we enqueue the blocks from the function that
        // we are inlining.

        // The input variables need to be renamed to match the inputs to the function call.
        let mut renamed_vars = HashMap::<VariableId, VariableId>::from_iter(izip!(
            lowered.blocks[root_block_id].inputs.iter().cloned(),
            inputs.iter().cloned()
        ));
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
                FlatBlockEnd::Callsite(outputs) => {
                    FlatBlockEnd::Callsite(outputs.iter().map(&mut rename_var).collect())
                }
                FlatBlockEnd::Return(returns) => {
                    if block_id != root_block_id {
                        panic!("Cannot inline a function an early return.");
                    }

                    FlatBlockEnd::Callsite(returns.iter().map(&mut rename_var).collect())
                }
                FlatBlockEnd::Unreachable => FlatBlockEnd::Unreachable,
            };

            let inputs = if block_id == root_block_id {
                // If this is the root block we need to delete the inputs as they are
                // taken from the caller rather than created in this block.
                vec![]
            } else {
                block.inputs.iter().map(&mut rename_var).collect()
            };

            let new_block = FlatBlock { inputs, statements, end };
            renamed_blocks.insert(block_id, self.enqueue_block(new_block));
        }

        Ok(renamed_blocks[&root_block_id])
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    flat_lower: &FlatLowered,
) -> Maybe<FlatLowered> {
    let lowering_builder = LoweringContextBuilder::new(db, function_id)?;
    FunctionInlinerRewriter::apply(lowering_builder.ctx()?, flat_lower)
}

// Rebuilds the statement with renamed var and block ids.
fn rebuild_statement<F>(
    statement: &Statement,
    mut rename_var: F,
    renamed_blocks: &HashMap<BlockId, BlockId>,
) -> Statement
where
    F: FnMut(&VariableId) -> VariableId,
{
    match statement {
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
    }
}
