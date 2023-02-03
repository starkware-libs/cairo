#[cfg(test)]
mod test;

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::{skip_diagnostic, Diagnostics, Maybe};
use cairo_lang_semantic::ConcreteFunctionWithBodyId;
use cairo_lang_syntax::node::ast;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, izip, Itertools};

use crate::blocks::FlatBlocks;
use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics};
use crate::lower::context::{LoweringContext, LoweringContextBuilder, VarRequest};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, StatementCall,
    StatementEnumConstruct, StatementLiteral, StatementMatchEnum, StatementMatchExtern,
    StatementStructConstruct, StatementStructDestructure, VarRemapping, VariableId,
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
    pub info: InlineInfo,
}

/// Per function information for the inlining phase.
#[derive(Debug, PartialEq, Eq)]
pub struct InlineInfo {
    pub is_inlineable: bool,
    pub has_early_return: bool,
}

pub fn priv_inline_data(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<PrivInlineData>> {
    let mut diagnostics = LoweringDiagnostics::new(function_id.module_file_id(db.upcast()));
    let config = parse_inline_attribute(db, &mut diagnostics, function_id)?;

    // If the the function is marked as #[inline(always)], we need to report
    // inlining problems.
    let report_diagnostics = config == InlineConfiguration::Always;
    let info = gather_inlining_info(db, &mut diagnostics, report_diagnostics, function_id)?;

    Ok(Arc::new(PrivInlineData { diagnostics: diagnostics.build(), config, info }))
}

/// Gathers inlining information for the given function.
/// If report_diagnostics is true, adds a diagnostics with the reason that prevents inlining.
fn gather_inlining_info(
    db: &dyn LoweringGroup,
    diagnostics: &mut LoweringDiagnostics,
    report_diagnostics: bool,
    function_id: FunctionWithBodyId,
) -> Maybe<InlineInfo> {
    let mut info = InlineInfo { is_inlineable: false, has_early_return: false };
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
        return Ok(info);
    }

    let lowered = db.priv_function_with_body_lowered_flat(function_id)?;
    let root_block_id = lowered.root?;
    let input_vars: HashSet<VariableId> =
        lowered.blocks[root_block_id].inputs.iter().copied().collect();
    for (block_id, block) in lowered.blocks.iter() {
        match &block.end {
            FlatBlockEnd::Return { .. } if block_id != root_block_id => {
                if report_diagnostics {
                    diagnostics.report(
                        function_id.untyped_stable_ptr(defs_db),
                        LoweringDiagnosticKind::InliningFunctionWithEarlyReturnNotSupported,
                    );
                }

                info.has_early_return = true;
            }
            FlatBlockEnd::Return(returns) => {
                if returns.iter().any(|r| input_vars.contains(r)) {
                    if report_diagnostics {
                        diagnostics.report(
                            function_id.untyped_stable_ptr(defs_db),
                            LoweringDiagnosticKind::InliningFunctionWithIdentityVarsNotSupported,
                        );
                    }
                    return Ok(info);
                }
            }
            FlatBlockEnd::Unreachable => {
                // TODO(ilya): Remove the following limitation.
                if block_id == root_block_id {
                    if report_diagnostics {
                        diagnostics.report(
                            function_id.untyped_stable_ptr(defs_db),
                            LoweringDiagnosticKind::InliningFunctionWithUnreachableEndNotSupported,
                        );
                    }
                    return Ok(info);
                }
            }
            FlatBlockEnd::Callsite(_) | FlatBlockEnd::Fallthrough(..) | FlatBlockEnd::Goto(..) => {
                if block_id == root_block_id {
                    panic!("Unexpected block end.");
                }
            }
        };
    }

    info.is_inlineable = true;
    Ok(info)
}

/// Parses the inline attributes for a given function.
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
    /// A Queue of blocks on which we want to apply the FunctionInlinerRewriter.
    block_queue: BlockQueue,
    /// rewritten statements.
    statements: Vec<Statement>,

    /// The end of the current block.
    block_end: FlatBlockEnd,
    /// stack for statements that require rewriting.
    statement_rewrite_stack: StatementStack,
    /// Indicates that there was an error during inlining.
    inlining_failed: bool,
}

#[derive(Default)]
pub struct StatementStack {
    stack: Vec<Statement>,
}

impl StatementStack {
    /// Pushes multiple statement into the stack.
    ///
    /// Note that to keep the order of the statements when they are popped from the stack
    /// they need to be pushed in reverse order.
    fn push_statments(&mut self, statements: impl DoubleEndedIterator<Item = Statement>) {
        self.stack.extend(statements.rev());
    }

    // Consumes all the statements in the stack.
    fn consume(&mut self) -> Vec<Statement> {
        self.stack.drain(..).rev().collect_vec()
    }

    fn pop_statement(&mut self) -> Option<Statement> {
        self.stack.pop()
    }
}

pub struct BlockQueue {
    /// A Queue of blocks that require processing.
    block_queue: VecDeque<FlatBlock>,
    /// The new blocks that are were created during the inlining.
    flat_blocks: FlatBlocks,
}
impl BlockQueue {
    /// Enqueues the block for processing and returns the block_id that this
    /// block is going to get in self.flat_blocks.
    fn enqueue_block(&mut self, block: FlatBlock) -> BlockId {
        self.block_queue.push_back(block);
        BlockId(self.flat_blocks.len() + self.block_queue.len())
    }
    //. Pops a block from the queue.
    fn dequeue(&mut self) -> Option<FlatBlock> {
        self.block_queue.pop_front()
    }
    /// Finalizes a block.
    fn finalize(&mut self, block: FlatBlock) -> BlockId {
        self.flat_blocks.alloc(block)
    }
}

/// Context for mapping ids from `lowered` to a new `FlatLowered` object.
pub struct Mapper<'a, 'b> {
    ctx: &'a mut LoweringContext<'b>,
    lowered: &'a FlatLowered,

    renamed_vars: HashMap<VariableId, VariableId>,
    renamed_blocks: HashMap<BlockId, BlockId>,
}
impl<'a, 'b> Mapper<'a, 'b> {
    /// Renames a var from lowered.variable, if the variable wasn't assigned an id yet,
    /// a new id is assigned and stored for future renames.
    pub fn rename_var(&mut self, old_var_id: &VariableId) -> VariableId {
        *self.renamed_vars.entry(*old_var_id).or_insert_with(|| {
            self.ctx.new_var(VarRequest {
                ty: self.lowered.variables[*old_var_id].ty,
                location: self.lowered.variables[*old_var_id].location,
            })
        })
    }

    /// Apply rename_var to all the variable in the `remapping`.
    pub fn update_remapping(&mut self, remapping: &VarRemapping) -> VarRemapping {
        VarRemapping {
            remapping: OrderedHashMap::from_iter(
                remapping.iter().map(|(dst, src)| (self.rename_var(dst), self.rename_var(src))),
            ),
        }
    }

    /// Rebuilds the statement with renamed var and block ids.
    fn rebuild_statement(&mut self, statement: &Statement) -> Statement {
        match statement {
            Statement::Literal(stmt) => Statement::Literal(StatementLiteral {
                value: stmt.value.clone(),
                output: self.rename_var(&stmt.output),
            }),
            Statement::Call(stmt) => Statement::Call(StatementCall {
                function: stmt.function,
                inputs: stmt.inputs.iter().map(|v| self.rename_var(v)).collect(),
                outputs: stmt.outputs.iter().map(|v| self.rename_var(v)).collect(),
            }),
            Statement::MatchExtern(stmt) => Statement::MatchExtern(StatementMatchExtern {
                function: stmt.function,
                inputs: stmt.inputs.iter().map(|v| self.rename_var(v)).collect(),
                arms: stmt
                    .arms
                    .iter()
                    .map(|(concrete_variant, block_id)| {
                        (concrete_variant.clone(), self.renamed_blocks[block_id])
                    })
                    .collect(),
            }),
            Statement::StructConstruct(stmt) => {
                Statement::StructConstruct(StatementStructConstruct {
                    inputs: stmt.inputs.iter().map(|v| self.rename_var(v)).collect(),
                    output: self.rename_var(&stmt.output),
                })
            }
            Statement::StructDestructure(stmt) => {
                Statement::StructDestructure(StatementStructDestructure {
                    input: self.rename_var(&stmt.input),
                    outputs: stmt.outputs.iter().map(|v| self.rename_var(v)).collect(),
                })
            }
            Statement::EnumConstruct(stmt) => Statement::EnumConstruct(StatementEnumConstruct {
                variant: stmt.variant.clone(),
                input: self.rename_var(&stmt.input),
                output: self.rename_var(&stmt.output),
            }),
            Statement::MatchEnum(stmt) => Statement::MatchEnum(StatementMatchEnum {
                concrete_enum_id: stmt.concrete_enum_id,
                input: self.rename_var(&stmt.input),
                arms: stmt
                    .arms
                    .iter()
                    .map(|(concrete_variant, block_id)| {
                        (concrete_variant.clone(), self.renamed_blocks[block_id])
                    })
                    .collect(),
            }),
        }
    }
}

impl<'db> FunctionInlinerRewriter<'db> {
    fn apply(ctx: LoweringContext<'db>, flat_lower: &FlatLowered) -> Maybe<FlatLowered> {
        let orig_root = flat_lower.root?;
        let mut rewriter = Self {
            ctx,
            block_queue: BlockQueue {
                block_queue: VecDeque::from(flat_lower.blocks.0.clone()),
                flat_blocks: FlatBlocks::new(),
            },
            statements: vec![],
            block_end: FlatBlockEnd::Unreachable,
            statement_rewrite_stack: StatementStack::default(),
            inlining_failed: false,
        };

        rewriter.ctx.variables = flat_lower.variables.clone();
        while let Some(block) = rewriter.block_queue.dequeue() {
            rewriter.block_end = block.end;
            rewriter.statement_rewrite_stack.push_statments(block.statements.into_iter());

            while let Some(statement) = rewriter.statement_rewrite_stack.pop_statement() {
                rewriter.rewrite(statement)?;
            }

            rewriter.block_queue.finalize(FlatBlock {
                inputs: block.inputs,
                statements: std::mem::take(&mut rewriter.statements),
                end: rewriter.block_end,
            });
        }

        let root = if rewriter.inlining_failed { Err(skip_diagnostic()) } else { Ok(orig_root) };

        assert!(rewriter.ctx.diagnostics.build().is_empty());
        Ok(FlatLowered {
            diagnostics: flat_lower.diagnostics.clone(),
            root,
            variables: rewriter.ctx.variables,
            blocks: rewriter.block_queue.flat_blocks,
        })
    }

    /// Rewrites statement and either appends it to self.statements or adds new statements to
    /// self.statements_rewrite_stack.
    fn rewrite(&mut self, statement: Statement) -> Maybe<()> {
        if let Statement::Call(ref stmt) = statement {
            let concrete_function = self.ctx.db.lookup_intern_function(stmt.function).function;
            let semantic_db = self.ctx.db.upcast();
            if let Some(function_id) = concrete_function.get_body(semantic_db) {
                let inline_data =
                    self.ctx.db.priv_inline_data(function_id.function_with_body_id(semantic_db))?;

                if !inline_data.diagnostics.is_empty() {
                    self.inlining_failed = true;
                }

                if inline_data.config == InlineConfiguration::Always
                    && inline_data.info.is_inlineable
                {
                    let optional_return_block_id = if inline_data.info.has_early_return {
                        // if the inlined function has an early return then we need to split the
                        // current block after the call instruction.

                        // Create a new block with all the instructions that follow the call
                        // instruction.
                        let block_id = self.block_queue.enqueue_block(FlatBlock {
                            inputs: vec![],
                            statements: self.statement_rewrite_stack.consume(),
                            end: self.block_end.clone(),
                        });

                        // Once the current block ends it should fallthrough to the above block.
                        self.block_end = FlatBlockEnd::Fallthrough(
                            block_id,
                            VarRemapping { remapping: OrderedHashMap::default() },
                        );
                        Some(block_id)
                    } else {
                        None
                    };

                    return self.inline_function(
                        function_id,
                        &stmt.inputs,
                        &stmt.outputs,
                        optional_return_block_id,
                    );
                }
            }
        }

        self.statements.push(statement);
        Ok(())
    }

    /// Inlines the given function, with the given input and outputs variables.
    /// The statements that needs to replace the call statement in the original block
    /// are pushed into the statement_rewrite_stack.
    /// May also push additional blocks to the block queue.
    /// The function takes an optional return block id to handle early returns.
    pub fn inline_function(
        &mut self,
        function_id: ConcreteFunctionWithBodyId,
        inputs: &[VariableId],
        outputs: &[VariableId],
        optional_return_block_id: Option<BlockId>,
    ) -> Maybe<()> {
        let lowered = self.ctx.db.priv_concrete_function_with_body_lowered_flat(function_id)?;
        let root_block_id = lowered.root?;
        let root_block = &lowered.blocks[root_block_id];

        // As the block_ids and variable_ids are per function, we need to rename all
        // the blocks and variables before we enqueue the blocks from the function that
        // we are inlining.

        // The input variables need to be renamed to match the inputs to the function call.
        // The returned variables needs to be renamed to match the outputs from the function call.
        let mut mapper = Mapper {
            ctx: &mut self.ctx,
            lowered: &lowered,
            renamed_vars: HashMap::<VariableId, VariableId>::from_iter(chain!(
                izip!(lowered.blocks[root_block_id].inputs.iter().cloned(), inputs.iter().cloned()),
                izip!(
                    extract_matches!(&root_block.end, FlatBlockEnd::Return).iter().cloned(),
                    outputs.iter().cloned()
                )
            )),
            renamed_blocks: HashMap::new(),
        };

        for (block_id, block) in lowered.blocks.iter() {
            if block_id == root_block_id {
                continue;
            }
            let mut statements = vec![];

            for stmt in &block.statements {
                statements.push(mapper.rebuild_statement(stmt));
            }
            let end = match &block.end {
                FlatBlockEnd::Callsite(remapping) => {
                    FlatBlockEnd::Callsite(mapper.update_remapping(remapping))
                }
                FlatBlockEnd::Return(returns) => FlatBlockEnd::Goto(
                    optional_return_block_id.unwrap(),
                    VarRemapping {
                        remapping: OrderedHashMap::from_iter(izip!(
                            outputs.iter().cloned(),
                            returns.iter().map(|var_id| mapper.rename_var(var_id)),
                        )),
                    },
                ),

                FlatBlockEnd::Unreachable => FlatBlockEnd::Unreachable,
                FlatBlockEnd::Fallthrough(block_id, remapping)
                | FlatBlockEnd::Goto(block_id, remapping) => FlatBlockEnd::Goto(
                    mapper.renamed_blocks[block_id],
                    mapper.update_remapping(remapping),
                ),
            };

            let inputs = block.inputs.iter().map(|v| mapper.rename_var(v)).collect();
            let new_block = FlatBlock { inputs, statements, end };
            mapper.renamed_blocks.insert(block_id, self.block_queue.enqueue_block(new_block));
        }

        self.statement_rewrite_stack.push_statments(
            root_block.statements.iter().map(|statement| mapper.rebuild_statement(statement)),
        );
        Ok(())
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
    flat_lowered: &mut FlatLowered,
) -> Maybe<()> {
    let lowering_builder = LoweringContextBuilder::new(db, function_id)?;
    if let Ok(new_flat_lowered) =
        FunctionInlinerRewriter::apply(lowering_builder.ctx()?, flat_lowered)
    {
        *flat_lowered = new_flat_lowered;
    }
    Ok(())
}
