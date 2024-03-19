#[cfg(test)]
mod test;

mod statements_weights;

use std::collections::{HashMap, VecDeque};

use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic::items::functions::InlineConfiguration;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{izip, zip_eq, Itertools};
use statements_weights::InlineWeight;

use self::statements_weights::ApproxCasmInlineWeight;
use crate::blocks::{FlatBlocks, FlatBlocksBuilder};
use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics};
use crate::ids::{ConcreteFunctionWithBodyId, FunctionWithBodyId};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, VarRemapping, VarUsage, VariableId,
};

pub fn get_inline_diagnostics(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let semantic_function_id = function_id.base_semantic_function(db);
    let mut diagnostics = LoweringDiagnostics::new(
        semantic_function_id.module_file_id(db.upcast()).file_id(db.upcast())?,
    );

    if let InlineConfiguration::Always(_) =
        db.function_declaration_inline_config(semantic_function_id)?
    {
        if db.in_cycle(function_id, crate::DependencyType::Call)? {
            diagnostics.report(
                semantic_function_id.untyped_stable_ptr(db.upcast()),
                LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
            );
        }
    }

    Ok(diagnostics.build())
}

/// Query implementation of [LoweringGroup::priv_should_inline].
pub fn priv_should_inline(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    // Breaks cycles.
    // TODO(ilya): consider #[inline(never)] attributes for feedback set.
    if db.function_with_body_feedback_set(function_id)?.contains(&function_id) {
        return Ok(false);
    }

    let config = db.function_declaration_inline_config(
        function_id.function_with_body_id(db).base_semantic_function(db),
    )?;

    Ok(match config {
        InlineConfiguration::Never(_) => false,
        InlineConfiguration::Should(_) => true,
        InlineConfiguration::Always(_) => true,
        InlineConfiguration::None => should_inline_lowered(db, function_id)?,
    })
}

// A heuristic to decide if a function without an inline attribute should be inlined.
fn should_inline_lowered(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    let lowered = db.inlined_function_with_body_lowered(function_id)?;
    // The inline heuristics optimization flag only applies to non-trivial small functions.
    // Functions which contains only a call or a literal are always inlined.

    let weight_of_blocks = ApproxCasmInlineWeight::new(db, &lowered).lowered_weight(&lowered);

    if weight_of_blocks < inline_small_functions_threshold(db).into_or_panic() {
        return Ok(true);
    }

    let root_block = lowered.blocks.root_block()?;
    // The inline heuristics optimization flag only applies to non-trivial small functions.
    // Functions which contains only a call or a literal are always inlined.
    let num_of_statements: usize =
        lowered.blocks.iter().map(|(_, block)| block.statements.len()).sum();
    if num_of_statements < inline_small_functions_threshold(db) {
        return Ok(true);
    }

    Ok(match &root_block.end {
        FlatBlockEnd::Return(..) => {
            // Inline a function that only calls another function or returns a literal.
            matches!(root_block.statements.as_slice(), [Statement::Call(_) | Statement::Const(_)])
        }
        FlatBlockEnd::Goto(..) | FlatBlockEnd::Match { .. } | FlatBlockEnd::Panic(_) => false,
        FlatBlockEnd::NotSet => {
            panic!("Unexpected block end.");
        }
    })
}

// TODO(ilya): Add Rewriter trait.

/// A rewriter that inlines functions annotated with #[inline(always)].
pub struct FunctionInlinerRewriter<'db> {
    /// The LoweringContext were we are building the new blocks.
    variables: VariableAllocator<'db>,
    /// A Queue of blocks on which we want to apply the FunctionInlinerRewriter.
    block_queue: BlockQueue,
    /// rewritten statements.
    statements: Vec<Statement>,

    /// The end of the current block.
    block_end: FlatBlockEnd,
    /// The current block id.
    current_block_id: BlockId,
    /// stack for statements that require rewriting.
    statement_rewrite_stack: StatementStack,
    /// Indicates that the inlining process was successful.
    inlining_success: Maybe<()>,
    /// A map between blocks and the parent block that created them.
    block_to_parent: HashMap<BlockId, BlockId>,
    /// A map between blocks and the function that originally contained them.
    block_to_function: HashMap<BlockId, ConcreteFunctionWithBodyId>,
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
    fn push_statements(&mut self, statements: impl DoubleEndedIterator<Item = Statement>) {
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
    /// A Queue of blocks that require processing, and their id.
    block_queue: VecDeque<FlatBlock>,
    /// The new blocks that were created during the inlining.
    flat_blocks: FlatBlocksBuilder,
}
impl BlockQueue {
    /// Enqueues the block for processing and returns the block_id that this
    /// block is going to get in self.flat_blocks.
    fn enqueue_block(&mut self, block: FlatBlock) -> BlockId {
        self.block_queue.push_back(block);
        BlockId(self.flat_blocks.len() + self.block_queue.len())
    }
    // Pops a block from the queue.
    fn dequeue(&mut self) -> Option<FlatBlock> {
        self.block_queue.pop_front()
    }
    /// Finalizes a block.
    fn finalize(&mut self, block: FlatBlock) -> BlockId {
        self.flat_blocks.alloc(block)
    }
}
impl Default for BlockQueue {
    fn default() -> Self {
        Self { block_queue: Default::default(), flat_blocks: FlatBlocksBuilder::new() }
    }
}

/// Context for mapping ids from `lowered` to a new `FlatLowered` object.
pub struct Mapper<'a, 'b> {
    variables: &'a mut VariableAllocator<'b>,
    lowered: &'a FlatLowered,
    renamed_vars: HashMap<VariableId, VariableId>,
    return_block_id: BlockId,
    outputs: &'a [id_arena::Id<crate::Variable>],

    /// An offset that is added to all the block IDs in order to translate them into the new
    /// lowering representation.
    block_id_offset: BlockId,
}

impl<'a, 'b> Rebuilder for Mapper<'a, 'b> {
    /// Maps a var id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    /// If the variable wasn't assigned an id yet, a new id is assigned.
    fn map_var_id(&mut self, orig_var_id: VariableId) -> VariableId {
        *self.renamed_vars.entry(orig_var_id).or_insert_with(|| {
            self.variables.new_var(VarRequest {
                ty: self.lowered.variables[orig_var_id].ty,
                location: self.lowered.variables[orig_var_id].location,
            })
        })
    }

    /// Maps a block id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    fn map_block_id(&mut self, orig_block_id: BlockId) -> BlockId {
        BlockId(self.block_id_offset.0 + orig_block_id.0)
    }

    fn transform_end(&mut self, end: &mut FlatBlockEnd) {
        match end {
            FlatBlockEnd::Return(returns, _location) => {
                let remapping = VarRemapping {
                    remapping: OrderedHashMap::from_iter(zip_eq(
                        self.outputs.iter().cloned(),
                        returns.iter().cloned(),
                    )),
                };
                *end = FlatBlockEnd::Goto(self.return_block_id, remapping);
            }
            FlatBlockEnd::Panic(_) | FlatBlockEnd::Goto(_, _) | FlatBlockEnd::Match { .. } => {}
            FlatBlockEnd::NotSet => unreachable!(),
        }
    }
}

impl<'db> FunctionInlinerRewriter<'db> {
    fn apply(
        variables: VariableAllocator<'db>,
        flat_lower: &FlatLowered,
        calling_function_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<FlatLowered> {
        let mut rewriter = Self {
            variables,
            block_queue: BlockQueue {
                block_queue: VecDeque::from(flat_lower.blocks.get().clone()),
                flat_blocks: FlatBlocksBuilder::new(),
            },
            statements: vec![],
            block_end: FlatBlockEnd::NotSet,
            current_block_id: BlockId::root(),
            statement_rewrite_stack: StatementStack::default(),
            inlining_success: flat_lower.blocks.has_root(),
            block_to_parent: HashMap::new(),
            block_to_function: (0..flat_lower.blocks.len())
                .map(|i| (BlockId(i), calling_function_id))
                .collect(),
        };

        rewriter.variables.variables = flat_lower.variables.clone();
        while let Some(block) = rewriter.block_queue.dequeue() {
            rewriter.block_end = block.end;
            rewriter.statement_rewrite_stack.push_statements(block.statements.into_iter());

            while let Some(statement) = rewriter.statement_rewrite_stack.pop_statement() {
                rewriter.rewrite(statement)?;
            }

            rewriter.block_queue.finalize(FlatBlock {
                statements: std::mem::take(&mut rewriter.statements),
                end: rewriter.block_end,
            });
            rewriter.current_block_id = rewriter.current_block_id.next_block_id();
        }

        let blocks = rewriter
            .inlining_success
            .map(|()| rewriter.block_queue.flat_blocks.build().unwrap())
            .unwrap_or_else(FlatBlocks::new_errored);

        Ok(FlatLowered {
            diagnostics: flat_lower.diagnostics.clone(),
            variables: rewriter.variables.variables,
            blocks,
            parameters: flat_lower.parameters.clone(),
            signature: flat_lower.signature.clone(),
        })
    }

    /// Rewrites a statement and either appends it to self.statements or adds new statements to
    /// self.statements_rewrite_stack.
    fn rewrite(&mut self, statement: Statement) -> Maybe<()> {
        if let Statement::Call(ref stmt) = statement {
            if let Some(called_func) = stmt.function.body(self.variables.db)? {
                let orig_func = self.block_to_function[&BlockId::root()];

                // TODO: Implement better logic to avoid inlining of destructors that call
                // themselves.
                if called_func != orig_func
                    && orig_func == self.block_to_function[&self.current_block_id]
                    && self.variables.db.priv_should_inline(called_func)?
                {
                    return self.inline_function(called_func, &stmt.inputs, &stmt.outputs);
                }
            }
        }

        self.statements.push(statement);
        Ok(())
    }

    /// Inlines the given function, with the given input and output variables.
    /// The statements that need to replace the call statement in the original block
    /// are pushed into the `statement_rewrite_stack`.
    /// May also push additional blocks to the block queue.
    /// The function takes an optional return block id to handle early returns.
    pub fn inline_function(
        &mut self,
        function_id: ConcreteFunctionWithBodyId,
        inputs: &[VarUsage],
        outputs: &[VariableId],
    ) -> Maybe<()> {
        let lowered = self.variables.db.inlined_function_with_body_lowered(function_id)?;
        lowered.blocks.has_root()?;

        // Create a new block with all the statements that follow the call statement.
        let return_block_id = self.block_queue.enqueue_block(FlatBlock {
            statements: self.statement_rewrite_stack.consume(),
            end: self.block_end.clone(),
        });
        if let Some(parent_block_id) = self.block_to_parent.get(&self.current_block_id) {
            self.block_to_parent.insert(return_block_id, *parent_block_id);
        }
        self.block_to_function
            .insert(return_block_id, self.block_to_function[&self.current_block_id]);

        // As the block_ids and variable_ids are per function, we need to rename all
        // the blocks and variables before we enqueue the blocks from the function that
        // we are inlining.

        // The input variables need to be renamed to match the inputs to the function call.
        let renamed_vars = HashMap::<VariableId, VariableId>::from_iter(izip!(
            lowered.parameters.iter().cloned(),
            inputs.iter().map(|var_usage| var_usage.var_id)
        ));

        let mut mapper = Mapper {
            variables: &mut self.variables,
            lowered: &lowered,
            renamed_vars,
            block_id_offset: BlockId(return_block_id.0 + 1),
            return_block_id,
            outputs,
        };

        // The current block should Goto to the root block of the inlined function.
        // Note that we can't remap the inputs as they might be used after we return
        // from the inlined function.
        // TODO(ilya): Try to use var remapping instead of renaming for the inputs to
        // keep track of the correct Variable.location.
        self.block_end =
            FlatBlockEnd::Goto(mapper.map_block_id(BlockId::root()), VarRemapping::default());

        for (block_id, block) in lowered.blocks.iter() {
            let block = mapper.rebuild_block(block);

            let new_block_id = self.block_queue.enqueue_block(block);
            assert_eq!(mapper.map_block_id(block_id), new_block_id, "Unexpected block_id.");
            self.block_to_parent.insert(new_block_id, self.current_block_id);
            self.block_to_function.insert(new_block_id, function_id);
        }

        Ok(())
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    flat_lowered: &mut FlatLowered,
) -> Maybe<()> {
    let function_with_body_id = function_id.function_with_body_id(db);
    let variables = VariableAllocator::new(
        db,
        function_with_body_id.base_semantic_function(db),
        flat_lowered.variables.clone(),
    )?;
    if let Ok(new_flat_lowered) =
        FunctionInlinerRewriter::apply(variables, flat_lowered, function_id)
    {
        *flat_lowered = new_flat_lowered;
    }
    Ok(())
}

/// Returns the threshold, in number of lowering statements, below which a function is marked as
/// `should_inline`.
fn inline_small_functions_threshold(db: &dyn LoweringGroup) -> usize {
    db.optimization_config().inline_small_functions_threshold
}
