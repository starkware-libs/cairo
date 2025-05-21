#[cfg(test)]
mod test;

pub mod statements_weights;

use std::collections::{HashMap, VecDeque};

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic::items::functions::InlineConfiguration;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{izip, zip_eq};

use crate::blocks::{Blocks, BlocksBuilder};
use crate::db::LoweringGroup;
use crate::diagnostic::{
    LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics, LoweringDiagnosticsBuilder,
};
use crate::ids::{
    ConcreteFunctionWithBodyId, FunctionWithBodyId, FunctionWithBodyLongId, LocationId,
};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::utils::{InliningStrategy, Rebuilder, RebuilderEx};
use crate::{
    Block, BlockEnd, BlockId, DependencyType, Lowered, LoweringStage, Statement, StatementCall,
    VarRemapping, VariableId,
};

pub fn get_inline_diagnostics(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Diagnostics<LoweringDiagnostic>> {
    let inline_config = match function_id.lookup_intern(db) {
        FunctionWithBodyLongId::Semantic(id) => db.function_declaration_inline_config(id)?,
        FunctionWithBodyLongId::Generated { .. } => InlineConfiguration::None,
    };
    let mut diagnostics = LoweringDiagnostics::default();

    if let InlineConfiguration::Always(_) = inline_config {
        if db.in_cycle(function_id, crate::DependencyType::Call)? {
            diagnostics.report(
                function_id.base_semantic_function(db).untyped_stable_ptr(db),
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
    if db.concrete_in_cycle(function_id, DependencyType::Call, LoweringStage::Monomorphized)? {
        return Ok(false);
    }

    let config = db.function_declaration_inline_config(
        function_id.base_semantic_function(db).function_with_body_id(db),
    )?;
    match (db.optimization_config().inlining_strategy, config) {
        (_, InlineConfiguration::Always(_)) => Ok(true),
        (InliningStrategy::Avoid, _) | (_, InlineConfiguration::Never(_)) => Ok(false),
        (_, InlineConfiguration::Should(_)) => Ok(true),
        (InliningStrategy::Default, InlineConfiguration::None) => {
            /// The default threshold for inlining small functions. Decided according to sample
            /// contracts profiling.
            const DEFAULT_INLINE_SMALL_FUNCTIONS_THRESHOLD: usize = 120;
            should_inline_lowered(db, function_id, DEFAULT_INLINE_SMALL_FUNCTIONS_THRESHOLD)
        }
        (InliningStrategy::InlineSmallFunctions(threshold), InlineConfiguration::None) => {
            should_inline_lowered(db, function_id, threshold)
        }
    }
}

// A heuristic to decide if a function without an inline attribute should be inlined.
fn should_inline_lowered(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    inline_small_functions_threshold: usize,
) -> Maybe<bool> {
    let weight_of_blocks = db.estimate_size(function_id)?;
    Ok(weight_of_blocks < inline_small_functions_threshold.into_or_panic())
}

// TODO(ilya): Add Rewriter trait.

/// A rewriter that inlines functions annotated with #[inline(always)].
pub struct FunctionInlinerRewriter<'db> {
    /// The LoweringContext where we are building the new blocks.
    variables: VariableAllocator<'db>,
    /// A Queue of blocks on which we want to apply the FunctionInlinerRewriter.
    block_queue: BlockRewriteQueue,
    /// rewritten statements.
    statements: Vec<Statement>,

    /// The end of the current block.
    block_end: BlockEnd,
    /// The processed statements of the current block.
    unprocessed_statements: <Vec<Statement> as IntoIterator>::IntoIter,
    /// Indicates that the inlining process was successful.
    inlining_success: Maybe<()>,
    /// The id of the function calling the possibly inlined functions.
    calling_function_id: ConcreteFunctionWithBodyId,
}

pub struct BlockRewriteQueue {
    /// A Queue of blocks that require processing, and their id.
    block_queue: VecDeque<(Block, bool)>,
    /// The new blocks that were created during the inlining.
    blocks: BlocksBuilder,
}
impl BlockRewriteQueue {
    /// Enqueues the block for processing and returns the block_id that this
    /// block is going to get in self.blocks.
    fn enqueue_block(&mut self, block: Block, requires_rewrite: bool) -> BlockId {
        self.block_queue.push_back((block, requires_rewrite));
        BlockId(self.blocks.len() + self.block_queue.len())
    }
    /// Pops a block requiring rewrites from the queue.
    /// If the block doesn't require rewrites, it is finalized and added to the blocks.
    fn dequeue(&mut self) -> Option<Block> {
        while let Some((block, requires_rewrite)) = self.block_queue.pop_front() {
            if requires_rewrite {
                return Some(block);
            }
            self.finalize(block);
        }
        None
    }
    /// Finalizes a block.
    fn finalize(&mut self, block: Block) {
        self.blocks.alloc(block);
    }
}

/// Context for mapping ids from `lowered` to a new `Lowered` object.
pub struct Mapper<'a, 'b> {
    variables: &'a mut VariableAllocator<'b>,
    lowered: &'a Lowered,
    renamed_vars: HashMap<VariableId, VariableId>,
    return_block_id: BlockId,
    outputs: &'a [id_arena::Id<crate::Variable>],
    inlining_location: StableLocation,

    /// An offset that is added to all the block IDs in order to translate them into the new
    /// lowering representation.
    block_id_offset: BlockId,
}

impl Rebuilder for Mapper<'_, '_> {
    /// Maps a var id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    /// If the variable wasn't assigned an id yet, a new id is assigned.
    fn map_var_id(&mut self, orig_var_id: VariableId) -> VariableId {
        *self.renamed_vars.entry(orig_var_id).or_insert_with(|| {
            self.variables.new_var(VarRequest {
                ty: self.lowered.variables[orig_var_id].ty,
                location: self.lowered.variables[orig_var_id]
                    .location
                    .inlined(self.variables.db, self.inlining_location),
            })
        })
    }

    /// Maps a block id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    fn map_block_id(&mut self, orig_block_id: BlockId) -> BlockId {
        BlockId(self.block_id_offset.0 + orig_block_id.0)
    }

    /// Adds the inlining location to a location.
    fn map_location(&mut self, location: LocationId) -> LocationId {
        location.inlined(self.variables.db, self.inlining_location)
    }

    fn transform_end(&mut self, end: &mut BlockEnd) {
        match end {
            BlockEnd::Return(returns, _location) => {
                let remapping = VarRemapping {
                    remapping: OrderedHashMap::from_iter(zip_eq(
                        self.outputs.iter().cloned(),
                        returns.iter().cloned(),
                    )),
                };
                *end = BlockEnd::Goto(self.return_block_id, remapping);
            }
            BlockEnd::Panic(_) | BlockEnd::Goto(_, _) | BlockEnd::Match { .. } => {}
            BlockEnd::NotSet => unreachable!(),
        }
    }
}

impl<'db> FunctionInlinerRewriter<'db> {
    fn apply(
        variables: VariableAllocator<'db>,
        lowered: &Lowered,
        calling_function_id: ConcreteFunctionWithBodyId,
    ) -> Maybe<Lowered> {
        let mut rewriter = Self {
            variables,
            block_queue: BlockRewriteQueue {
                block_queue: lowered.blocks.iter().map(|(_, b)| (b.clone(), true)).collect(),
                blocks: BlocksBuilder::new(),
            },
            statements: vec![],
            block_end: BlockEnd::NotSet,
            unprocessed_statements: Default::default(),
            inlining_success: lowered.blocks.has_root(),
            calling_function_id,
        };

        rewriter.variables.variables = lowered.variables.clone();
        while let Some(block) = rewriter.block_queue.dequeue() {
            rewriter.block_end = block.end;
            rewriter.unprocessed_statements = block.statements.into_iter();

            while let Some(statement) = rewriter.unprocessed_statements.next() {
                rewriter.rewrite(statement)?;
            }

            rewriter.block_queue.finalize(Block {
                statements: std::mem::take(&mut rewriter.statements),
                end: rewriter.block_end,
            });
        }

        let blocks = rewriter
            .inlining_success
            .map(|()| rewriter.block_queue.blocks.build().unwrap())
            .unwrap_or_else(Blocks::new_errored);

        Ok(Lowered {
            diagnostics: lowered.diagnostics.clone(),
            variables: rewriter.variables.variables,
            blocks,
            parameters: lowered.parameters.clone(),
            signature: lowered.signature.clone(),
        })
    }

    /// Rewrites a statement and either appends it to self.statements or adds new statements to
    /// self.statements_rewrite_stack.
    fn rewrite(&mut self, statement: Statement) -> Maybe<()> {
        if let Statement::Call(ref stmt) = statement {
            if let Some(called_func) = stmt.function.body(self.variables.db)? {
                if let crate::ids::ConcreteFunctionWithBodyLongId::Specialized(specialized) =
                    self.calling_function_id.lookup_intern(self.variables.db)
                {
                    if specialized.base == called_func {
                        // A specialized function should always inline its base.
                        return self.inline_function(called_func, stmt);
                    }
                }

                // TODO: Implement better logic to avoid inlining of destructors that call
                // themselves.
                if called_func != self.calling_function_id
                    && self.variables.db.priv_should_inline(called_func)?
                {
                    return self.inline_function(called_func, stmt);
                }
            }
        }

        self.statements.push(statement);
        Ok(())
    }

    /// Inlines the given function call.
    pub fn inline_function(
        &mut self,
        function_id: ConcreteFunctionWithBodyId,
        call_stmt: &StatementCall,
    ) -> Maybe<()> {
        let lowered = self.variables.db.lowered_body(function_id, LoweringStage::PostBaseline)?;
        lowered.blocks.has_root()?;

        // Create a new block with all the statements that follow the call statement.
        let return_block_id = self.block_queue.enqueue_block(
            Block {
                statements: std::mem::take(&mut self.unprocessed_statements).collect(),
                end: self.block_end.clone(),
            },
            true,
        );

        // As the block_ids and variable_ids are per function, we need to rename all
        // the blocks and variables before we enqueue the blocks from the function that
        // we are inlining.

        // The input variables need to be renamed to match the inputs to the function call.
        let renamed_vars = HashMap::<VariableId, VariableId>::from_iter(izip!(
            lowered.parameters.iter().cloned(),
            call_stmt.inputs.iter().map(|var_usage| var_usage.var_id)
        ));

        let db = self.variables.db;
        let inlining_location = call_stmt.location.lookup_intern(db).stable_location;

        let mut mapper = Mapper {
            variables: &mut self.variables,
            lowered: &lowered,
            renamed_vars,
            block_id_offset: BlockId(return_block_id.0 + 1),
            return_block_id,
            outputs: &call_stmt.outputs,
            inlining_location,
        };

        // The current block should Goto to the root block of the inlined function.
        // Note that we can't remap the inputs as they might be used after we return
        // from the inlined function.
        // TODO(ilya): Try to use var remapping instead of renaming for the inputs to
        // keep track of the correct Variable.location.
        self.block_end =
            BlockEnd::Goto(mapper.map_block_id(BlockId::root()), VarRemapping::default());

        for (block_id, block) in lowered.blocks.iter() {
            let block = mapper.rebuild_block(block);
            // Inlining is top down - so need to perform further inlining on the inlined function
            // blocks.
            let new_block_id = self.block_queue.enqueue_block(block, false);
            assert_eq!(mapper.map_block_id(block_id), new_block_id, "Unexpected block_id.");
        }

        Ok(())
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut Lowered,
) -> Maybe<()> {
    let variables = VariableAllocator::new(
        db,
        function_id.base_semantic_function(db).function_with_body_id(db),
        lowered.variables.clone(),
    )?;
    if let Ok(new_lowered) = FunctionInlinerRewriter::apply(variables, lowered, function_id) {
        *lowered = new_lowered;
    }
    Ok(())
}
