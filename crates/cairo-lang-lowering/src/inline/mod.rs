#[cfg(test)]
mod test;

pub mod statements_weights;

use std::collections::{HashMap, VecDeque};

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic::items::functions::{GenericFunctionWithBodyId, InlineConfiguration};
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::{izip, zip_eq};

use crate::blocks::{Blocks, BlocksBuilder};
use crate::db::LoweringGroup;
use crate::diagnostic::{
    LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics, LoweringDiagnosticsBuilder,
};
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, FunctionWithBodyId,
    FunctionWithBodyLongId, LocationId,
};
use crate::lower::context::VariableAllocator;
use crate::optimizations::const_folding::{
    ConstFoldingContext, Reachability, VarInfo, priv_const_folding_info,
};
use crate::utils::{InliningStrategy, Rebuilder, RebuilderEx};
use crate::{
    Block, BlockEnd, BlockId, DependencyType, Lowered, LoweringStage, Statement, StatementCall,
    VarRemapping, Variable, VariableId,
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
    db: &'db dyn LoweringGroup,
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

    var_alloc: VariableAllocator<'db>,
    const_folding_ctx: ConstFoldingContext<'db>,
    finalize: bool,
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
pub struct Mapper<'a> {
    db: &'a dyn LoweringGroup,
    variables: &'a mut Arena<Variable>,
    lookup_context: &'a ImplLookupContext,
    lowered: &'a Lowered,
    renamed_vars: HashMap<VariableId, VariableId>,
    return_block_id: BlockId,
    outputs: &'a [id_arena::Id<crate::Variable>],
    inlining_location: StableLocation,

    /// An offset that is added to all the block IDs in order to translate them into the new
    /// lowering representation.
    block_id_offset: BlockId,
}

impl Rebuilder for Mapper<'_> {
    /// Maps a var id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    /// If the variable wasn't assigned an id yet, a new id is assigned.
    fn map_var_id(&mut self, orig_var_id: VariableId) -> VariableId {
        *self.renamed_vars.entry(orig_var_id).or_insert_with(|| {
            let orig_var = &self.lowered.variables[orig_var_id];
            self.variables.alloc(Variable::new(
                self.db,
                self.lookup_context.clone(),
                orig_var.ty,
                orig_var.location,
            ))
        })
    }

    /// Maps a block id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    fn map_block_id(&mut self, orig_block_id: BlockId) -> BlockId {
        BlockId(self.block_id_offset.0 + orig_block_id.0)
    }

    /// Adds the inlining location to a location.
    fn map_location(&mut self, location: LocationId) -> LocationId {
        location.inlined(self.db, self.inlining_location)
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

impl<'a> FunctionInlinerRewriter<'a> {
    fn apply(
        db: &'a dyn LoweringGroup,
        lowered: &Lowered,
        calling_function_id: ConcreteFunctionWithBodyId,
        disable_const_folding: bool,
    ) -> Maybe<Lowered> {
        let caller_base = match calling_function_id.lookup_intern(db) {
            ConcreteFunctionWithBodyLongId::Specialized(specialized_func) => specialized_func.base,
            _ => calling_function_id,
        };
        let mut rewriter = Self {
            db,
            block_queue: BlockRewriteQueue {
                block_queue: lowered.blocks.iter().map(|(_, b)| (b.clone(), true)).collect(),
                blocks: BlocksBuilder::new(),
            },
            statements: vec![],
            block_end: BlockEnd::NotSet,
            unprocessed_statements: Default::default(),
            inlining_success: lowered.blocks.has_root(),
            calling_function_id,
            var_alloc: VariableAllocator::new(
                db,
                calling_function_id.base_semantic_function(db).function_with_body_id(db),
                Default::default(),
            )?,
            const_folding_ctx: ConstFoldingContext {
                db,
                var_info: UnorderedHashMap::default(),
                variables: lowered.variables.clone(),
                libfunc_info: priv_const_folding_info(db),
                caller_base,
                reachability: UnorderedHashMap::from_iter([(BlockId::root(), Reachability::Any)]),
                additional_stmts: vec![],
                disabled: disable_const_folding,
            },
            finalize: true,
        };

        if calling_function_id.base_semantic_function(db).generic_function(db)
            == GenericFunctionWithBodyId::Free(
                rewriter.const_folding_ctx.libfunc_info.panic_with_const_felt252,
            )
        {
            return Ok(lowered.clone());
        }

        while let Some(block) = rewriter.block_queue.dequeue() {
            let block_id = rewriter.block_queue.blocks.len();
            let Some(reachability) =
                rewriter.const_folding_ctx.reachability.get(&BlockId(block_id))
            else {
                rewriter.block_queue.finalize(block);
                continue;
            };

            match reachability {
                Reachability::Any => {}
                Reachability::FromSingleGoto(remapping) => {
                    for (dst, src) in remapping.iter() {
                        if let Some(v) = rewriter.const_folding_ctx.as_const(src.var_id) {
                            rewriter
                                .const_folding_ctx
                                .var_info
                                .insert(*dst, VarInfo::Const(v.clone()));
                        }
                    }
                }
            }

            rewriter.block_end = block.end;
            rewriter.unprocessed_statements = block.statements.into_iter();

            while let Some(mut statement) = rewriter.unprocessed_statements.next() {
                rewriter.const_folding_ctx.visit_statement(&mut statement);
                rewriter.rewrite(statement)?;
            }

            let mut new_block = Block {
                statements: std::mem::take(&mut rewriter.statements),
                end: rewriter.block_end,
            };

            rewriter.const_folding_ctx.visit_block_end(&mut new_block);

            if !rewriter.finalize {
                rewriter.finalize = true;
                rewriter.block_queue.enqueue_block(new_block, false);
            } else {
                rewriter.block_queue.finalize(new_block);
            };
        }

        let blocks = rewriter
            .inlining_success
            .map(|()| rewriter.block_queue.blocks.build().unwrap())
            .unwrap_or_else(Blocks::new_errored);

        Ok(Lowered {
            diagnostics: lowered.diagnostics.clone(),
            variables: rewriter.const_folding_ctx.variables,
            blocks,
            parameters: lowered.parameters.clone(),
            signature: lowered.signature.clone(),
        })
    }

    /// Rewrites a statement and either appends it to self.statements or adds new statements to
    /// self.statements_rewrite_stack.
    fn rewrite(&mut self, statement: Statement) -> Maybe<()> {
        if let Statement::Call(ref stmt) = statement {
            if let Some(called_func) = stmt.function.body(self.db)? {
                if let crate::ids::ConcreteFunctionWithBodyLongId::Specialized(specialized) =
                    self.calling_function_id.lookup_intern(self.db)
                {
                    if specialized.base == called_func {
                        // A specialized function should always inline its base.
                        return self.inline_function(called_func, stmt);
                    }
                }

                // TODO: Implement better logic to avoid inlining of destructors that call
                // themselves.
                if called_func != self.calling_function_id
                    && self.db.priv_should_inline(called_func)?
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
        let db = self.db;
        let lowered = db.lowered_body(function_id, LoweringStage::PostBaseline)?;
        lowered.blocks.has_root()?;

        // As the block_ids and variable_ids are per function, we need to rename all
        // the blocks and variables before we enqueue the blocks from the function that
        // we are inlining.

        // The input variables need to be renamed to match the inputs to the function call.
        let renamed_vars = HashMap::<VariableId, VariableId>::from_iter(izip!(
            lowered.parameters.iter().cloned(),
            call_stmt.inputs.iter().map(|var_usage| var_usage.var_id)
        ));

        let inlining_location = call_stmt.location.lookup_intern(db).stable_location;

        let block_id_offset =
            self.block_queue.blocks.len() + self.block_queue.block_queue.len() + 1;

        let new_block = Block {
            statements: std::mem::take(&mut self.statements),
            end: BlockEnd::Goto(BlockId(block_id_offset), VarRemapping::default()),
        };

        if self.finalize {
            self.block_queue.finalize(new_block);
            self.finalize = false;
        } else {
            self.block_queue.enqueue_block(new_block, false);
        }

        let mut mapper = Mapper {
            db,
            variables: &mut self.const_folding_ctx.variables,
            lookup_context: &self.var_alloc.lookup_context,
            lowered: &lowered,
            renamed_vars,
            block_id_offset: BlockId(block_id_offset),
            return_block_id: BlockId(block_id_offset + lowered.blocks.len()),
            outputs: &call_stmt.outputs,
            inlining_location,
        };

        let new_blocks = lowered
            .blocks
            .iter()
            .map(|(block_id, block)| (mapper.map_block_id(block_id), mapper.rebuild_block(block)))
            .collect::<Vec<_>>();

        for (_, mut block) in new_blocks {
            for stmt in block.statements.iter_mut() {
                self.const_folding_ctx.visit_statement(stmt);
            }
            self.const_folding_ctx.visit_block_end(&mut block);
            self.block_queue.enqueue_block(block, false);
        }

        Ok(())
    }
}

pub fn apply_inlining(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut Lowered,
    disable_const_folding: bool,
) -> Maybe<()> {
    if let Ok(new_lowered) =
        FunctionInlinerRewriter::apply(db, lowered, function_id, disable_const_folding)
    {
        *lowered = new_lowered;
        if let Ok(new_lowered) = FunctionInlinerRewriter::apply(db, lowered, function_id, true) {
            *lowered = new_lowered;
        }
    }
    Ok(())
}
