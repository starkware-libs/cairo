#[cfg(test)]
mod test;

pub mod statements_weights;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::items::functions::InlineConfiguration;
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{Itertools, zip_eq};
use salsa::Database;

use crate::blocks::{Blocks, BlocksBuilder};
use crate::db::LoweringGroup;
use crate::diagnostic::{
    LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics, LoweringDiagnosticsBuilder,
};
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, FunctionWithBodyId,
    FunctionWithBodyLongId, LocationId,
};
use crate::optimizations::const_folding::ConstFoldingContext;
use crate::utils::{InliningStrategy, Rebuilder, RebuilderEx};
use crate::{
    Block, BlockEnd, BlockId, DependencyType, Lowered, LoweringStage, Statement, StatementCall,
    VarRemapping, Variable, VariableArena, VariableId,
};

pub fn get_inline_diagnostics<'db>(
    db: &'db dyn Database,
    function_id: FunctionWithBodyId<'db>,
) -> Maybe<Diagnostics<'db, LoweringDiagnostic<'db>>> {
    let inline_config = match function_id.long(db) {
        FunctionWithBodyLongId::Semantic(id) => db.function_declaration_inline_config(*id)?,
        FunctionWithBodyLongId::Generated { .. } => InlineConfiguration::None,
    };
    let mut diagnostics = LoweringDiagnostics::default();

    if let InlineConfiguration::Always(_) = inline_config
        && db.in_cycle(function_id, crate::DependencyType::Call)?
    {
        diagnostics.report(
            function_id.base_semantic_function(db).untyped_stable_ptr(db),
            LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
        );
    }

    Ok(diagnostics.build())
}

/// Query implementation of [LoweringGroup::priv_should_inline].
#[salsa::tracked]
pub fn priv_should_inline<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<bool> {
    if db.priv_never_inline(function_id)? {
        return Ok(false);
    }

    // Breaks cycles.
    if db.concrete_in_cycle(function_id, DependencyType::Call, LoweringStage::Monomorphized)? {
        return Ok(false);
    }

    match (db.optimizations().inlining_strategy(), function_inline_config(db, function_id)?) {
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

/// Query implementation of [LoweringGroup::priv_never_inline].
#[salsa::tracked]
pub fn priv_never_inline<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<bool> {
    Ok(matches!(function_inline_config(db, function_id)?, InlineConfiguration::Never(_)))
}

/// Returns the [InlineConfiguration] of a function.
fn function_inline_config<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<InlineConfiguration<'db>> {
    match function_id.long(db) {
        ConcreteFunctionWithBodyLongId::Semantic(id) => {
            db.function_declaration_inline_config(id.function_with_body_id(db))
        }
        ConcreteFunctionWithBodyLongId::Generated(_) => Ok(InlineConfiguration::None),
        ConcreteFunctionWithBodyLongId::Specialized(specialized) => {
            function_inline_config(db, specialized.base)
        }
    }
}

// A heuristic to decide if a function without an inline attribute should be inlined.
fn should_inline_lowered(
    db: &dyn Database,
    function_id: ConcreteFunctionWithBodyId<'_>,
    inline_small_functions_threshold: usize,
) -> Maybe<bool> {
    let weight_of_blocks = db.estimate_size(function_id)?;
    Ok(weight_of_blocks < inline_small_functions_threshold.into_or_panic())
}
/// Context for mapping ids from `lowered` to a new `Lowered` object.
pub struct Mapper<'db, 'mt, 'l> {
    db: &'db dyn Database,
    variables: &'mt mut VariableArena<'db>,
    lowered: &'l Lowered<'db>,
    renamed_vars: UnorderedHashMap<VariableId, VariableId>,

    outputs: Vec<VariableId>,
    inlining_location: StableLocation<'db>,

    /// An offset that is added to all the block IDs in order to translate them into the new
    /// lowering representation.
    block_id_offset: BlockId,

    /// Return statements are replaced with goto to this block with the appropriate remapping.
    return_block_id: BlockId,
}

impl<'db, 'mt, 'l> Mapper<'db, 'mt, 'l> {
    pub fn new(
        db: &'db dyn Database,
        variables: &'mt mut VariableArena<'db>,
        lowered: &'l Lowered<'db>,
        call_stmt: StatementCall<'db>,
        block_id_offset: usize,
    ) -> Self {
        // The input variables need to be renamed to match the inputs to the function call.
        let renamed_vars = UnorderedHashMap::<VariableId, VariableId>::from_iter(zip_eq(
            lowered.parameters.iter().cloned(),
            call_stmt.inputs.iter().map(|var_usage| var_usage.var_id),
        ));

        let inlining_location = call_stmt.location.long(db).stable_location;

        Self {
            db,
            variables,
            lowered,
            renamed_vars,
            block_id_offset: BlockId(block_id_offset),
            return_block_id: BlockId(block_id_offset + lowered.blocks.len()),
            outputs: call_stmt.outputs,
            inlining_location,
        }
    }
}

impl<'db, 'mt> Rebuilder<'db> for Mapper<'db, 'mt, '_> {
    /// Maps a var id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    /// If the variable wasn't assigned an id yet, a new id is assigned.
    fn map_var_id(&mut self, orig_var_id: VariableId) -> VariableId {
        *self.renamed_vars.entry(orig_var_id).or_insert_with(|| {
            let orig_var = &self.lowered.variables[orig_var_id];
            self.variables.alloc(Variable {
                location: orig_var.location.inlined(self.db, self.inlining_location),
                ..orig_var.clone()
            })
        })
    }

    /// Maps a block id from the original lowering representation to the equivalent id in the
    /// new lowering representation.
    fn map_block_id(&mut self, orig_block_id: BlockId) -> BlockId {
        BlockId(self.block_id_offset.0 + orig_block_id.0)
    }

    /// Adds the inlining location to a location.
    fn map_location(&mut self, location: LocationId<'db>) -> LocationId<'db> {
        location.inlined(self.db, self.inlining_location)
    }

    fn transform_end(&mut self, end: &mut BlockEnd<'db>) {
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

/// Inner function for applying inlining.
///
/// This function should be called through `apply_inlining` to remove all the lowered blocks in the
/// error case.
fn inner_apply_inlining<'db>(
    db: &'db dyn Database,
    lowered: &mut Lowered<'db>,
    calling_function_id: ConcreteFunctionWithBodyId<'db>,
    mut enable_const_folding: bool,
) -> Maybe<()> {
    lowered.blocks.has_root()?;

    let mut blocks: BlocksBuilder<'db> = BlocksBuilder::new();

    let mut stack: Vec<std::vec::IntoIter<BlockId>> = vec![
        lowered
            .blocks
            .iter()
            .map(|(_, block)| blocks.alloc(block.clone()))
            .collect_vec()
            .into_iter(),
    ];

    let mut const_folding_ctx =
        ConstFoldingContext::new(db, calling_function_id, &mut lowered.variables);

    enable_const_folding = enable_const_folding && !const_folding_ctx.should_skip_const_folding(db);

    while let Some(mut func_blocks) = stack.pop() {
        for block_id in func_blocks.by_ref() {
            let blocks = &mut blocks;
            if enable_const_folding
                && !const_folding_ctx.visit_block_start(block_id, |block_id| &blocks.0[block_id.0])
            {
                continue;
            }

            // Read the next block id before `blocks` is borrowed.
            let next_block_id = blocks.len();
            let block = blocks.get_mut_block(block_id);

            let mut opt_inline_info = None;
            for (idx, statement) in block.statements.iter_mut().enumerate() {
                if enable_const_folding {
                    const_folding_ctx.visit_statement(statement);
                }
                if let Some((call_stmt, called_func)) =
                    should_inline(db, calling_function_id, statement)?
                {
                    opt_inline_info = Some((idx, call_stmt.clone(), called_func));
                    break;
                }
            }

            let Some((call_stmt_idx, call_stmt, called_func)) = opt_inline_info else {
                if enable_const_folding {
                    const_folding_ctx.visit_block_end(block_id, block);
                }
                // Nothing to inline in this block, go to the next block.
                continue;
            };

            let inlined_lowered = db.lowered_body(called_func, LoweringStage::PostBaseline)?;
            inlined_lowered.blocks.has_root()?;

            // Drain the statements starting at the call to the inlined function.
            let remaining_statements =
                block.statements.drain(call_stmt_idx..).skip(1).collect_vec();

            // Replace the end of the block with a goto to the root block of the inlined function.
            let orig_block_end = std::mem::replace(
                &mut block.end,
                BlockEnd::Goto(BlockId(next_block_id), VarRemapping::default()),
            );

            if enable_const_folding {
                const_folding_ctx.visit_block_end(block_id, block);
            }

            let mut inline_mapper = Mapper::new(
                db,
                const_folding_ctx.variables,
                inlined_lowered,
                call_stmt,
                next_block_id,
            );

            // Apply the mapper to the inlined blocks and add them as a contiguous chunk to the
            // blocks builder.
            let mut inlined_blocks_ids = inlined_lowered
                .blocks
                .iter()
                .map(|(_block_id, block)| blocks.alloc(inline_mapper.rebuild_block(block)))
                .collect_vec();

            // Move the remaining statements and the original block end to a new return block.
            let return_block_id =
                blocks.alloc(Block { statements: remaining_statements, end: orig_block_end });
            assert_eq!(return_block_id, inline_mapper.return_block_id);

            // Append the id of the return block to the list of blocks in the inlined function.
            // It is not part of that function, but we want to visit it right after the inlined
            // function blocks.
            inlined_blocks_ids.push(return_block_id);

            // Return the remaining blocks from the current function to the stack and add the blocks
            // of the inlined function to the top of the stack.
            stack.push(func_blocks);
            stack.push(inlined_blocks_ids.into_iter());
            break;
        }
    }

    lowered.blocks = blocks.build().unwrap();
    Ok(())
}

/// Rewrites a statement and either appends it to self.statements or adds new statements to
/// self.statements_rewrite_stack.
fn should_inline<'db, 'r>(
    db: &'db dyn Database,
    calling_function_id: ConcreteFunctionWithBodyId<'db>,
    statement: &'r Statement<'db>,
) -> Maybe<Option<(&'r StatementCall<'db>, ConcreteFunctionWithBodyId<'db>)>>
where
    'db: 'r,
{
    if let Statement::Call(stmt) = statement {
        if stmt.with_coupon {
            return Ok(None);
        }

        if let Some(called_func) = stmt.function.body(db)? {
            if let ConcreteFunctionWithBodyLongId::Specialized(specialized) =
                calling_function_id.long(db)
                && specialized.base == called_func
            {
                // A specialized function should always inline its base.
                return Ok(Some((stmt, called_func)));
            }

            // TODO: Implement better logic to avoid inlining of destructors that call
            // themselves.
            if called_func != calling_function_id && db.priv_should_inline(called_func)? {
                return Ok(Some((stmt, called_func)));
            }
        }
    }

    Ok(None)
}

/// Applies inlining to a lowered function.
///
/// Note that if const folding is enabled, the blocks must be topologically sorted.
pub fn apply_inlining<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
    enable_const_folding: bool,
) -> Maybe<()> {
    if let Err(diag_added) = inner_apply_inlining(db, lowered, function_id, enable_const_folding) {
        lowered.blocks = Blocks::new_errored(diag_added);
    }
    Ok(())
}
