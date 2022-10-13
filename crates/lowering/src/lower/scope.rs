use std::collections::HashSet;

use itertools::chain;
use utils::ordered_hash_map::OrderedHashMap;
use utils::ordered_hash_set::OrderedHashSet;
use utils::{borrow_as_box, try_extract_matches};

use super::context::LoweringContext;
use super::semantic_map::{SemanticVariableEntry, SemanticVariablesMap};
use crate::{Block, BlockEnd, BlockId, Statement, VariableId};

pub mod generators;

/// Wrapper around VariableId, guaranteeing that the variable is alive.
/// Thus, it does not implement copy nor clone.
#[derive(Debug)]
pub struct OwnedVariable(VariableId);
impl OwnedVariable {
    /// Duplicates the variable if it is duplicatable.
    pub fn try_duplicate(&self, ctx: &LoweringContext<'_>) -> Option<Self> {
        if ctx.variables[self.0].duplicatable { Some(OwnedVariable(self.0)) } else { None }
    }
}

/// Scope of a block, describing its current state.
/// Maintains the liveness state of lowered variables.
/// Also maintains bound semantic variables. See [SemanticVariablesMap].
// Note: The derive(Default) is for using borrow_as_box below, but it is undesirable for the user to
// create an instance of BlockScope.
#[derive(Default)]
pub struct BlockScope {
    /// Variables given as inputs. Relevant for function blocks / match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// Function to pull semantic variables from outer scope when not found in current scope.
    merger: Box<BlockFlowMerger>,
    /// Semantic variables pulled from higher scopes, to be used as inputs to the block.
    pulled_semantic_vars: OrderedHashMap<semantic::VarId, VariableId>,
    /// Living variables owned by this scope.
    living_variables: OrderedHashSet<VariableId>,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantic_variables: SemanticVariablesMap,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
}

/// Represents how a block ends.
pub enum BlockScopeEnd {
    Callsite(Option<OwnedVariable>),
    Return(Vec<OwnedVariable>),
    Unreachable,
}

impl BlockScope {
    /// Puts a semantic variable and its owned lowered variable into the current scope.
    pub fn put_semantic_variable(&mut self, semantic_var_id: semantic::VarId, var: OwnedVariable) {
        self.semantic_variables.put(semantic_var_id, var);
    }

    /// Returns the stored semantic variable if it exists in the scope. Otherwise, introduces it as
    /// an input and returns it.
    /// This can be read as "borrowing" the semantic variable from an outer scope.
    pub fn use_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> SemanticVariableEntry {
        self.semantic_variables.get(ctx, semantic_var_id).unwrap_or_else(|| {
            let semantic_var_id = semantic_var_id;
            let var = self.merger.use_from_higher_scope(ctx, semantic_var_id).expect(
                "Requested a non available variable. Semantic model should have caught this.",
            );

            assert!(
                self.pulled_semantic_vars.insert(semantic_var_id, var.0).is_none(),
                "Semantic variable introduced more than once as input to the block"
            );
            self.semantic_variables.put(semantic_var_id, var).get_var(ctx)
        })
    }

    /// Seals a BlockScope from adding statements or variables. A sealed block should be finalized
    /// with final pulls to get a [Block]. See [BlockSealed].
    fn seal(mut self, end: BlockScopeEnd) -> (BlockSealed, Box<BlockFlowMerger>) {
        let end = match end {
            BlockScopeEnd::Callsite(maybe_output) => {
                BlockSealedEnd::Callsite(maybe_output.map(|var| self.take_var(var)))
            }
            BlockScopeEnd::Return(returns) => {
                BlockSealedEnd::Return(returns.into_iter().map(|var| self.take_var(var)).collect())
            }
            BlockScopeEnd::Unreachable => BlockSealedEnd::Unreachable,
        };
        let sealed = BlockSealed {
            inputs: self.inputs,
            pulled_semantic_vars: self.pulled_semantic_vars,
            living_variables: self.living_variables,
            semantic_variables: self.semantic_variables,
            statements: self.statements,
            end,
        };
        (sealed, self.merger)
    }

    /// Internal. Gets a variable, removing from `living_variables` if not duplicatable.
    fn use_var(&mut self, ctx: &LoweringContext<'_>, var: OwnedVariable) -> VariableId {
        let var_id = var.0;
        if ctx.variables[var_id].duplicatable {
            return var_id;
        }
        self.take_var(var)
    }

    /// Internal. Take a variable, removing from `living_variables`.
    fn take_var(&mut self, var: OwnedVariable) -> VariableId {
        let var_id = var.0;
        assert!(self.living_variables.swap_remove(&var_id), "Unexpected dead variable.");
        var_id
    }

    /// Internal. Introduces a new variable into `living_variables`.
    fn introduce_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        ty: semantic::TypeId,
    ) -> OwnedVariable {
        let var_id = ctx.new_variable(ty);
        assert!(self.living_variables.insert(var_id), "Unexpected reintroduced variable.");
        OwnedVariable(var_id)
    }
}

/// A block that was sealed after adding all the statements, just before determining the final
/// inputs.
pub struct BlockSealed {
    inputs: Vec<VariableId>,
    pulled_semantic_vars: OrderedHashMap<semantic::VarId, VariableId>,
    living_variables: OrderedHashSet<VariableId>,
    semantic_variables: SemanticVariablesMap,
    statements: Vec<Statement>,
    end: BlockSealedEnd,
}

pub enum BlockSealedEnd {
    Callsite(Option<VariableId>),
    Return(Vec<VariableId>),
    Unreachable,
}

impl BlockSealed {
    // TODO(spapini): Add the functions:
    //   pub fn pulls_lower_bound(&self) -> OrderedHashSet<semantic::VarId>
    //   pub fn pushes_upper_bound(&self) -> OrderedHashSet<semantic::VarId>
    /// Finalizes a sealed block. Expected the final sequence of pulls and pushes.
    /// Pulls are all the semantic variables taken from outer scopes (including function params,
    /// etc.). These will be the inputs to the block, in this order.
    /// Pushes are all the semantic variables that are expected to be given back to the outer
    /// scope. The rest will be dropped. These will appear in the outputs of the block in case
    /// of a Callsite ending, before the optional extra output of the block (i.e. block value).
    ///
    /// Pulls must include at least all the pulled variables in block.pulled_semantic_vars.
    /// Pushes must include at most all the living semantic variables that were pulled.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        pulls: &OrderedHashMap<semantic::VarId, OwnedVariable>,
        pushes: &[semantic::VarId],
    ) -> BlockFinalized {
        let BlockSealed {
            inputs,
            pulled_semantic_vars,
            mut living_variables,
            mut semantic_variables,
            statements,
            end,
        } = self;
        // Pull extra semantic variables if necessary.
        for (semantic_var_id, var) in pulls.iter() {
            if !pulled_semantic_vars.contains_key(semantic_var_id) {
                living_variables.insert(var.0);
            }
        }
        // Compute drops.
        let (end, end_info) = match end {
            BlockSealedEnd::Callsite(maybe_output) => {
                let pushes: Vec<_> = pushes
                    .iter()
                    .map(|semantic_var_id| {
                        // TODO(spapini): Convert to a diagnostic.
                        // TODO(spapini): Extract var manager from scope to avoid doing it manually.
                        let var_id = semantic_variables
                            .take(*semantic_var_id)
                            .expect("finalize() called with dead output semantic variables.")
                            .var()
                            .expect("Value already moved.")
                            .0;
                        living_variables.swap_remove(&var_id);
                        var_id
                    })
                    .collect();
                let maybe_output_ty = maybe_output.map(|var_id| ctx.variables[var_id].ty);
                let push_tys = pushes.iter().map(|var_id| ctx.variables[*var_id].ty).collect();
                let outputs = chain!(maybe_output.into_iter(), pushes).collect();
                (BlockEnd::Callsite(outputs), BlockEndInfo::Callsite { maybe_output_ty, push_tys })
            }
            BlockSealedEnd::Return(returns) => (BlockEnd::Return(returns), BlockEndInfo::End),
            BlockSealedEnd::Unreachable => (BlockEnd::Unreachable, BlockEndInfo::End),
        };
        // TODO(spapini): Fix this in case of return.
        let drops = living_variables.into_iter().collect();

        let block = ctx.blocks.alloc(Block { inputs, statements, drops, end });
        BlockFinalized { block, end_info }
    }
}

/// A block that was finalized, after merging the flow with all the parallel blocks.
pub struct BlockFinalized {
    pub block: BlockId,
    pub end_info: BlockEndInfo,
}

/// Describes the structure of the output variables of a finalized block.
pub enum BlockEndInfo {
    /// The block returns to callsite.
    Callsite {
        /// Type for the "block value" output variable if exists.
        maybe_output_ty: Option<semantic::TypeId>,
        /// Types for the push (rebind) output variables, that get bound to semantic variables at
        /// the calling scope.
        push_tys: Vec<semantic::TypeId>,
    },
    /// The block does not return to callsite, and thus, has no outputs.
    End,
}

/// A trait for a context object that holds a LoweringContext and can lend it.
pub trait ContextLender<'a> {
    fn ctx(&mut self) -> &mut LoweringContext<'a>;
}

/// Responsible for merging block flows.
/// In a case where one or more blocks appear in parallel (e.g. match between multiple blocks),
/// the created blocks should be fed into this object. After all blocks have been fed, finalize()
/// should be called to get BlockMergerFinalized, which is used to finalize each sealed block.
/// Example:
/// ```ignore
/// use lowering::lower::scope::BlockFlowMerger;
/// let (block_sealed, merger_finalized) = BlockFlowMerger::with_root(&mut ctx, |ctx, merger| {
///     let block_sealed = merger.run_in_subscope(ctx, vec![], |ctx, scope, _| {
///         // Add things to `scope`.
///         Some(BlockScopeEnd::Unreachable)
///     });
///     block_sealed
/// });
/// ```
#[derive(Default)]
pub struct BlockFlowMerger {
    parent_scope: Option<Box<BlockScope>>,
    pulls: OrderedHashMap<semantic::VarId, OwnedVariable>,
    pushable: Option<HashSet<semantic::VarId>>,
    maybe_output_ty: Option<semantic::TypeId>,
    // TODO(spapini): Optimize pushes by using shouldnt_push.
}
impl BlockFlowMerger {
    /// Creates a new instance of [BlockFlowMerger] within a limited closure.
    /// Finalizes the merger and returns a [BlockMergerFinalized] instance used for finalizing
    /// blocks.
    /// For the root merger - when there is no parent scope, use [`BlockFlowMerger::with_root()`].
    pub fn with<'a, Ctx: ContextLender<'a>, T, F: FnOnce(&mut Ctx, &mut Self) -> T>(
        ctx: &mut Ctx,
        parent_scope: &mut BlockScope,
        f: F,
    ) -> (T, BlockMergerFinalized) {
        borrow_as_box(parent_scope, |boxed_parent_scope| {
            let mut merger = Self { parent_scope: Some(boxed_parent_scope), ..Self::default() };
            let res = f(ctx, &mut merger);
            let (finalized, returned_scope) = merger.finalize(ctx.ctx());
            ((res, finalized), returned_scope.unwrap())
        })
    }

    /// Creates a new instance of [BlockFlowMerger] within a limited closure.
    /// Finalizes the merger and returns a [BlockMergerFinalized] instance used for finalizing
    /// blocks.
    /// Similar to [`BlockFlowMerger::with()`], except gets no parent_scope, and thus, should be
    /// used for the root scope only.
    pub fn with_root<'a, Ctx: ContextLender<'a>, T, F: FnOnce(&mut Ctx, &mut Self) -> T>(
        ctx: &mut Ctx,
        f: F,
    ) -> (T, BlockMergerFinalized) {
        let mut merger = Self::default();
        let res = f(ctx, &mut merger);
        let (finalized, _returned_scope) = merger.finalize(ctx.ctx());

        (res, finalized)
    }

    /// Runs a closure with a new subscope [BlockScope] instance. The closure should return
    /// a [BlockScopeEnd] for this block if successfull. This block's flow will be merged with the
    /// rest of the blocks created with this function.
    /// Returns the a [BlockSealed] for that block.
    pub fn run_in_subscope<
        'a,
        Ctx: ContextLender<'a>,
        F: FnOnce(&mut Ctx, &mut BlockScope, Vec<OwnedVariable>) -> Option<BlockScopeEnd>,
    >(
        &mut self,
        ctx: &mut Ctx,
        input_tys: Vec<semantic::TypeId>,
        f: F,
    ) -> Option<BlockSealed> {
        let block_sealed = borrow_as_box(self, |merger| {
            let mut block_scope = BlockScope { merger, ..BlockScope::default() };

            // Set inputs.
            let input_vars: Vec<_> = input_tys
                .into_iter()
                .map(|ty| block_scope.introduce_variable(ctx.ctx(), ty))
                .collect();
            block_scope.inputs = input_vars.iter().map(|var| var.0).collect();
            if let Some(block_end) = f(ctx, &mut block_scope, input_vars) {
                let (block_sealed, merger) = block_scope.seal(block_end);
                (Some(block_sealed), merger)
            } else {
                (None, block_scope.merger)
            }
        })?;
        self.add_block_sealed(ctx.ctx(), &block_sealed);
        Some(block_sealed)
    }

    /// Pulls a semantic variable from an outer scope.
    fn use_from_higher_scope(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<OwnedVariable> {
        // Try to take ownership from parent scope if the semantic variable is not present.
        if !self.pulls.contains_key(&semantic_var_id) {
            self.parent_scope
                .as_mut()
                .and_then(|scope| scope.use_semantic_variable(ctx, semantic_var_id).var())
                .map(|var| self.pulls.insert(semantic_var_id, var));
        }

        // If we own it, give a copy.
        let var = self.pulls.get(&semantic_var_id)?;
        Some(OwnedVariable(var.0))
    }

    /// Adds a sealed block to the merger. This will help the merger decide on the correct
    /// pulls and pushes.
    fn add_block_sealed(
        &mut self,
        ctx: &LoweringContext<'_>,
        block_sealed: &BlockSealed,
    ) -> Option<()> {
        // TODO(spapini): Make this prettier.
        let maybe_output = try_extract_matches!(block_sealed.end, BlockSealedEnd::Callsite)?;
        self.maybe_output_ty = maybe_output.map(|var_id| ctx.variables[var_id].ty);
        let can_push: HashSet<_> = block_sealed.semantic_variables.alive().copied().collect();
        if let Some(some_can_push) = &mut self.pushable {
            *some_can_push = some_can_push.intersection(&can_push).copied().collect();
        } else {
            self.pushable = Some(can_push);
        };
        Some(())
    }

    /// Finalizes the merger, deciding on the correct pulls and pushes for all the blocks
    /// encountered.
    fn finalize(
        self,
        ctx: &LoweringContext<'_>,
    ) -> (BlockMergerFinalized, Option<Box<BlockScope>>) {
        let (pushes, end_info) = match self.pushable {
            Some(pushes) => {
                let pushes: Vec<_> = pushes.into_iter().collect();
                let push_tys =
                    pushes.iter().map(|var_id| ctx.semantic_defs[*var_id].ty()).collect();
                (pushes, BlockEndInfo::Callsite { maybe_output_ty: self.maybe_output_ty, push_tys })
            }
            None => (vec![], BlockEndInfo::End),
        };
        // TODO(spapini): Make stable.
        // TODO(spapini): Optimize pushes by maintaining shouldnt_push.
        (BlockMergerFinalized { end_info, pulls: self.pulls, pushes }, self.parent_scope)
    }
}

/// Determined pulls and pushes. Generated after calling [`BlockFlowMerger::finalize()`].
pub struct BlockMergerFinalized {
    pub end_info: BlockEndInfo,
    pulls: OrderedHashMap<semantic::VarId, OwnedVariable>,
    pub pushes: Vec<semantic::VarId>,
}
impl BlockMergerFinalized {
    pub fn finalize_block(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        block_sealed: BlockSealed,
    ) -> BlockFinalized {
        block_sealed.finalize(ctx, &self.pulls, &self.pushes)
    }
}
