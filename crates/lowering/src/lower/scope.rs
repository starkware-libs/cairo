use std::collections::HashSet;

use itertools::chain;
use utils::ordered_hash_map::OrderedHashMap;
use utils::{borrow_as_box, try_extract_matches};

use super::context::LoweringContext;
use super::semantic_map::{SemanticVariableEntry, SemanticVariablesMap};
use super::variables::{LivingVar, LivingVariables, Splitter, UsableVariable};
use crate::{Block, BlockEnd, BlockId, Statement, VariableId};

pub mod generators;

/// Scope of a block, describing its current state.
/// Maintains the liveness state of lowered variables.
/// Also maintains bound semantic variables. See [SemanticVariablesMap].
// Note: The derive(Default) is for using borrow_as_box below, but it is undesirable for the user to
// create an instance of BlockScope.
#[derive(Default)]
pub struct BlockScope {
    /// Variables given as inputs. Relevant for function blocks / match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// A [BlockFlowMerger] instance that helps pull variables from higher scopes and records these
    /// pulls.
    merger: Box<BlockFlowMerger>,
    /// Living variables owned by this scope.
    living_variables: LivingVariables,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantic_variables: SemanticVariablesMap,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
}

/// Represents how a block ends.
pub enum BlockScopeEnd {
    /// Return to callsite with an optional expression (e.g. a block that might end with a tail
    /// expression).
    Callsite(Option<LivingVar>),
    /// Return from the function.
    Return(Vec<LivingVar>),
    /// The end of the block is unreachable.
    Unreachable,
}

impl BlockScope {
    /// Puts a semantic variable and its owned lowered variable into the current scope.
    // TODO(spapini): Consider enforcing that semantic_var_id is pulled from a higher scope if
    // possible. put_semantic_variable() might be used in a scope other than the defining scope of
    // that variable (for example, in assignments). This should only be possible if the variable
    // was pulled, so that we remember to push it later.
    pub fn put_semantic_variable(&mut self, semantic_var_id: semantic::VarId, var: LivingVar) {
        self.semantic_variables.put(semantic_var_id, var);
    }

    /// Returns the stored semantic variable if it exists in the scope. Otherwise, pulls from a
    /// higher scope and returns it.
    /// This can be read as "borrowing" the semantic variable from an outer scope.
    pub fn use_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> SemanticVariableEntry {
        self.try_ensure_semantic_variable(ctx, semantic_var_id);
        self.semantic_variables.get(ctx, semantic_var_id).unwrap_or(SemanticVariableEntry::Moved)
    }

    /// Tries to ensure that a semantic variable lives in the current scope.
    /// If it doesn't currently live in the scope, try to pull from a higher scope.
    pub fn try_ensure_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<()> {
        if self.semantic_variables.contains(semantic_var_id) {
            return Some(());
        }
        let var = self.merger.take_from_higher_scope(ctx, semantic_var_id)?;
        let var = self.living_variables.introduce_var(var);
        self.semantic_variables.put(semantic_var_id, var);
        Some(())
    }

    /// Seals a BlockScope from adding statements or variables. A sealed block should be finalized
    /// with final pulls to get a [Block]. See [BlockSealed].
    fn seal(mut self, end: BlockScopeEnd) -> (BlockSealed, Box<BlockFlowMerger>) {
        let end = match end {
            BlockScopeEnd::Callsite(maybe_output) => BlockSealedEnd::Callsite(
                maybe_output.map(|var| self.living_variables.take_var(var)),
            ),
            BlockScopeEnd::Return(returns) => {
                let mut drops = Vec::new();
                let returns =
                    returns.into_iter().map(|var| self.living_variables.take_var(var)).collect();
                self.append_all_living_stack(&mut drops);
                BlockSealedEnd::Return { returns, drops }
            }
            BlockScopeEnd::Unreachable => BlockSealedEnd::Unreachable,
        };
        let sealed = BlockSealed {
            inputs: self.inputs,
            living_variables: self.living_variables,
            semantic_variables: self.semantic_variables,
            statements: self.statements,
            end,
        };
        (sealed, self.merger)
    }

    /// Appends all the living variable in the call stack, from this scope to the root.
    fn append_all_living_stack(&self, all_living: &mut Vec<VariableId>) {
        all_living.extend(self.living_variables.get_all());
        self.merger.append_all_living_stack(all_living);
    }
}

/// A block that was sealed after adding all the statements, just before determining the final
/// inputs.
pub struct BlockSealed {
    inputs: Vec<VariableId>,
    living_variables: LivingVariables,
    semantic_variables: SemanticVariablesMap,
    statements: Vec<Statement>,
    end: BlockSealedEnd,
}

/// Represents how a block ends. See [BlockScopeEnd].
pub enum BlockSealedEnd {
    Callsite(Option<UsableVariable>),
    Return { returns: Vec<UsableVariable>, drops: Vec<VariableId> },
    Unreachable,
}

/// Parameters for [`BlockSealed::finalize()`].
pub struct BlockFinalizeParams<'a> {
    // Variables that are pulled from the calling scope to current scope.
    pulls: OrderedHashMap<semantic::VarId, UsableVariable>,
    // Variables that are returned from current scope to the calling scope via block outputs.
    pushes: &'a [semantic::VarId],
    // Variables that are returned from current scope to the calling scope by not changing them.
    bring_back: &'a [semantic::VarId],
}

impl BlockSealed {
    /// Finalizes a sealed block. Expected the final sequence of pulls and pushes.
    /// Pulls are all the semantic variables taken from outer scopes (including function params,
    /// etc.). These will be the inputs to the block, in this order.
    /// Pushes are all the semantic variables that are expected to be given back to the outer
    /// scope. The rest will be dropped. These will appear in the outputs of the block in case
    /// of a Callsite ending, before the optional extra output of the block (i.e. block value).
    ///
    /// Pushes are assumed to only include living semantic variables that were pulled.
    fn finalize(
        self,
        ctx: &mut LoweringContext<'_>,
        params: BlockFinalizeParams<'_>,
    ) -> BlockFinalized {
        let BlockSealed { inputs, mut living_variables, mut semantic_variables, statements, end } =
            self;
        // Pull extra semantic variables if necessary.
        for (semantic_var_id, var) in params.pulls.into_iter() {
            if !semantic_variables.contains(semantic_var_id) {
                semantic_variables.put(semantic_var_id, living_variables.introduce_var(var));
            }
        }
        // Compute drops.
        let (end, end_info, drops) = match end {
            BlockSealedEnd::Callsite(maybe_output) => {
                for semantic_var_id in params.bring_back {
                    // Take the variable from this scope if it exists, so we can bring it will stay
                    // alive for the caller scope.
                    semantic_variables
                        .take(*semantic_var_id)
                        .and_then(|entry| entry.take_var())
                        .map(|var| living_variables.take_var(var));
                }
                let pushes: Vec<_> = params
                    .pushes
                    .iter()
                    .map(|semantic_var_id| {
                        // These should not panic by assumption of the function.
                        let var = semantic_variables
                            .take(*semantic_var_id)
                            .expect("Pushed variable was never pulled.")
                            .take_var()
                            .expect("Pushed variable is dead.");
                        living_variables.take_var(var).var_id()
                    })
                    .collect();
                let maybe_output = maybe_output.as_ref().map(UsableVariable::var_id);
                let maybe_output_ty = maybe_output.map(|var_id| ctx.variables[var_id].ty);
                let push_tys = pushes.iter().map(|var_id| ctx.variables[*var_id].ty).collect();
                let outputs = chain!(maybe_output.into_iter(), pushes).collect();
                let drops = living_variables.get_all();
                (
                    BlockEnd::Callsite(outputs),
                    BlockEndInfo::Callsite { maybe_output_ty, push_tys },
                    drops,
                )
            }
            BlockSealedEnd::Return { returns, drops } => (
                BlockEnd::Return(returns.iter().map(UsableVariable::var_id).collect()),
                BlockEndInfo::End,
                drops,
            ),
            BlockSealedEnd::Unreachable => (BlockEnd::Unreachable, BlockEndInfo::End, vec![]),
        };

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
    /// Holds the pulled variables and allows splitting them for parallel branches. See [Splitter].
    splitter: Splitter,
    /// Variables that were pulled from a higher scope, "used", are kept here.
    pulls: OrderedHashMap<semantic::VarId, LivingVar>,
    /// All variables that were pulled, and then rebound to something else.
    changed_semantic_vars: HashSet<semantic::VarId>,
    /// All variables that were pulled and consumed (moved) in at least one branch, and thus cannot
    /// be available in the parent scope anymore (i.e. cannot be pushed).
    moved_semantic_vars: HashSet<semantic::VarId>,
    maybe_output_ty: Option<semantic::TypeId>,
    // TODO(spapini): Optimize pushes by using shouldnt_push.
}
impl BlockFlowMerger {
    /// Creates a new instance of [BlockFlowMerger] within a limited closure.
    /// Finalizes the merger and returns a [BlockMergerFinalized] instance used for finalizing
    /// blocks.
    /// For the root merger - when there is no parent scope, use [`BlockFlowMerger::with_root()`].
    pub fn with<T, F: FnOnce(&mut LoweringContext<'_>, &mut Self) -> T>(
        ctx: &mut LoweringContext<'_>,
        parent_scope: &mut BlockScope,
        f: F,
    ) -> (T, BlockMergerFinalized) {
        borrow_as_box(parent_scope, |boxed_parent_scope| {
            let mut merger = Self { parent_scope: Some(boxed_parent_scope), ..Self::default() };
            let res = f(ctx, &mut merger);
            let (finalized, returned_scope) = merger.finalize(ctx, &[]);
            ((res, finalized), returned_scope.unwrap())
        })
    }

    /// Creates a new instance of [BlockFlowMerger] within a limited closure.
    /// Finalizes the merger and returns a [BlockMergerFinalized] instance used for finalizing
    /// blocks.
    /// Similar to [`BlockFlowMerger::with()`], except gets no parent_scope, and thus, should be
    /// used for the root scope only.
    pub fn with_root<T, F: FnOnce(&mut LoweringContext<'_>, &mut Self) -> T>(
        ctx: &mut LoweringContext<'_>,
        extra_outputs: &[semantic::VarId],
        f: F,
    ) -> (T, BlockMergerFinalized) {
        let mut merger = Self::default();
        let res = f(ctx, &mut merger);
        let (finalized, _returned_scope) = merger.finalize(ctx, extra_outputs);

        (res, finalized)
    }

    /// Runs a closure with a new subscope [BlockScope] instance. The closure should return
    /// a [BlockScopeEnd] for this block if successfull. This block's flow will be merged with the
    /// rest of the blocks created with this function.
    /// Returns the a [BlockSealed] for that block.
    pub fn run_in_subscope<
        F: FnOnce(&mut LoweringContext<'_>, &mut BlockScope, Vec<LivingVar>) -> Option<BlockScopeEnd>,
    >(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        input_tys: Vec<semantic::TypeId>,
        f: F,
    ) -> Option<BlockSealed> {
        let block_sealed = borrow_as_box(self, |merger| {
            let mut block_scope = BlockScope { merger, ..BlockScope::default() };

            // Set inputs.
            let input_vars: Vec<_> = input_tys
                .into_iter()
                .map(|ty| block_scope.living_variables.introduce_new_var(ctx, ty))
                .collect();
            block_scope.inputs = input_vars.iter().map(|var| var.var_id()).collect();
            if let Some(block_end) = f(ctx, &mut block_scope, input_vars) {
                let (block_sealed, merger) = block_scope.seal(block_end);
                (Some(block_sealed), merger)
            } else {
                (None, block_scope.merger)
            }
        })?;
        self.add_block_sealed(ctx, &block_sealed);
        Some(block_sealed)
    }

    /// Pulls a semantic variable from an outer scope.
    fn take_from_higher_scope(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> Option<UsableVariable> {
        // Try to use from parent scope if the semantic variable is not present.
        if !self.pulls.contains_key(&semantic_var_id) {
            self.parent_scope.as_mut().and_then(|scope| {
                let var = scope.use_semantic_variable(ctx, semantic_var_id).take_var()?;
                let var = self.splitter.add(scope.living_variables.use_var(ctx, var));
                self.pulls.insert(semantic_var_id, var);
                Some(())
            });
        }

        // If we own it, give a copy.
        Some(self.splitter.split(self.pulls.get(&semantic_var_id)?))
    }

    /// Appends all the living variable in the call stack, from this scope to the root.
    fn append_all_living_stack(&self, all_living: &mut Vec<VariableId>) {
        if let Some(parent_scope) = &self.parent_scope {
            parent_scope.append_all_living_stack(all_living);
        }
    }

    /// Adds a sealed block to the merger. This will help the merger decide on the correct
    /// pulls and pushes.
    fn add_block_sealed(
        &mut self,
        ctx: &LoweringContext<'_>,
        block_sealed: &BlockSealed,
    ) -> Option<()> {
        let maybe_output = try_extract_matches!(&block_sealed.end, BlockSealedEnd::Callsite)?;
        self.maybe_output_ty = maybe_output.as_ref().map(|var| ctx.variables[var.var_id()].ty);
        let mut current_moved = HashSet::new();
        let mut current_changed = HashSet::new();
        for (semantic_var_id, entry) in block_sealed.semantic_variables.var_mapping.iter() {
            match entry {
                SemanticVariableEntry::Alive(var)
                    if block_sealed.living_variables.contains(var.var_id()) =>
                {
                    // Living variable.
                    if let Some(pulled_var) = self.pulls.get(semantic_var_id) {
                        if var.var_id() != pulled_var.var_id() {
                            // Changed.
                            current_changed.insert(*semantic_var_id);
                        }
                    }
                }
                _ => {
                    // Dead variable.
                    current_moved.insert(*semantic_var_id);
                }
            }
        }
        self.moved_semantic_vars =
            self.moved_semantic_vars.union(&current_moved).copied().collect();
        self.changed_semantic_vars =
            self.changed_semantic_vars.union(&current_changed).copied().collect();
        Some(())
    }

    /// Finalizes the merger, deciding on the correct pulls and pushes for all the blocks
    /// encountered.
    fn finalize(
        self,
        ctx: &LoweringContext<'_>,
        extra_outputs: &[semantic::VarId],
    ) -> (BlockMergerFinalized, Option<Box<BlockScope>>) {
        let mut pulls = OrderedHashMap::default();
        let mut pushes = Vec::new();
        let mut bring_back = Vec::new();
        for (semantic_var_id, var) in self.pulls.into_iter() {
            if self.moved_semantic_vars.contains(&semantic_var_id) {
                // Variable was moved inside a block. Pull without pushing back.
                pulls.insert(semantic_var_id, var);
            } else if self.changed_semantic_vars.contains(&semantic_var_id) {
                // Variable was not moved, but was changed. Pull and push for rebind in outer scope.
                pulls.insert(semantic_var_id, var);
                pushes.push(semantic_var_id);
            } else {
                // Semantic variable wasn't moved nor changed on any branch. Bring it back to
                // calling scope if possible.
                bring_back.push(semantic_var_id)
            }
        }
        pushes.extend(extra_outputs.iter().copied());
        let push_tys = pushes.iter().map(|var_id| ctx.semantic_defs[*var_id].ty()).collect();
        let end_info = BlockEndInfo::Callsite { maybe_output_ty: self.maybe_output_ty, push_tys };

        // TODO(spapini): Optimize pushes by maintaining shouldnt_push.
        (
            BlockMergerFinalized { end_info, splitter: self.splitter, pulls, pushes, bring_back },
            self.parent_scope,
        )
    }
}

/// Used to finalize blocks. Generated after calling [`BlockFlowMerger::finalize()`].
pub struct BlockMergerFinalized {
    // End information for the block.
    pub end_info: BlockEndInfo,
    /// Holds the pulled variables and allows splitting them for parallel branches. See [Splitter].
    splitter: Splitter,
    // Variables that are pulled from the calling scope to current scope.
    pulls: OrderedHashMap<semantic::VarId, LivingVar>,
    // Variables that are returned from current scope to the calling scope via block outputs.
    pub pushes: Vec<semantic::VarId>,
    // Variables that are returned from current scope to the calling scope by not changing them.
    bring_back: Vec<semantic::VarId>,
}
impl BlockMergerFinalized {
    /// Finalizes a sealed block.
    pub fn finalize_block(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        block_sealed: BlockSealed,
    ) -> BlockFinalized {
        let pulls: OrderedHashMap<_, _> =
            self.pulls.iter().map(|(key, var)| (*key, self.splitter.split(var))).collect();
        let params =
            BlockFinalizeParams { pulls, pushes: &self.pushes, bring_back: &self.bring_back };
        block_sealed.finalize(ctx, params)
    }
}
