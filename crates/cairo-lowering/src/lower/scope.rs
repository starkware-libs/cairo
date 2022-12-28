use std::collections::{HashMap, HashSet};

use diagnostics::Maybe;
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
    /// Variables given as inputs to the block, including implicits. Relevant for function blocks /
    /// match arm blocks, etc...
    inputs: Vec<VariableId>,
    /// A [BlockFlowMerger] instance that helps pull variables from higher scopes and records these
    /// pulls.
    merger: Box<BlockFlowMerger>,
    /// Living variables owned by this scope.
    living_variables: LivingVariables,
    /// A store for semantic variables, owning their OwnedVariable instances.
    semantic_variables: SemanticVariablesMap,
    /// A store for implicit variables, owning their OwnedVariable instances.
    implicits: HashMap<semantic::TypeId, LivingVar>,
    // The implicits that are used/changed in this block.
    changed_implicits: HashSet<semantic::TypeId>,
    /// Current sequence of lowered statements emitted.
    statements: Vec<Statement>,
}

/// Represents how a block ends.
pub enum BlockScopeEnd {
    /// Return to callsite with an optional expression (e.g. a block that might end with a tail
    /// expression).
    Callsite(Option<LivingVar>),
    /// Return from the function. The value is a vector of the vars to be returned (not dropped).
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

    /// Returns the stored semantic variable if it exists in the scope. Otherwise, pulls from a
    /// higher scope and returns it.
    /// Move the variable.
    pub fn take_semantic_variable(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        semantic_var_id: semantic::VarId,
    ) -> SemanticVariableEntry {
        self.try_ensure_semantic_variable(ctx, semantic_var_id);
        self.semantic_variables.take(semantic_var_id).unwrap_or(SemanticVariableEntry::Moved)
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

    /// Puts an implicit variable and its owned lowered variable into the current scope.
    pub fn put_implicit(&mut self, ty: semantic::TypeId, var: LivingVar) {
        self.implicits.insert(ty, var);
    }

    /// Marks the implicit as changed and moves it.
    pub fn take_implicit(&mut self, ty: semantic::TypeId) -> Option<LivingVar> {
        self.mark_implicit_changed(ty);
        self.implicits.remove(&ty)
    }

    pub fn mark_implicit_changed(&mut self, ty: semantic::TypeId) {
        self.changed_implicits.insert(ty);
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
            implicits: self.implicits,
            changed_implicits: self.changed_implicits,
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

    /// Pull the living implicit variables into the given merger.
    fn pull_implicits(&mut self, merger: &mut BlockFlowMerger) {
        for (ty, var) in self.implicits.drain() {
            let usable_var = self.living_variables.take_var(var);
            let living_var = merger.splitter.add(usable_var);
            merger.implicit_pulls.insert(ty, living_var);
        }
    }
}

/// A block that was sealed after adding all the statements, just before determining the final
/// inputs.
pub struct BlockSealed {
    /// The inputs to the block, including implicits.
    inputs: Vec<VariableId>,
    /// The living variables at the end of the block.
    living_variables: LivingVariables,
    /// The semantic variables and their state in the end of the block.
    semantic_variables: SemanticVariablesMap,
    /// All the implicits available in this block.
    implicits: HashMap<semantic::TypeId, LivingVar>,
    /// The implicits that were used/changed by this block.
    changed_implicits: HashSet<semantic::TypeId>,
    /// The lowered statements of this block.
    statements: Vec<Statement>,
    /// The end type of this block.
    end: BlockSealedEnd,
}

/// Represents how a block ends. See [BlockScopeEnd].
pub enum BlockSealedEnd {
    /// Return to callsite with an optional expression (e.g. a block that might end with a tail
    /// expression).
    Callsite(Option<UsableVariable>),
    /// Return from the function.
    Return { returns: Vec<UsableVariable>, drops: Vec<VariableId> },
    /// The end of the block is unreachable.
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
    // Implicits that are returned from current scope to the calling scope via block outputs.
    implicit_pushes: &'a [semantic::TypeId],
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
        let BlockSealed {
            inputs,
            mut living_variables,
            mut implicits,
            mut semantic_variables,
            statements,
            end,
            ..
        } = self;
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
                    // Take the variable from this scope if it exists, so it will stay alive for the
                    // caller scope.
                    semantic_variables
                        .take(*semantic_var_id)
                        .and_then(|entry| entry.take_var())
                        .map(|var| living_variables.take_var(var));
                }
                let implicit_pushes: Vec<VariableId> = params
                    .implicit_pushes
                    .iter()
                    .map(|ty| {
                        // This should not panic as implicits are always alive (may only change, but
                        // not drop).
                        let var = implicits
                            .remove(ty)
                            .expect("Implicit removed from main map before finalize()");
                        living_variables.take_var(var).var_id()
                    })
                    .collect();
                for (_, var) in implicits {
                    living_variables.take_var(var);
                }

                let pushes: Vec<VariableId> = params
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
                let outputs = chain!(implicit_pushes, pushes, maybe_output.into_iter()).collect();

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
#[derive(Clone)]
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
    // Implicit parameters that were pulled (moved) from the higher scope. This are always all the
    // implicits that exist in the higher scope.
    implicit_pulls: OrderedHashMap<semantic::TypeId, LivingVar>,
    /// All variables that were pulled, and then rebound to something else.
    changed_semantic_vars: HashSet<semantic::VarId>,
    /// All variables that were pulled and consumed (moved) in at least one branch, and thus cannot
    /// be available in the parent scope anymore (i.e. cannot be pushed).
    moved_semantic_vars: HashSet<semantic::VarId>,
    /// All implicits that were changed in the block. Note, in the case of an optimized match, this
    /// doesn't include the implicits that were consumed by the extern function.
    changed_implicits: HashSet<semantic::TypeId>,
    maybe_output_ty: Option<semantic::TypeId>,
    reachable: bool,
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
        extra_outputs: &[semantic::VarId],
        f: F,
    ) -> (T, BlockMergerFinalized) {
        borrow_as_box(parent_scope, |mut boxed_parent_scope| {
            let mut merger = Self::default();
            boxed_parent_scope.pull_implicits(&mut merger);
            merger.parent_scope = Some(boxed_parent_scope);

            let res = f(ctx, &mut merger);
            let (finalized, returned_scope) = merger.finalize(ctx, extra_outputs);

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
    /// a [BlockScopeEnd] for this block if successful. This block's flow will be merged with the
    /// rest of the blocks created with this function.
    /// `input_tys` are the types of all the inputs to the block, including implicits.
    /// Returns the a [BlockSealed] for that block.
    pub fn run_in_subscope<
        F: FnOnce(&mut LoweringContext<'_>, &mut BlockScope, Vec<LivingVar>) -> Maybe<BlockScopeEnd>,
    >(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        input_tys: Vec<semantic::TypeId>,
        // TODO(yuval): rename f.
        f: F,
    ) -> Maybe<BlockSealed> {
        let block_sealed = borrow_as_box(self, |merger| {
            // Add all implicits to scope.
            let mut living_variables = LivingVariables::default();
            let implicit_vars: Vec<_> = merger
                .implicit_pulls
                .iter()
                .map(|(ty, var)| {
                    let usable_var = merger.splitter.split(var);
                    let living_var = living_variables.introduce_var(usable_var);
                    (*ty, living_var)
                })
                .collect();
            let mut block_scope = BlockScope { merger, living_variables, ..BlockScope::default() };
            for (ty, living_var) in implicit_vars.into_iter() {
                block_scope.put_implicit(ty, living_var);
            }

            // Set inputs.
            let input_vars: Vec<_> = input_tys
                .into_iter()
                .map(|ty| block_scope.living_variables.introduce_new_var(ctx, ty))
                .collect();
            block_scope.inputs = input_vars.iter().map(|var| var.var_id()).collect();
            match f(ctx, &mut block_scope, input_vars) {
                Ok(block_end) => {
                    let (block_sealed, merger) = block_scope.seal(block_end);
                    (Ok(block_sealed), merger)
                }
                Err(diag_edded) => (Err(diag_edded), block_scope.merger),
            }
        })?;
        self.add_block_sealed(ctx, &block_sealed);
        Ok(block_sealed)
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
        self.reachable = true;
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
        self.changed_implicits =
            self.changed_implicits.union(&block_sealed.changed_implicits).copied().collect();
        Some(())
    }

    /// Finalizes the merger, deciding on the correct pulls and pushes for all the blocks
    /// encountered.
    /// `extra_outputs` - Semantic variables to push as ref parameters
    fn finalize(
        mut self,
        ctx: &LoweringContext<'_>,
        extra_outputs: &[semantic::VarId],
    ) -> (BlockMergerFinalized, Option<Box<BlockScope>>) {
        let mut pulls = OrderedHashMap::default();
        let mut pushes = Vec::new();
        let mut bring_back = OrderedHashMap::default();
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
                let parent_scope = self.parent_scope.as_mut().unwrap();
                if !parent_scope.living_variables.contains(var.var_id()) {
                    parent_scope.living_variables.introduce_var(self.splitter.split(&var));
                }
                bring_back.insert(semantic_var_id, var);
            }
        }
        // TODO(spapini): extra_outputs might not be alive. Currently, this panics.
        pushes.extend(extra_outputs.iter().copied());

        let mut unchanged_implicits = HashMap::new();
        if self.parent_scope.is_some() {
            // The unchanged implicits are all the ones existing in the parent scope minus the
            // changed ones. Note that in an optimized match, the ones existing in
            // the parent_scope don't include the ones that are used by the extern
            // function. These are added later to implicits_to_push, as they do need to
            // be pushed back out of the match.
            let parent_scope = self.parent_scope.as_mut().unwrap();
            for (ty, var) in self.implicit_pulls.into_iter() {
                if !self.changed_implicits.contains(&ty) {
                    parent_scope.living_variables.introduce_var(self.splitter.split(&var));
                    unchanged_implicits.insert(ty, var);
                }
            }
        }

        let mut implicits_to_push: Vec<semantic::TypeId> = Vec::new();
        for ty in ctx.implicits {
            if !unchanged_implicits.contains_key(ty) {
                implicits_to_push.push(*ty);
            }
        }

        let push_tys = chain!(
            implicits_to_push.clone(),
            pushes.iter().map(|var_id| ctx.semantic_defs[*var_id].ty()),
        )
        .collect();
        let end_info = if self.reachable {
            BlockEndInfo::Callsite { maybe_output_ty: self.maybe_output_ty, push_tys }
        } else {
            BlockEndInfo::End
        };

        // TODO(spapini): Optimize pushes by maintaining shouldnt_push.
        (
            BlockMergerFinalized {
                end_info,
                pulls,
                splitter: self.splitter,
                outer_var_info: OuterVarInfo { pushes, bring_back },
                outer_implicit_info: OuterImplicitInfo {
                    pushes: implicits_to_push,
                    unchanged: unchanged_implicits,
                },
            },
            self.parent_scope,
        )
    }
}

/// Information about the effect on the variables of the outer scope.
#[derive(Debug)]
pub struct OuterVarInfo {
    /// Variables that are returned from current scope to the calling scope via block outputs.
    pub pushes: Vec<semantic::VarId>,
    /// Variables that are returned from current scope to the calling scope by not changing them.
    pub bring_back: OrderedHashMap<semantic::VarId, LivingVar>,
}
/// Information about the effect on the implicits of the outer scope.
#[derive(Debug)]
pub struct OuterImplicitInfo {
    /// Implicits that are returned from current scope to the calling scope via block outputs.
    pub pushes: Vec<semantic::TypeId>,
    /// Implicits that are returned from current scope to the calling scope by not changing them.
    pub unchanged: HashMap<semantic::TypeId, LivingVar>,
}

/// Used to finalize blocks. Generated after calling [`BlockFlowMerger::finalize()`].
pub struct BlockMergerFinalized {
    /// End information for the block.
    pub end_info: BlockEndInfo,
    /// Holds the pulled variables and allows splitting them for parallel branches. See [Splitter].
    splitter: Splitter,
    /// Variables that are pulled from the calling scope to current scope.
    pulls: OrderedHashMap<semantic::VarId, LivingVar>,
    /// Information about the effect on the variables of the outer scope.
    pub outer_var_info: OuterVarInfo,
    /// Information about the effect on the implicits of the outer scope.
    pub outer_implicit_info: OuterImplicitInfo,
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
        let params = BlockFinalizeParams {
            pulls,
            pushes: &self.outer_var_info.pushes,
            bring_back: &self.outer_var_info.bring_back.keys().copied().collect::<Vec<_>>(),
            implicit_pushes: &self.outer_implicit_info.pushes,
        };
        block_sealed.finalize(ctx, params)
    }
}
