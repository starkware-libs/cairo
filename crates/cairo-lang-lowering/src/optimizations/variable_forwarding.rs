#[cfg(test)]
#[path = "variable_forwarding_test.rs"]
mod test;

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::Itertools;
use salsa::Database;

use crate::analysis::def_site::DefSiteAnalysis;
use crate::analysis::dominator::Dominators;
use crate::analysis::equality_analysis::{EqualityAnalysis, EqualityState};
use crate::analysis::use_sites::UseSites;
use crate::analysis::{DefLocation, UseLocation};
use crate::objects::blocks::Blocks;
use crate::{
    BlockEnd, BlockId, Lowered, MatchInfo, Statement, VarRemapping, VarUsage, VariableArena,
    VariableId,
};

/// A rename to apply when a definition's removal is finalized.
struct RenameDep {
    loc: UseLocation,
    from: VariableId,
    to: VariableId,
}

/// State committed from successful removal transactions.
#[derive(Default)]
struct CommittedState {
    /// Removed statements, grouped by block for efficient Phase 3 application.
    removed: OrderedHashMap<BlockId, UnorderedHashSet<usize>>,
    /// Renames to apply at use locations; gated by `removed` so renames at removed stmts skip.
    renames: Vec<RenameDep>,
    /// Per-variable count of uses freed by committed removals.
    freed_count: OrderedHashMap<VariableId, usize>,
    /// Per-variable count of uses added by committed renames.
    added_count: OrderedHashMap<VariableId, usize>,
}

impl CommittedState {
    fn is_stmt_removed(&self, block_id: BlockId, stmt_idx: usize) -> bool {
        self.removed.get(&block_id).is_some_and(|indices| indices.contains(&stmt_idx))
    }

    fn merge(&mut self, txn: Transaction) {
        for def in txn.removed {
            // TODO: We should deal with all def locations
            if let DefLocation::Statement((block_id, stmt_idx)) = def {
                self.removed.entry(block_id).or_default().insert(stmt_idx);
            }
        }
        self.renames.extend(txn.renames);
        for (v, delta) in txn.freed_delta {
            *self.freed_count.entry(v).or_default() += delta;
        }
        for (v, delta) in txn.added_delta {
            *self.added_count.entry(v).or_default() += delta;
        }
    }
}

/// A transaction — local delta on top of committed state.
/// Merged into committed on success, discarded on failure (no effect).
#[derive(Default)]
struct Transaction {
    /// Definitions tentatively marked for removal in this transaction.
    removed: OrderedHashSet<DefLocation>,
    /// Renames tentatively recorded; reach committed state on merge.
    renames: Vec<RenameDep>,
    /// Per-variable count of uses freed within this transaction.
    freed_delta: OrderedHashMap<VariableId, usize>,
    /// Per-variable count of uses added by tentative renames within this transaction.
    added_delta: OrderedHashMap<VariableId, usize>,
}

/// Applies variable forwarding optimization.
///
/// Uses a worklist + transactional model. Each removal attempt creates an
/// isolated transaction that recursively resolves non-copy obligations
/// (removing producers whose outputs become unconsumed). If all obligations
/// are resolved the transaction is committed; otherwise it is discarded.
/// On failure, defs that were tentatively added to the transaction are
/// returned to the worklist for later retry.
///
/// Assumes blocks are in topological order.
pub fn variable_forwarding<'db>(db: &'db dyn Database, lowered: &mut Lowered<'db>) {
    if lowered.blocks.is_empty() {
        return;
    }

    let equalities = EqualityAnalysis::analyze(db, lowered);

    // Fold enum matches where equality analysis knows the variant, replacing
    // match_enum with Goto. Done before computing use_sites so that the now-unused
    // enum variables can be removed by the transactional worklist below.
    for (block_idx, opt_state) in equalities.iter().enumerate() {
        if let Some(state) = opt_state {
            try_fold_same_block_match(
                state,
                &lowered.variables,
                &mut lowered.blocks[BlockId(block_idx)],
            );
        }
    }

    let dominators = Dominators::analyze(lowered);
    let def_sites = DefSiteAnalysis::analyze(lowered).0;
    let use_sites = UseSites::analyze(lowered);
    let ctx = ForwardingCtx {
        variables: &lowered.variables,
        blocks: &mut lowered.blocks,
        dominators,
        def_sites,
        use_sites,
        equalities: &equalities,
    };
    ctx.forward_all();
}

/// Working context for the forwarding optimization.
struct ForwardingCtx<'a, 'db> {
    /// Variable arena (for copyable/droppable checks).
    variables: &'a VariableArena<'db>,
    /// Mutable blocks; used for both reading statement inputs/outputs and applying renames.
    blocks: &'a mut Blocks<'db>,
    /// Block dominator tree, for visibility checks across blocks.
    dominators: Dominators,
    /// Definition site for each variable, indexed by variable arena index.
    def_sites: Vec<Option<DefLocation>>,
    /// Use sites for each variable, mutated in-place by `prefill` renames.
    use_sites: UseSites,
    /// Per-block equality analysis state, used to find each variable's representative.
    equalities: &'a [Option<EqualityState<'db>>],
}

impl<'a, 'db> ForwardingCtx<'a, 'db> {
    fn forward_all(mut self) {
        // Phase 1: Forward copy+droppable types eagerly.
        self.prefill();

        // Phase 2: Worklist-driven transactional removal.
        // Seed with all definitions, then cascade: successful removals free
        // inputs, whose producers are re-enqueued for re-evaluation.
        let mut committed = CommittedState::default();
        let mut worklist: Vec<DefLocation> =
            self.def_sites.iter().copied().flatten().unique().collect();
        let mut tried: UnorderedHashSet<DefLocation> = UnorderedHashSet::default();
        while let Some(def) = worklist.pop() {
            if self.is_committed(def, &committed) || !tried.insert(def) {
                continue;
            }
            let mut txn = Transaction::default();
            if self.try_remove(def, &committed, &mut txn)
                && self.verify_non_copy_safety(&committed, &txn)
            {
                self.enqueue_freed_producers(&committed, &txn, &mut worklist, &mut tried);
                committed.merge(txn);
            }
        }

        // Phase 3: Apply committed renames and remove statements.
        for r in &committed.renames {
            if let UseLocation::Statement((block_id, stmt_idx)) = r.loc
                && committed.is_stmt_removed(block_id, stmt_idx)
            {
                continue;
            }
            self.rename_var(r.loc, r.from, r.to);
        }
        for (block_id, indices) in committed.removed.iter() {
            let mut i = 0;
            self.blocks[*block_id].statements.retain(|_| {
                let keep = !indices.contains(&i);
                i += 1;
                keep
            });
        }
    }

    /// Forwards copy+droppable variables to their representatives.
    fn prefill(&mut self) {
        for (var_id, var) in self.variables {
            let Some(def) = self.definition(var_id) else {
                continue;
            };
            if !self.removable(def) || var.info.copyable.is_err() || var.info.droppable.is_err() {
                continue;
            }
            // Collect to break borrow on self (rename_var / use_sites mutate).
            for loc in self.use_sites.use_locs(var_id).collect_vec() {
                let Some(eq_state) = self.equalities[loc.block().0].as_ref() else {
                    continue;
                };
                let lead = eq_state.find_immut(var_id);
                if lead != var_id && self.is_visible(loc, lead) {
                    self.rename_var(loc, var_id, lead);
                    self.use_sites.remove_use(var_id, loc);
                    self.use_sites.add_use(lead, loc);
                }
            }
        }
    }

    /// Tries to remove `def` within the current transaction. Recursively resolves
    /// non-drop obligations by removing producers whose outputs become unconsumed.
    /// Returns true if the removal (and all cascaded obligations) succeeded.
    /// On failure, `txn` may contain partial changes — caller should discard the txn.
    fn try_remove(
        &self,
        def: DefLocation,
        committed: &CommittedState,
        txn: &mut Transaction,
    ) -> bool {
        if self.is_removed(def, committed, txn) {
            return true;
        }

        let Some(deps) = self.try_forward_all_outputs(def, committed, txn) else {
            return false;
        };

        // Record removal within transaction.
        txn.removed.insert(def);
        for r in &deps {
            *txn.added_delta.entry(r.to).or_default() += 1;
        }
        txn.renames.extend(deps);

        // Free inputs and resolve non-copy obligations.
        let DefLocation::Statement(stmt_loc) = def else { return true };
        // Collect to break borrow on self.blocks (recursive try_remove mutates txn).
        let inputs: Vec<VariableId> =
            self.blocks[stmt_loc].inputs().iter().map(|u| u.var_id).collect();
        for v in inputs {
            *txn.freed_delta.entry(v).or_default() += 1;

            if self.variables[v].info.copyable.is_err()
                && self.effective_use_count(v, committed, txn) == 0
            {
                // Non-copy variable with no remaining consumers — try to remove
                // the producer so that renames targeting `v` don't create duplicate
                // consuming uses. If the cascade fails, propagate failure so the
                // whole transaction is discarded; partial cascade state is left in
                // `txn` and will be dropped together with the rest by the worklist
                // when this top-level `try_remove` returns false.
                let Some(producer) = self.definition(v) else {
                    continue;
                };
                if !self.try_remove(producer, committed, txn) {
                    return false;
                }
            }
        }
        true
    }

    /// Returns the renames needed to forward all uses of `v` to its representative,
    /// or `None` if any use cannot be forwarded.
    /// Non-consuming uses (snapshots) are included — they need renaming too.
    fn forwarding_dependencies(
        &self,
        v: VariableId,
        committed: &CommittedState,
        txn: &Transaction,
    ) -> Option<Vec<RenameDep>> {
        let mut deps = Vec::new();
        for loc in self.use_sites.use_locs(v) {
            if let UseLocation::Statement(sl) = loc
                && self.is_removed(DefLocation::Statement(sl), committed, txn)
            {
                continue;
            }
            let eq_state = self.equalities[loc.block().0].as_ref()?;
            // Snapshots are NOT rejected — they generate renames like any other use.
            let rep = eq_state.find_immut(v);
            if rep == v || !self.is_visible(loc, rep) {
                return None;
            }
            deps.push(RenameDep { loc, from: v, to: rep });
        }
        // Fail if v gained uses from renames. These added uses aren't tracked in
        // use_sites, so the renames above wouldn't cover them — removing v's
        // producer would leave dangling references.
        if self.has_added_uses(v, committed, txn) {
            return None;
        }
        Some(deps)
    }

    /// Checks ALL outputs of a node. Returns combined rename list or `None`.
    fn try_forward_all_outputs(
        &self,
        def: DefLocation,
        committed: &CommittedState,
        txn: &Transaction,
    ) -> Option<Vec<RenameDep>> {
        if !self.removable(def) {
            return None;
        }
        // TODO(eytan-starkware): Handle BlockEntry from goto remappings.
        let DefLocation::Statement(sl) = def else { return None };
        let outputs: Vec<VariableId> = self.blocks[sl].outputs().to_vec();
        let mut all_renames = Vec::new();
        for v in outputs {
            all_renames.extend(self.forwarding_dependencies(v, committed, txn)?);
        }
        Some(all_renames)
    }

    /// Effective use count: original minus freed plus added.
    fn effective_use_count(
        &self,
        v: VariableId,
        committed: &CommittedState,
        txn: &Transaction,
    ) -> usize {
        self.use_sites.use_count(v)
            - committed.freed_count.get(&v).copied().unwrap_or(0)
            - txn.freed_delta.get(&v).copied().unwrap_or(0)
            + committed.added_count.get(&v).copied().unwrap_or(0)
            + txn.added_delta.get(&v).copied().unwrap_or(0)
    }

    /// Verifies that no non-copy variable ends up with more uses than it started
    /// with. A rename `v → rep` adds a use of `rep`; this is only valid if `rep`'s
    /// original use was freed (producer removed) in the same or earlier transaction.
    ///
    /// TODO(eytan-starkware): This check is overly conservative. Uses in mutually
    /// exclusive branches (neither dominates the other) are safe even if they
    /// increase the total count — at runtime the variable is consumed at most once.
    /// Relax this to check that uses form an antichain in the dominance tree.
    fn verify_non_copy_safety(&self, committed: &CommittedState, txn: &Transaction) -> bool {
        txn.added_delta.iter().all(|(&v, _)| {
            self.variables[v].info.copyable.is_ok()
                || self.effective_use_count(v, committed, txn) <= self.use_sites.use_count(v)
        })
    }

    /// After a successful transaction, enqueue freed inputs' producers.
    fn enqueue_freed_producers(
        &self,
        committed: &CommittedState,
        txn: &Transaction,
        worklist: &mut Vec<DefLocation>,
        tried: &mut UnorderedHashSet<DefLocation>,
    ) {
        for &def in &txn.removed {
            let DefLocation::Statement(stmt_loc) = def else { continue };
            for input in self.blocks[stmt_loc].inputs() {
                let producer = self
                    .definition(input.var_id)
                    .expect("Variable was used, so a definition is expected.");
                if !self.is_removed(producer, committed, txn) {
                    worklist.push(producer);
                    tried.remove(&producer);
                }
            }
        }
    }

    /// Rewrites all usages of `from` to `to` at the given location.
    fn rename_var(&mut self, loc: UseLocation, from: VariableId, to: VariableId) {
        match loc {
            UseLocation::Statement((block_id, stmt_idx)) => {
                for input in self.blocks[block_id].statements[stmt_idx].inputs_mut() {
                    if input.var_id == from {
                        input.var_id = to;
                    }
                }
            }
            UseLocation::BlockEnd(block_id) => match &mut self.blocks[block_id].end {
                BlockEnd::Return(inputs, _) => {
                    for input in inputs {
                        if input.var_id == from {
                            input.var_id = to;
                        }
                    }
                }
                BlockEnd::Panic(input) => {
                    if input.var_id == from {
                        input.var_id = to;
                    }
                }
                BlockEnd::Match { info } => {
                    for input in info.inputs_mut() {
                        if input.var_id == from {
                            input.var_id = to;
                        }
                    }
                }
                BlockEnd::Goto(_, remapping) => {
                    for src in remapping.values_mut() {
                        if src.var_id == from {
                            src.var_id = to;
                        }
                    }
                }
                BlockEnd::NotSet => {}
            },
        }
    }

    fn is_visible(&self, use_loc: UseLocation, var_id: VariableId) -> bool {
        let Some(def) = self.definition(var_id) else {
            return false;
        };
        let def_block = def.block();
        let use_block = use_loc.block();
        if def_block != use_block {
            return self.dominators.dominates(def_block, use_block);
        }
        match (def, use_loc) {
            (DefLocation::BlockEntry(_), _) => true,
            (DefLocation::Statement(_), UseLocation::BlockEnd(_)) => true,
            (DefLocation::Statement((_, di)), UseLocation::Statement((_, ui))) => ui > di,
        }
    }

    fn is_committed(&self, def: DefLocation, committed: &CommittedState) -> bool {
        match def {
            DefLocation::Statement((block_id, stmt_idx)) => {
                committed.is_stmt_removed(block_id, stmt_idx)
            }
            DefLocation::BlockEntry(_) => false,
        }
    }

    fn is_removed(&self, def: DefLocation, committed: &CommittedState, txn: &Transaction) -> bool {
        self.is_committed(def, committed) || txn.removed.contains(&def)
    }

    fn has_added_uses(&self, v: VariableId, committed: &CommittedState, txn: &Transaction) -> bool {
        committed.added_count.contains_key(&v) || txn.added_delta.contains_key(&v)
    }

    fn definition(&self, var: VariableId) -> Option<DefLocation> {
        self.def_sites[var.index()]
    }

    fn removable(&self, def: DefLocation) -> bool {
        match def {
            DefLocation::Statement(stmt_loc) => match self.blocks[stmt_loc] {
                Statement::Const(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_)
                | Statement::EnumConstruct(_)
                | Statement::Snapshot(_)
                | Statement::Desnap(_)
                | Statement::IntoBox(_)
                | Statement::Unbox(_) => true,
                Statement::Call(_) => false,
            },
            DefLocation::BlockEntry(_) => true,
        }
    }
}

/// If block ends with `match_enum(v)` where `v`'s variant is known via equality
/// analysis, replace the match with `Goto(arm.block_id, {arm.var_ids[0] -> payload_var})`.
///
/// The fold removes the match's consumption of `v` and adds a new use of `payload_var`
/// via the goto remapping. To preserve linearity:
/// - `v` (the matched var) must be droppable: removing its only consumer (the match) may leave it
///   unused; non-droppable would leak.
/// - `payload_var` must be copyable: the new goto-remapping use is an additional consumption beyond
///   the one originally fed into `EnumConstruct`. For non-copy payload this would double-consume
///   unless the upstream `EnumConstruct` were also removed — which is not guaranteed here (e.g. its
///   output may still be snapshotted), so we conservatively require copyable.
fn try_fold_same_block_match<'db>(
    state: &EqualityState<'db>,
    variables: &VariableArena<'db>,
    block: &mut crate::Block<'db>,
) {
    let BlockEnd::Match { info: MatchInfo::Enum(ref enum_info) } = block.end else {
        return;
    };
    let matched_var = enum_info.input.var_id;
    let Some((known_variant, payload_var)) = state.get_enum_construct(matched_var) else {
        return;
    };
    if variables[matched_var].info.droppable.is_err()
        || variables[payload_var].info.copyable.is_err()
    {
        return;
    }
    let Some(arm) = enum_info.arms.iter().find(
        |arm| matches!(&arm.arm_selector, MatchArmSelector::VariantId(v) if *v == known_variant),
    ) else {
        return;
    };
    let mut remapping = VarRemapping::default();
    remapping
        .insert(arm.var_ids[0], VarUsage { var_id: payload_var, location: enum_info.location });
    block.end = BlockEnd::Goto(arm.block_id, remapping);
}
