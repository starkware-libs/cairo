#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{Itertools, zip_eq};

use super::var_renamer::VarRenamer;
use crate::borrow_check::Demand;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::EmptyDemandReporter;
use crate::utils::RebuilderEx;
use crate::{
    Block, BlockEnd, BlockId, Lowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, VarRemapping, VarUsage, VariableArena, VariableId,
};

pub type MatchOptimizerDemand<'db> = Demand<VariableId, (), ()>;

impl<'db> MatchOptimizerDemand<'db> {
    fn update(&mut self, statement: &Statement<'db>) {
        self.variables_introduced(&mut EmptyDemandReporter {}, statement.outputs(), ());
        self.variables_used(
            &mut EmptyDemandReporter {},
            statement.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
    }
}

/// Optimizes Statement::EnumConstruct that is followed by a match to jump to the target of the
/// relevant match arm.
///
/// For example, given:
///
/// ```plain
/// blk0:
/// Statements:
/// (v1: core::option::Option::<core::integer::u32>) <- Option::Some(v0)
/// End:
/// Goto(blk1, {v1-> v2})
///
/// blk1:
/// Statements:
/// End:
/// Match(match_enum(v2) {
///   Option::Some(v3) => blk4,
///   Option::None(v4) => blk5,
/// })
/// ```
///
/// Change `blk0` to jump directly to `blk4`.
pub fn optimize_matches<'db>(lowered: &mut Lowered<'db>) {
    if lowered.blocks.is_empty() {
        return;
    }
    let ctx = MatchOptimizerContext { fixes: vec![] };
    let mut analysis = BackAnalysis::new(lowered, ctx);
    analysis.get_root_info();
    let ctx = analysis.analyzer;

    let mut new_blocks = vec![];
    let mut next_block_id = BlockId(lowered.blocks.len());

    // Track variable renaming that results from applying the fixes below.
    // For each (variable_id, arm_idx) pair that is remapped (prior to the match),
    // we assign a new variable (to satisfy the SSA requirement).
    //
    // For example, consider the following blocks:
    //   blk0:
    //   Statements:
    //   (v0: test::Color) <- Color::Red(v5)
    //   End:
    //   Goto(blk1, {v1 -> v2, v0 -> v3})
    //
    //   blk1:
    //   Statements:
    //   End:
    //   Match(match_enum(v3) {
    //     Color::Red(v4) => blk2,
    //   })
    //
    // When the optimization is applied, block0 will jump directly to blk2. Since the definition of
    // v2 is at blk1, we must map v1 to a new variable.
    //
    // If there is another fix for the same match arm, the same variable will be used.
    let mut var_renaming = UnorderedHashMap::<(VariableId, usize), VariableId>::default();

    // Fixes were added in reverse order and need to be applied in that order.
    // This is because `additional_remappings` in later blocks may need to be renamed by fixes from
    // earlier blocks.
    for fix in ctx.fixes {
        // Choose new variables for each destination of the additional remappings (see comment
        // above).
        let mut new_remapping = fix.remapping.clone();
        let mut renamed_vars = OrderedHashMap::<VariableId, VariableId>::default();
        for (var, dst) in fix.additional_remappings.iter() {
            // Allocate a new variable, if it was not allocated before.
            let new_var = *var_renaming
                .entry((*var, fix.arm_idx))
                .or_insert_with(|| lowered.variables.alloc(lowered.variables[*var].clone()));
            new_remapping.insert(new_var, *dst);
            renamed_vars.insert(*var, new_var);
        }

        let block = &mut lowered.blocks[fix.statement_location.0];
        assert_eq!(
            block.statements.len() - 1,
            fix.statement_location.1 + fix.n_same_block_statement,
            "Unexpected number of statements in block."
        );

        if fix.remove_enum_construct {
            block.statements.remove(fix.statement_location.1);
        }

        handle_additional_statements(
            &mut lowered.variables,
            &mut var_renaming,
            &mut new_remapping,
            &mut renamed_vars,
            block,
            &fix,
        );

        block.end = BlockEnd::Goto(fix.target_block, new_remapping);
        if fix.statement_location.0 == fix.match_block {
            // The match was removed (by the assignment of `block.end` above), no need to fix it.
            // Sanity check: there should be no additional remapping in this case.
            assert!(fix.additional_remappings.remapping.is_empty());
            continue;
        }

        let block = &mut lowered.blocks[fix.match_block];
        let BlockEnd::Match { info: MatchInfo::Enum(MatchEnumInfo { arms, location, .. }) } =
            &mut block.end
        else {
            unreachable!("match block should end with a match.");
        };

        let arm = arms.get_mut(fix.arm_idx).unwrap();
        if fix.target_block != arm.block_id {
            // The match arm was already fixed, no need to fix it again.
            continue;
        }

        // Fix match arm not to jump directly to a block that has an incoming gotos and add
        // remapping that matches the goto above.
        let arm_var = arm.var_ids.get_mut(0).unwrap();
        let orig_var = *arm_var;
        *arm_var = lowered.variables.alloc(lowered.variables[orig_var].clone());
        let mut new_block_remapping: VarRemapping<'_> = Default::default();

        new_block_remapping.insert(orig_var, VarUsage { var_id: *arm_var, location: *location });
        for (var, new_var) in renamed_vars.iter() {
            new_block_remapping.insert(*new_var, VarUsage { var_id: *var, location: *location });
        }

        new_blocks.push(Block {
            statements: vec![],
            end: BlockEnd::Goto(arm.block_id, new_block_remapping),
        });
        arm.block_id = next_block_id;
        next_block_id = next_block_id.next_block_id();

        let mut var_renamer = VarRenamer { renamed_vars: renamed_vars.into_iter().collect() };
        // Apply the variable renaming to the reachable blocks.
        for block_id in fix.reachable_blocks {
            let block = &mut lowered.blocks[block_id];
            *block = var_renamer.rebuild_block(block);
        }
    }

    for block in new_blocks {
        lowered.blocks.push(block);
    }
}

/// Handles the additional statements in the fix.
///
/// The additional statements are not in the same block as the enum construct so they need
/// to be copied the current block with new outputs to keep the SSA property.
/// further we need to remap and rename the outputs for the merge in `fix.target_block`.
///
/// Note that since the statements are copied this might increase the code size.
fn handle_additional_statements<'db>(
    variables: &mut VariableArena<'db>,
    var_renaming: &mut UnorderedHashMap<(VariableId, usize), VariableId>,
    new_remapping: &mut VarRemapping<'db>,
    renamed_vars: &mut OrderedHashMap<VariableId, VariableId>,
    block: &mut Block<'db>,
    fix: &FixInfo<'db>,
) {
    if fix.additional_stmts.is_empty() {
        return;
    }

    // Maps input in the original lowering to the inputs after the optimization.
    // Since the statement are copied from after `additional_remappings` to before it,
    // `inputs_remapping` is initialized with `additional_remapping`.
    let mut inputs_remapping = UnorderedHashMap::<VariableId, VariableId>::from_iter(
        fix.additional_remappings.iter().map(|(k, v)| (*k, v.var_id)),
    );
    for mut stmt in fix.additional_stmts.iter().cloned() {
        for input in stmt.inputs_mut() {
            if let Some(orig_var) = inputs_remapping.get(&input.var_id) {
                input.var_id = *orig_var;
            }
        }

        for output in stmt.outputs_mut() {
            let orig_output = *output;
            // Allocate a new variable for the output in the fixed block.
            *output = variables.alloc(variables[*output].clone());
            inputs_remapping.insert(orig_output, *output);

            // Allocate a new post remapping output, if it was not allocated before.
            let new_output = *var_renaming
                .entry((orig_output, fix.arm_idx))
                .or_insert_with(|| variables.alloc(variables[*output].clone()));
            let location = variables[*output].location;
            new_remapping.insert(new_output, VarUsage { var_id: *output, location });
            renamed_vars.insert(orig_output, new_output);
        }

        block.statements.push(stmt);
    }
}

/// Try to apply the optimization at the given statement.
/// If the optimization can be applied, return the fix information and updates the analysis info
/// accordingly.
fn try_get_fix_info<'db>(
    StatementEnumConstruct { variant, input, output }: &StatementEnumConstruct<'db>,
    info: &mut AnalysisInfo<'db, '_>,
    candidate: &mut OptimizationCandidate<'db, '_>,
    statement_location: (BlockId, usize),
) -> Option<FixInfo<'db>> {
    let (arm_idx, arm) = candidate
        .match_arms
        .iter()
        .find_position(
            |arm| matches!(&arm.arm_selector, MatchArmSelector::VariantId(v) if v == variant),
        )
        .expect("arm not found.");

    let [var_id] = arm.var_ids.as_slice() else {
        panic!("An arm of an EnumMatch should produce a single variable.");
    };

    // Prepare a remapping object for the input of the EnumConstruct, which will be used as `var_id`
    // in `arm.block_id`.
    let mut remapping = VarRemapping::default();
    remapping.insert(*var_id, *input);

    // Compute the demand based on the demand of the specific arm, rather than the current demand
    // (which contains the union of the demands from all the arms).
    // Apply the remapping of the input variable and the additional remappings if exist.
    let mut demand = std::mem::take(&mut candidate.arm_demands[arm_idx]);

    let additional_stmts = candidate
        .statement_rev
        .iter()
        .rev()
        .skip(candidate.n_same_block_statement)
        .cloned()
        .cloned()
        .collect_vec();
    for stmt in &additional_stmts {
        demand.update(stmt);
    }

    demand
        .apply_remapping(&mut EmptyDemandReporter {}, [(var_id, (&input.var_id, ()))].into_iter());

    let additional_remappings = match candidate.remapping {
        Some(remappings) => {
            // Filter the additional remappings to only include those that are used in relevant arm.
            VarRemapping {
                remapping: OrderedHashMap::from_iter(remappings.iter().filter_map(|(dst, src)| {
                    if demand.vars.contains_key(dst) { Some((*dst, *src)) } else { None }
                })),
            }
        }
        None => VarRemapping::default(),
    };

    if !additional_remappings.is_empty() && candidate.future_merge {
        // If there are additional_remappings and a future merge we cannot apply the optimization.
        return None;
    }

    demand.apply_remapping(
        &mut EmptyDemandReporter {},
        additional_remappings.iter().map(|(dst, src_var_usage)| (dst, (&src_var_usage.var_id, ()))),
    );

    for stmt in candidate.statement_rev.iter().rev() {
        demand.update(stmt);
    }
    info.demand = demand;
    info.reachable_blocks = std::mem::take(&mut candidate.arm_reachable_blocks[arm_idx]);

    Some(FixInfo {
        statement_location,
        match_block: candidate.match_block,
        arm_idx,
        target_block: arm.block_id,
        remapping,
        reachable_blocks: info.reachable_blocks.clone(),
        additional_remappings,
        n_same_block_statement: candidate.n_same_block_statement,
        remove_enum_construct: !info.demand.vars.contains_key(output),
        additional_stmts,
    })
}

pub struct FixInfo<'db> {
    /// The location that needs to be fixed,
    statement_location: (BlockId, usize),
    /// The block with the match statement that we want to jump over.
    match_block: BlockId,
    /// The index of the arm that we want to jump to.
    arm_idx: usize,
    /// The target block to jump to.
    target_block: BlockId,
    /// Remaps the input of the enum construct to the variable that is introduced by the match arm.
    remapping: VarRemapping<'db>,
    /// The blocks that can be reached from the relevant arm of the match.
    reachable_blocks: OrderedHashSet<BlockId>,
    /// Additional remappings that appeared in a `Goto` leading to the match.
    additional_remappings: VarRemapping<'db>,
    /// The number of statement in the in the same block as the enum construct.
    n_same_block_statement: usize,
    /// Indicated that the enum construct statement can be removed.
    remove_enum_construct: bool,
    /// Additional statement that appear before the match but not in the same block as the enum
    /// construct.
    additional_stmts: Vec<Statement<'db>>,
}

#[derive(Clone)]
struct OptimizationCandidate<'db, 'a> {
    /// The variable that is matched.
    match_variable: VariableId,

    /// The match arms of the extern match that we are optimizing.
    match_arms: &'a [MatchArm<'db>],

    /// The block that the match is in.
    match_block: BlockId,

    /// The demands at the arms.
    arm_demands: Vec<MatchOptimizerDemand<'db>>,

    /// Whether there is a future merge between the match arms.
    future_merge: bool,

    /// The blocks that can be reached from each of the arms.
    arm_reachable_blocks: Vec<OrderedHashSet<BlockId>>,

    /// A remappings that appeared in a `Goto` leading to the match.
    /// Only one such remapping is allowed as this is typically the case
    /// after running `optimize_remapping` and `reorder_statements` and it simplifies the
    /// optimization.
    remapping: Option<&'a VarRemapping<'db>>,

    /// The statements before the match in reverse order.
    statement_rev: Vec<&'a Statement<'db>>,

    /// The number of statement in the in the same block as the enum construct.
    n_same_block_statement: usize,
}

pub struct MatchOptimizerContext<'db> {
    fixes: Vec<FixInfo<'db>>,
}

#[derive(Clone)]
pub struct AnalysisInfo<'db, 'a> {
    candidate: Option<OptimizationCandidate<'db, 'a>>,
    demand: MatchOptimizerDemand<'db>,
    /// Blocks that can be reached from the current block.
    reachable_blocks: OrderedHashSet<BlockId>,
}

impl<'db: 'a, 'a> Analyzer<'db, 'a> for MatchOptimizerContext<'db> {
    type Info = AnalysisInfo<'db, 'a>;

    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &Block<'db>) {
        info.reachable_blocks.insert(block_id);
    }

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
        if let Some(mut candidate) = info.candidate.take() {
            match stmt {
                Statement::EnumConstruct(enum_construct_stmt)
                    if enum_construct_stmt.output == candidate.match_variable =>
                {
                    if let Some(fix_info) = try_get_fix_info(
                        enum_construct_stmt,
                        info,
                        &mut candidate,
                        statement_location,
                    ) {
                        self.fixes.push(fix_info);
                        return;
                    }

                    // Since `candidate.match_variable` was introduced, the candidate is no longer
                    // applicable.
                    info.candidate = None;
                }
                _ => {
                    candidate.statement_rev.push(stmt);
                    candidate.n_same_block_statement += 1;
                    info.candidate = Some(candidate);
                }
            }
        }

        info.demand.update(stmt);
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &'a VarRemapping<'db>,
    ) {
        info.demand.apply_remapping(
            &mut EmptyDemandReporter {},
            remapping.iter().map(|(dst, src)| (dst, (&src.var_id, ()))),
        );

        let Some(candidate) = &mut info.candidate else {
            return;
        };

        // New block, so reset the number of statements that are in the same block as the enum
        // construct.
        candidate.n_same_block_statement = 0;

        if candidate.future_merge
            && candidate.statement_rev.iter().any(|stmt| !stmt.outputs().is_empty())
        {
            // If we have a future merge and a statement not in the same block as the enum construct
            // has an output, we cannot apply the optimization.
            info.candidate = None;
            return;
        }

        if remapping.is_empty() {
            return;
        }

        if candidate.remapping.is_some() {
            info.candidate = None;
            return;
        }

        // Store the goto's remapping.
        candidate.remapping = Some(remapping);
        if let Some(var_usage) = remapping.get(&candidate.match_variable) {
            candidate.match_variable = var_usage.var_id;
        }
    }

    fn merge_match(
        &mut self,
        (block_id, _statement_idx): StatementLocation,
        match_info: &'a MatchInfo<'db>,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let (arm_demands, arm_reachable_blocks): (Vec<_>, Vec<_>) =
            infos.map(|info| (info.demand, info.reachable_blocks)).unzip();

        let arm_demands_without_arm_var = zip_eq(match_info.arms(), &arm_demands)
            .map(|(arm, demand)| {
                let mut demand = demand.clone();
                // Remove the variable that is introduced by the match arm.
                demand.variables_introduced(&mut EmptyDemandReporter {}, &arm.var_ids, ());

                (demand, ())
            })
            .collect_vec();
        let mut demand = MatchOptimizerDemand::merge_demands(
            &arm_demands_without_arm_var,
            &mut EmptyDemandReporter {},
        );

        // Union the reachable blocks for all the infos.
        let mut reachable_blocks = OrderedHashSet::default();
        let mut max_possible_size = 0;
        for cur_reachable_blocks in &arm_reachable_blocks {
            reachable_blocks.extend(cur_reachable_blocks.iter().cloned());
            max_possible_size += cur_reachable_blocks.len();
        }
        // If the size of `reachable_blocks` is less than the sum of the sizes of the
        // `arm_reachable_blocks`, then there was a collision.
        let found_collision = reachable_blocks.len() < max_possible_size;

        let candidate = match match_info {
            // A match is a candidate for the optimization if it is a match on an Enum
            // and its input is unused after the match.
            MatchInfo::Enum(MatchEnumInfo { input, arms, .. })
                if !demand.vars.contains_key(&input.var_id) =>
            {
                Some(OptimizationCandidate {
                    match_variable: input.var_id,
                    match_arms: arms,
                    match_block: block_id,
                    arm_demands,
                    future_merge: found_collision,
                    arm_reachable_blocks,
                    remapping: None,
                    statement_rev: vec![],
                    n_same_block_statement: 0,
                })
            }
            _ => None,
        };

        demand.variables_used(
            &mut EmptyDemandReporter {},
            match_info.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );

        Self::Info { candidate, demand, reachable_blocks }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage<'db>],
    ) -> Self::Info {
        let mut demand = MatchOptimizerDemand::default();
        demand.variables_used(
            &mut EmptyDemandReporter {},
            vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
        Self::Info { candidate: None, demand, reachable_blocks: Default::default() }
    }
}
