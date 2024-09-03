#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{zip_eq, Itertools};

use super::var_renamer::VarRenamer;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::EmptyDemandReporter;
use crate::borrow_check::Demand;
use crate::utils::RebuilderEx;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, VarRemapping, VarUsage, VariableId,
};

pub type MatchOptimizerDemand = Demand<VariableId, (), ()>;

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
pub fn optimize_matches(lowered: &mut FlatLowered) {
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
    // For each match arm and variable that is remapped, we assign a new variable.
    // If there is another fix for the same match arm, the same variable will be used.
    let mut var_renaming = UnorderedHashMap::<(VariableId, usize), VariableId>::default();

    // Fixes were added in reverse order, so we apply them in reverse.
    // Either order will result in correct code, but this way variables with smaller ids appear
    // earlier.
    for FixInfo {
        statement_location,
        match_block,
        arm_idx,
        target_block,
        remapping,
        reachable_blocks,
        additional_remapping,
    } in ctx.fixes.into_iter().rev()
    {
        // Choose new variables for each destination of the additional remappings.
        let mut new_remapping = remapping.clone();
        let mut renamed_vars = OrderedHashMap::<VariableId, VariableId>::default();
        for (var, dst) in additional_remapping.iter() {
            // Allocate a new variable, if it was not allocated before.
            let new_var = *var_renaming
                .entry((*var, arm_idx))
                .or_insert_with(|| lowered.variables.alloc(lowered.variables[*var].clone()));
            new_remapping.insert(new_var, *dst);
            renamed_vars.insert(*var, new_var);
        }
        let mut var_renamer =
            VarRenamer { renamed_vars: renamed_vars.clone().into_iter().collect() };

        let block = &mut lowered.blocks[statement_location.0];
        assert_eq!(
            block.statements.len() - 1,
            statement_location.1,
            "The optimization can only be applied to the last statement in the block."
        );
        block.statements.pop();
        block.end = FlatBlockEnd::Goto(target_block, new_remapping);

        if statement_location.0 == match_block {
            // The match was removed, no need to fix it.
            continue;
        }

        let block = &mut lowered.blocks[match_block];
        let FlatBlockEnd::Match { info: MatchInfo::Enum(MatchEnumInfo { arms, location, .. }) } =
            &mut block.end
        else {
            unreachable!("match block should end with a match.");
        };

        let arm = arms.get_mut(arm_idx).unwrap();
        if target_block != arm.block_id {
            // The match arm was already fixed, no need to fix it again.
            continue;
        }

        // Fix match arm not to jump directly to a block that has an incoming gotos and add
        // remapping that matches the goto above.
        let arm_var = arm.var_ids.get_mut(0).unwrap();
        let orig_var = *arm_var;
        *arm_var = lowered.variables.alloc(lowered.variables[orig_var].clone());
        let mut new_block_remapping: VarRemapping = Default::default();
        new_block_remapping.insert(orig_var, VarUsage { var_id: *arm_var, location: *location });
        for (var, new_var) in renamed_vars.iter() {
            new_block_remapping.insert(*new_var, VarUsage { var_id: *var, location: *location });
        }
        new_blocks.push(FlatBlock {
            statements: vec![],
            end: FlatBlockEnd::Goto(arm.block_id, new_block_remapping),
        });
        arm.block_id = next_block_id;
        next_block_id = next_block_id.next_block_id();

        // Fix reachable blocks.
        for block_id in reachable_blocks {
            let block = &mut lowered.blocks[block_id];
            *block = var_renamer.rebuild_block(block);
        }
    }

    for block in new_blocks.into_iter() {
        lowered.blocks.push(block);
    }
}

/// Returns true if the statement can be optimized out and false otherwise.
/// If the statement can be optimized, returns a [FixInfo] object.
fn statement_can_be_optimized_out(
    stmt: &Statement,
    info: &mut AnalysisInfo<'_>,
    statement_location: (BlockId, usize),
) -> Option<FixInfo> {
    let Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) = stmt else {
        return None;
    };
    let candidate = info.candidate.as_mut()?;
    if *output != candidate.match_variable {
        return None;
    }
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

    let mut demand = candidate.arm_demands[arm_idx].clone();

    let mut remapping = VarRemapping::default();
    // The input to EnumConstruct should be available as `var_id`
    // in `arm.block_id`
    remapping.insert(*var_id, *input);

    let match_var_remapping = remapping.clone();

    if let Some(additional_remappings) = &candidate.additional_remappings {
        remapping.extend(additional_remappings.iter().map(|(var, usage)| (*var, *usage)));
    }

    demand.apply_remapping(
        &mut EmptyDemandReporter {},
        remapping.iter().map(|(dst, src_var_usage)| (dst, (&src_var_usage.var_id, ()))),
    );
    info.demand = demand;

    Some(FixInfo {
        statement_location,
        match_block: candidate.match_block,
        arm_idx,
        target_block: arm.block_id,
        remapping: match_var_remapping,
        // TODO: Avoid the following clone() by passing candidate to this function by value.
        reachable_blocks: candidate.arm_reachable_blocks[arm_idx].clone(),
        additional_remapping: candidate.additional_remappings.clone().unwrap_or_default(),
    })
}

pub struct FixInfo {
    /// The location that needs to be fixed,
    statement_location: (BlockId, usize),
    /// The block with the match statement that we want to jump over.
    match_block: BlockId,
    /// The index of the arm that we want to jump to.
    arm_idx: usize,
    /// The target block to jump to.
    target_block: BlockId,
    /// The variable remapping that should be applied.
    remapping: VarRemapping,
    /// The blocks that can be reached from the relevant arm of the match.
    reachable_blocks: OrderedHashSet<BlockId>,
    /// Additional remappings that appeared in a `Goto` leading to the match.
    additional_remapping: VarRemapping,
}

#[derive(Clone)]
struct OptimizationCandidate<'a> {
    /// The variable that is match.
    match_variable: VariableId,

    /// The match arms of the extern match that we are optimizing.
    match_arms: &'a [MatchArm],

    /// The block that the match is in.
    match_block: BlockId,

    /// The demands at the arms.
    arm_demands: Vec<MatchOptimizerDemand>,

    /// Whether there is a future merge between the match arms.
    future_merge: bool,

    /// The blocks that can be reached from each of the arms.
    arm_reachable_blocks: Vec<OrderedHashSet<BlockId>>,

    /// Additional remappings that appeared in a `Goto` leading to the match.
    additional_remappings: Option<VarRemapping>,
}

pub struct MatchOptimizerContext {
    fixes: Vec<FixInfo>,
}

#[derive(Clone)]
pub struct AnalysisInfo<'a> {
    candidate: Option<OptimizationCandidate<'a>>,
    demand: MatchOptimizerDemand,
    /// Blocks that can be reach from the current block.
    reachable_blocks: OrderedHashSet<BlockId>,
}
impl<'a> Analyzer<'a> for MatchOptimizerContext {
    type Info = AnalysisInfo<'a>;

    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &FlatBlock) {
        info.reachable_blocks.insert(block_id);
    }

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        if let Some(fix_info) = statement_can_be_optimized_out(stmt, info, statement_location) {
            self.fixes.push(fix_info);
        } else {
            info.demand.variables_introduced(&mut EmptyDemandReporter {}, stmt.outputs(), ());
            info.demand.variables_used(
                &mut EmptyDemandReporter {},
                stmt.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
            );
        }

        info.candidate = None;
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        if remapping.is_empty() {
            // Do nothing. Keep the candidate if exists.
            return;
        }

        info.demand.apply_remapping(
            &mut EmptyDemandReporter {},
            remapping.iter().map(|(dst, src)| (dst, (&src.var_id, ()))),
        );

        let Some(ref mut candidate) = &mut info.candidate else {
            return;
        };

        let Some(var_usage) = remapping.get(&candidate.match_variable) else {
            // Revoke the candidate.
            info.candidate = None;
            return;
        };
        let orig_match_variable = candidate.match_variable;
        candidate.match_variable = var_usage.var_id;

        if remapping.len() > 1 {
            if candidate.future_merge || candidate.additional_remappings.is_some() {
                // Don't apply the optimization in this case, as it breaks SSA when we use the same
                // remapping with different destination blocks.

                // TODO(ilya): Support multiple remappings with future merges.
                // Revoke the candidate.
                info.candidate = None;
            } else {
                // Store the goto's remapping, except for the match variable.
                candidate.additional_remappings = Some(VarRemapping {
                    remapping: remapping
                        .iter()
                        .filter_map(|(var, dst)| {
                            if *var != orig_match_variable { Some((*var, *dst)) } else { None }
                        })
                        .collect(),
                });
            }
        }
    }

    fn merge_match(
        &mut self,
        (block_id, _statement_idx): StatementLocation,
        match_info: &'a MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let infos: Vec<_> = infos.collect();
        let arm_demands = zip_eq(match_info.arms(), &infos)
            .map(|(arm, info)| {
                let mut demand = info.demand.clone();
                demand.variables_introduced(&mut EmptyDemandReporter {}, &arm.var_ids, ());

                (demand, ())
            })
            .collect_vec();
        let mut demand =
            MatchOptimizerDemand::merge_demands(&arm_demands, &mut EmptyDemandReporter {});

        // Union the reachable blocks for all the infos.
        let arm_reachable_blocks =
            infos.iter().map(|info| info.reachable_blocks.clone()).collect_vec();
        let mut reachable_blocks = OrderedHashSet::default();
        let mut found_collision = false;
        for cur_reachable_blocks in &arm_reachable_blocks {
            if !found_collision {
                found_collision = cur_reachable_blocks.intersects(&reachable_blocks);
            }
            reachable_blocks.extend(cur_reachable_blocks.iter().cloned());
        }

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
                    arm_demands: infos.into_iter().map(|info| info.demand).collect(),
                    future_merge: found_collision,
                    arm_reachable_blocks,
                    additional_remappings: None,
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
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut demand = MatchOptimizerDemand::default();
        demand.variables_used(
            &mut EmptyDemandReporter {},
            vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
        Self::Info { candidate: None, demand, reachable_blocks: Default::default() }
    }
}
