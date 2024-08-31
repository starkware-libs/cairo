#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::EmptyDemandReporter;
use crate::borrow_check::Demand;
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

    // Fixes were added in reverse order, so we apply them in reverse.
    // Either order will result in correct code, but this way variables with smaller ids appear
    // earlier.
    for FixInfo {
        statement_location,
        match_block,
        arm_idx,
        target_block,
        remapping,
        reachable_blocks: _,
    } in ctx.fixes.into_iter().rev()
    {
        let block = &mut lowered.blocks[statement_location.0];
        assert_eq!(
            block.statements.len() - 1,
            statement_location.1,
            "The optimization can only be applied to the last statement in the block."
        );
        block.statements.pop();
        block.end = FlatBlockEnd::Goto(target_block, remapping);

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
        new_blocks.push(FlatBlock {
            statements: vec![],
            end: FlatBlockEnd::Goto(
                arm.block_id,
                VarRemapping {
                    remapping: OrderedHashMap::from_iter([(
                        orig_var,
                        VarUsage { var_id: *arm_var, location: *location },
                    )]),
                },
            ),
        });
        arm.block_id = next_block_id;
        next_block_id = next_block_id.next_block_id();
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
        remapping,
        // TODO: Avoid the following clone() by passing candidate to this function by value.
        reachable_blocks: candidate.arm_reachable_blocks[arm_idx].clone(),
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
    #[allow(dead_code)]
    reachable_blocks: OrderedHashSet<BlockId>,
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
        candidate.match_variable = var_usage.var_id;

        if remapping.len() > 1 && candidate.future_merge {
            // Don't apply the optimization in this case, as it breaks SSA when we use the same
            // remapping with different destination blocks.

            // TODO(ilya): Support multiple remappings with future merges.
            // Revoke the candidate.
            info.candidate = None;
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
