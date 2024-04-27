#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::DemandReporter;
use crate::borrow_check::Demand;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, VarRemapping, VarUsage, VariableId,
};

pub type MatchOptimizerDemand = Demand<VariableId, (), ()>;

/// Optimizes Statement::EnumConstruct that is followed by a match to jump to the target of the
/// relevant match arm.
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
    // Either order is will result in correct code, but this way variables with smaller ids appear
    // earlier.
    for FixInfo { statement_location, match_block, arm_idx, target_block, remapping } in
        ctx.fixes.into_iter().rev()
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

pub struct MatchOptimizerContext {
    fixes: Vec<FixInfo>,
}

impl MatchOptimizerContext {
    /// Returns true if the statement can be optimized out and false otherwise.
    /// If the statement can be optimized a fix info is added to `self.fixes`.
    fn statement_can_be_optimized_out(
        &mut self,
        stmt: &Statement,
        info: &mut AnalysisInfo<'_>,
        statement_location: (BlockId, usize),
    ) -> bool {
        let Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) = stmt
        else {
            return false;
        };
        let Some(ref mut candidate) = &mut info.candidate else {
            return false;
        };
        if *output != candidate.match_variable {
            return false;
        }
        let (arm_idx, arm) = candidate
            .match_arms
            .iter()
            .find_position(|arm| {
                arm.arm_selector == MatchArmSelector::VariantId((*variant).clone())
            })
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
            self,
            remapping.iter().map(|(dst, src_var_usage)| (dst, (&src_var_usage.var_id, ()))),
        );
        info.demand = demand;

        self.fixes.push(FixInfo {
            statement_location,
            match_block: candidate.match_block,
            arm_idx,
            target_block: arm.block_id,
            remapping,
        });
        true
    }
}

impl DemandReporter<VariableId> for MatchOptimizerContext {
    type IntroducePosition = ();
    type UsePosition = ();
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
}

#[derive(Clone)]
pub struct AnalysisInfo<'a> {
    candidate: Option<OptimizationCandidate<'a>>,
    demand: MatchOptimizerDemand,
}
impl<'a> Analyzer<'a> for MatchOptimizerContext {
    type Info = AnalysisInfo<'a>;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        if !self.statement_can_be_optimized_out(stmt, info, statement_location) {
            info.demand.variables_introduced(self, stmt.outputs(), ());
            info.demand.variables_used(
                self,
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
        if !remapping.is_empty() {
            info.demand
                .apply_remapping(self, remapping.iter().map(|(dst, src)| (dst, (&src.var_id, ()))));

            if let Some(ref mut candidate) = &mut info.candidate {
                let expected_remappings =
                    if let Some(var_usage) = remapping.get(&candidate.match_variable) {
                        candidate.match_variable = var_usage.var_id;
                        1
                    } else {
                        0
                    };

                if remapping.len() != expected_remappings {
                    // Remapping is currently not supported as it breaks SSA when we use the same
                    // remapping with different destination blocks.

                    // TODO(ilya): Support multiple remappings.
                    info.candidate = None;
                }
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
                demand.variables_introduced(self, &arm.var_ids, ());

                (demand, ())
            })
            .collect_vec();
        let mut demand = MatchOptimizerDemand::merge_demands(&arm_demands, self);

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
                })
            }

            _ => None,
        };

        demand.variables_used(
            self,
            match_info.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );

        Self::Info { candidate, demand }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut demand = MatchOptimizerDemand::default();
        demand.variables_used(self, vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())));
        Self::Info { candidate: None, demand }
    }
}
