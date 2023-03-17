#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use itertools::{zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::DemandReporter;
use crate::borrow_check::LoweredDemand;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, VarRemapping, VariableId,
};

/// Optimizes Statement::EnumConstruct that is follwed by a match to jump to the target of the
/// relevent match arm.
pub fn optimize_matches(lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let ctx = MatchOptimizerContext { fixes: vec![] };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: ctx };
        analysis.get_root_info();
        let ctx = analysis.analyzer;

        for FixInfo { statement_location, target_block, remapping } in ctx.fixes.into_iter() {
            let block = &mut lowered.blocks[statement_location.0];

            assert_eq!(
                block.statements.len() - 1,
                statement_location.1,
                "The optimization can only be applied to the last statment in the block."
            );
            block.statements.pop();

            block.end = FlatBlockEnd::Goto(target_block, remapping)
        }
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
        info: &mut AnalysisInfo,
        statement_location: (BlockId, usize),
    ) -> bool {
        let Statement::EnumConstruct(StatementEnumConstruct {
            variant, input, output }) = stmt else { return false;};
        let Some(ref mut candidate) = &mut info.candidate else {return false;};
        if *output != candidate.match_variable {
            return false;
        }
        let (arm_idx, arm) = candidate
            .match_arms
            .iter()
            .find_position(|arm| arm.variant_id == *variant)
            .expect("arm not found.");

        let [var_id] = arm.var_ids.as_slice() else {
            panic!("An arm of an EnumMatch should produce a single variable.");
        };

        let mut demand = candidate.arm_demands[arm_idx].clone();

        let mut remapping = VarRemapping::default();
        if demand.vars.contains(var_id) {
            // The input to EnumConstruct should be available as `var_id`
            // in `arm.block_id`
            remapping.insert(*var_id, *input);
        }

        demand.apply_remapping(self, remapping.iter().map(|(dst, src)| (*dst, *src)));
        info.demand = demand;

        self.fixes.push(FixInfo { statement_location, target_block: arm.block_id, remapping });
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
    /// The block That we want to jump to.
    target_block: BlockId,
    /// The variable remapping that should be applied.
    remapping: VarRemapping,
}

#[derive(Clone)]
struct OptimizationCandidate {
    /// The variable that is match.
    match_variable: VariableId,

    /// The match arms of the extern match that we are optimizing.
    match_arms: Vec<MatchArm>,

    /// The demands at the arms.
    arm_demands: Vec<LoweredDemand>,
}

#[derive(Clone)]
pub struct AnalysisInfo {
    candidate: Option<OptimizationCandidate>,
    demand: LoweredDemand,
}
impl Analyzer for MatchOptimizerContext {
    type Info = AnalysisInfo;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        if !self.statement_can_be_optimized_out(stmt, info, statement_location) {
            info.demand.variables_introduced(self, &stmt.outputs(), ());
            info.demand.variables_used(self, &stmt.inputs(), ());
        }

        info.candidate = None;
    }

    fn visit_remapping(
        &mut self,
        info: &mut Self::Info,
        _block_id: BlockId,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        if !remapping.is_empty() {
            info.demand.apply_remapping(self, remapping.iter().map(|(dst, src)| (*dst, *src)));

            if let Some(ref mut candidate) = &mut info.candidate {
                let expected_remappings =
                    if let Some(var_id) = remapping.get(&candidate.match_variable) {
                        candidate.match_variable = *var_id;
                        1
                    } else {
                        0
                    };

                if remapping.len() != expected_remappings {
                    // Remapping is currently not supported as it breaks SSA when we use the same
                    // remapping with diffrent destantation blocks.

                    // TODO(ilya): Support multiple remappings.
                    info.candidate = None;
                }
            }
        }
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let arm_demands = zip_eq(match_info.arms(), infos)
            .map(|(arm, info)| {
                let mut demand = info.demand.clone();
                demand.variables_introduced(self, &arm.var_ids, ());

                (demand, ())
            })
            .collect_vec();
        let mut demand = LoweredDemand::merge_demands(&arm_demands, self);

        let candidate = match match_info {
            // A match is a candidate for the optimization if it is a match on an Enum
            // and its input is unused after the match.
            MatchInfo::Enum(MatchEnumInfo { concrete_enum_id: _, input, arms })
                if !demand.vars.contains(input) =>
            {
                Some(OptimizationCandidate {
                    match_variable: *input,
                    match_arms: arms.to_vec(),
                    arm_demands: infos.iter().map(|info| info.demand.clone()).collect(),
                })
            }

            _ => None,
        };

        demand.variables_used(self, &match_info.inputs(), ());

        Self::Info { candidate, demand }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info {
        let mut demand = LoweredDemand::default();
        demand.variables_used(self, vars, ());
        Self::Info { candidate: None, demand }
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        data: &VariableId,
    ) -> Self::Info {
        let mut demand = LoweredDemand::default();
        demand.variables_used(self, &[*data], ());
        Self::Info { candidate: None, demand }
    }
}
