#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use cairo_lang_semantic::ConcreteVariant;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, Statement,
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

        for FixInfo { block_to_fix, target_block, remapping } in ctx.fixes.into_iter() {
            let block = &mut lowered.blocks[block_to_fix];

            block.end = FlatBlockEnd::Goto(target_block, remapping)
        }
    }
}

pub struct MatchOptimizerContext {
    fixes: Vec<FixInfo>,
}

pub struct FixInfo {
    block_to_fix: BlockId,
    target_block: BlockId,
    remapping: VarRemapping,
}

#[derive(Clone)]
pub struct AnalysisInfo {
    match_variable: VariableId,
    match_arms: Vec<(ConcreteVariant, BlockId)>,

    // The aggregated remapping from the current point till the match statement.
    aggregated_remapping: VarRemapping,
}
impl Analyzer for MatchOptimizerContext {
    type Info = Option<AnalysisInfo>;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        if let Statement::EnumConstruct(StatementEnumConstruct { variant, input: _, output }) = stmt
        {
            if let Some(info) = info {
                let remapping = std::mem::take(&mut info.aggregated_remapping);
                let match_variable =
                    remapping.get(&info.match_variable).unwrap_or(&info.match_variable);
                if *output == *match_variable {
                    let arm = info
                        .match_arms
                        .iter()
                        .find(|(arm_variant, _block_id)| arm_variant == variant)
                        .expect("arm not found.");

                    self.fixes.push(FixInfo {
                        block_to_fix: statement_location.0,
                        target_block: arm.1,
                        remapping,
                    });
                }
            }
        }

        *info = None;
    }

    fn visit_remapping(
        &mut self,
        info: &mut Self::Info,
        _block_id: BlockId,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        if let Some(info) = info {
            let mut remapping = remapping.clone();

            // Compress `dst2 -> dst1 -> src` to `dst2 -> src`
            for (_dst2, dst1) in info.aggregated_remapping.iter_mut() {
                if let Some(src) = remapping.swap_remove(dst1) {
                    *dst1 = src;
                }
            }

            for (dst, src) in remapping.iter() {
                assert!(
                    info.aggregated_remapping.insert(*dst, *src).is_none(),
                    "dst variable defined twice."
                );
            }
        }
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        _arms: &[(BlockId, Self::Info)],
    ) -> Self::Info {
        if let MatchInfo::Enum(MatchEnumInfo { concrete_enum_id: _, input, arms }) = match_info {
            Some(AnalysisInfo {
                match_variable: *input,
                match_arms: arms.to_vec(),
                aggregated_remapping: VarRemapping::default(),
            })
        } else {
            None
        }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        _vars: &[VariableId],
    ) -> Self::Info {
        None
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VariableId,
    ) -> Self::Info {
        None
    }
}
