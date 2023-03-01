//! This module introduced the BackAnalysis utility that allows writing analyzers that go backwards
//! in the flow of the program, on a Lowered representation.

use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchInfo, Statement, VarRemapping, VariableId,
};

/// Location of a lowering statement inside a block.
pub type StatementLocation = (BlockId, usize);

/// Analyzer trait to implement for each specific analysis.
#[allow(unused_variables)]
pub trait Analyzer {
    type Info: Clone;
    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, block: &FlatBlock) {}
    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
    }
    fn visit_remapping(
        &mut self,
        info: &mut Self::Info,
        block_id: BlockId,
        target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
    }
    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &MatchInfo,
        arms: &[(BlockId, Self::Info)],
    ) -> Self::Info;
    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info;
}

/// Main analysis type that allows traversing the flow backwards.
pub struct BackAnalysis<'a, TAnalyzer: Analyzer> {
    pub lowered: &'a FlatLowered,
    pub cache: HashMap<BlockId, TAnalyzer::Info>,
    pub analyzer: TAnalyzer,
}
impl<'a, TAnalyzer: Analyzer> BackAnalysis<'a, TAnalyzer> {
    /// Gets the analysis info for the entire function.
    pub fn get_root_info(&mut self) -> TAnalyzer::Info {
        self.get_block_info(BlockId::root())
    }

    /// Gets the analysis info from the start of a block.
    fn get_block_info(&mut self, block_id: BlockId) -> TAnalyzer::Info {
        if let Some(cached_result) = self.cache.get(&block_id) {
            return cached_result.clone();
        }

        let mut info = self.get_end_info(block_id, &self.lowered.blocks[block_id].end);
        let block_end_offset = self.lowered.blocks[block_id].statements.len();

        // Go through statements backwards, and update info.
        for (i, stmt) in
            self.lowered.blocks[block_id].statements[0..block_end_offset].iter().enumerate().rev()
        {
            let statement_location = (block_id, i);
            self.analyzer.visit_stmt(&mut info, statement_location, stmt);
        }

        self.analyzer.visit_block_start(&mut info, block_id, &self.lowered.blocks[block_id]);

        // Cache result.
        self.cache.insert(block_id, info.clone());
        info
    }

    /// Gets the analysis info from a [FlatBlockEnd] onwards.
    fn get_end_info(&mut self, block_id: BlockId, block_end: &FlatBlockEnd) -> TAnalyzer::Info {
        let statement_location = (block_id, self.lowered.blocks[block_id].statements.len());
        match block_end {
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Fallthrough(target_block_id, remapping)
            | FlatBlockEnd::Goto(target_block_id, remapping) => {
                let mut info = self.get_block_info(*target_block_id);
                self.analyzer.visit_remapping(&mut info, block_id, *target_block_id, remapping);
                info
            }
            FlatBlockEnd::Return(vars) => self.analyzer.info_from_return(statement_location, vars),
            FlatBlockEnd::Match { info } => {
                let arm_infos = info
                    .arms()
                    .iter()
                    .rev()
                    .map(|(_, arm_block)| (*arm_block, self.get_block_info(*arm_block)))
                    .collect_vec()
                    .into_iter()
                    .rev()
                    .collect_vec();
                self.analyzer.merge_match(statement_location, info, &arm_infos[..])
            }
        }
    }
}
