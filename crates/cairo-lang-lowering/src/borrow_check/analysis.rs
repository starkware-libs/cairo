//! This module introduced the BackAnalysis utility that allows writing analyzers that go backwards
//! in the flow of the program, on a Lowered representation.

use std::collections::HashMap;

use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchInfo, Statement, VarRemapping, VarUsage,
};

/// Location of a lowering statement inside a block.
pub type StatementLocation = (BlockId, usize);

/// Analyzer trait to implement for each specific analysis.
#[allow(unused_variables)]
pub trait Analyzer<'a> {
    type Info: Clone;
    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, block: &FlatBlock) {}
    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement,
    ) {
    }
    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
    }
    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info;
    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &'a [VarUsage],
    ) -> Self::Info;

    /// Default `info_from_panic` implementation for post 'lower_panics' phases.
    /// Earlier phases need to override this implementation.
    fn info_from_panic(
        &mut self,
        statement_location: StatementLocation,
        var: &VarUsage,
    ) -> Self::Info {
        unreachable!("Panics should have been stripped in the `lower_panics` phase.");
    }
}

/// Main analysis type that allows traversing the flow backwards.
pub struct BackAnalysis<'a, TAnalyzer: Analyzer<'a>> {
    lowered: &'a FlatLowered,
    pub analyzer: TAnalyzer,
    block_info: HashMap<BlockId, TAnalyzer::Info>,
}
impl<'a, TAnalyzer: Analyzer<'a>> BackAnalysis<'a, TAnalyzer> {
    /// Creates a new BackAnalysis instance.
    pub fn new(lowered: &'a FlatLowered, analyzer: TAnalyzer) -> Self {
        Self { lowered, analyzer, block_info: Default::default() }
    }
    /// Gets the analysis info for the entire function.
    pub fn get_root_info(&mut self) -> TAnalyzer::Info {
        let mut dfs_stack = vec![BlockId::root()];
        while let Some(block_id) = dfs_stack.last() {
            let end = &self.lowered.blocks[*block_id].end;
            if !self.add_missing_dependency_blocks(&mut dfs_stack, end) {
                self.calc_block_info(dfs_stack.pop().unwrap());
            }
        }
        self.block_info.remove(&BlockId::root()).unwrap()
    }

    /// Gets the analysis info from the start of a block.
    fn calc_block_info(&mut self, block_id: BlockId) {
        let mut info = self.get_end_info(block_id);

        // Go through statements backwards, and update info.
        for (i, stmt) in self.lowered.blocks[block_id].statements.iter().enumerate().rev() {
            let statement_location = (block_id, i);
            self.analyzer.visit_stmt(&mut info, statement_location, stmt);
        }

        self.analyzer.visit_block_start(&mut info, block_id, &self.lowered.blocks[block_id]);

        // Store result.
        self.block_info.insert(block_id, info);
    }

    /// Adds to the DFS stack the dependent blocks that are not yet in cache - returns whether if
    /// there are any such blocks.
    fn add_missing_dependency_blocks(
        &self,
        dfs_stack: &mut Vec<BlockId>,
        block_end: &'a FlatBlockEnd,
    ) -> bool {
        match block_end {
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Goto(target_block_id, _)
                if !self.block_info.contains_key(target_block_id) =>
            {
                dfs_stack.push(*target_block_id);
                true
            }
            FlatBlockEnd::Goto(_, _) | FlatBlockEnd::Return(..) | FlatBlockEnd::Panic(_) => false,
            FlatBlockEnd::Match { info } => {
                let mut missing_cache = false;
                for arm in info.arms() {
                    if !self.block_info.contains_key(&arm.block_id) {
                        dfs_stack.push(arm.block_id);
                        missing_cache = true;
                    }
                }
                missing_cache
            }
        }
    }

    /// Gets the analysis info from the block's end onwards.
    fn get_end_info(&mut self, block_id: BlockId) -> TAnalyzer::Info {
        let block_end = &self.lowered.blocks[block_id].end;
        let statement_location = (block_id, self.lowered.blocks[block_id].statements.len());
        match block_end {
            FlatBlockEnd::NotSet => unreachable!(),
            FlatBlockEnd::Goto(target_block_id, remapping) => {
                let mut info = self.block_info[target_block_id].clone();
                self.analyzer.visit_goto(
                    &mut info,
                    statement_location,
                    *target_block_id,
                    remapping,
                );
                info
            }
            FlatBlockEnd::Return(vars, _location) => {
                self.analyzer.info_from_return(statement_location, vars)
            }
            FlatBlockEnd::Panic(data) => self.analyzer.info_from_panic(statement_location, data),
            FlatBlockEnd::Match { info } => {
                // Can remove the block since match blocks do not merge.
                let arm_infos =
                    info.arms().iter().map(|arm| self.block_info.remove(&arm.block_id).unwrap());
                self.analyzer.merge_match(statement_location, info, arm_infos)
            }
        }
    }
}
