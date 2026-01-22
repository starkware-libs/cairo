//! This module introduces the BackAnalysis utility that allows writing analyzers that go backwards
//! in the flow of the program, on a Lowered representation.

use std::collections::HashMap;

use crate::analysis::{Analyzer, DataflowAnalyzer, Direction, StatementLocation};
use crate::{Block, BlockEnd, BlockId, Lowered, MatchInfo, Statement, VarRemapping, VarUsage};

/// Main analysis type that allows traversing the flow backwards.
pub struct BackAnalysis<'db, 'a, TAnalyzer: Analyzer<'db, 'a>> {
    lowered: &'a Lowered<'db>,
    pub analyzer: TAnalyzer,
    block_info: HashMap<BlockId, TAnalyzer::Info>,
}
impl<'db, 'a, TAnalyzer: Analyzer<'db, 'a>> BackAnalysis<'db, 'a, TAnalyzer> {
    /// Creates a new BackAnalysis instance.
    pub fn new(lowered: &'a Lowered<'db>, analyzer: TAnalyzer) -> Self {
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
        block_end: &'a BlockEnd<'_>,
    ) -> bool {
        match block_end {
            BlockEnd::NotSet => unreachable!(),
            BlockEnd::Goto(target_block_id, _)
                if !self.block_info.contains_key(target_block_id) =>
            {
                dfs_stack.push(*target_block_id);
                true
            }
            BlockEnd::Goto(_, _) | BlockEnd::Return(..) | BlockEnd::Panic(_) => false,
            BlockEnd::Match { info } => {
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
            BlockEnd::NotSet => unreachable!(),
            BlockEnd::Goto(target_block_id, remapping) => {
                let mut info = self.block_info[target_block_id].clone();
                self.analyzer.visit_goto(
                    &mut info,
                    statement_location,
                    *target_block_id,
                    remapping,
                );
                info
            }
            BlockEnd::Return(vars, _location) => {
                self.analyzer.info_from_return(statement_location, vars)
            }
            BlockEnd::Panic(data) => self.analyzer.info_from_panic(statement_location, data),
            BlockEnd::Match { info } => {
                // Can remove the block since match blocks do not merge.
                let arm_infos =
                    info.arms().iter().map(|arm| self.block_info.remove(&arm.block_id).unwrap());
                self.analyzer.merge_match(statement_location, info, arm_infos)
            }
        }
    }
}

/// Backward analysis runner using `DataflowAnalyzer`.
///
/// This is an adapter that wraps `BackAnalysis` internally, translating
/// between the new `DataflowAnalyzer` trait and the legacy `Analyzer` trait.
/// Once all analyses are migrated, this can be simplified to inline the
/// traversal logic directly.
pub struct DataflowBackAnalysis<'db, 'a, TAnalyzer: DataflowAnalyzer<'db, 'a>> {
    inner: BackAnalysis<'db, 'a, AnalyzerAdapter<'db, 'a, TAnalyzer>>,
}

impl<'db, 'a, TAnalyzer: DataflowAnalyzer<'db, 'a>> DataflowBackAnalysis<'db, 'a, TAnalyzer> {
    /// Creates a new DataflowBackAnalysis instance.
    pub fn new(lowered: &'a Lowered<'db>, analyzer: &'a mut TAnalyzer) -> Self {
        assert!(
            TAnalyzer::DIRECTION == Direction::Backward,
            "DataflowBackAnalysis requires a backward analyzer"
        );
        let adapter = AnalyzerAdapter { analyzer, lowered };
        Self { inner: BackAnalysis::new(lowered, adapter) }
    }

    /// Runs the analysis and returns the result.
    ///
    /// For backward analysis, returns the info at the function entry (root block).
    pub fn run(mut self) -> TAnalyzer::Info {
        self.inner.get_root_info()
    }
}

/// Adapter that implements the legacy `Analyzer` trait by delegating to `DataflowAnalyzer`.
pub struct AnalyzerAdapter<'db, 'a, TAnalyzer: DataflowAnalyzer<'db, 'a>> {
    pub analyzer: &'a mut TAnalyzer,
    lowered: &'a Lowered<'db>,
}

impl<'db, 'a, TAnalyzer: DataflowAnalyzer<'db, 'a>> Analyzer<'db, 'a>
    for AnalyzerAdapter<'db, 'a, TAnalyzer>
{
    type Info = TAnalyzer::Info;

    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &Block<'db>) {
        // Get block from lowered with correct lifetime 'a.
        let block = &self.lowered.blocks[block_id];
        // First apply transfer_block (which processes statements in reverse for backward).
        self.analyzer.transfer_block(info, block_id, block);
        // Then call the block start hook.
        self.analyzer.visit_block_start(info, block_id, block);
    }

    fn visit_stmt(
        &mut self,
        _info: &mut Self::Info,
        _statement_location: StatementLocation,
        _stmt: &'a Statement<'db>,
    ) {
        // Statements are handled by transfer_block in visit_block_start.
        // This is intentionally empty.
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        target_block_id: BlockId,
        remapping: &'a VarRemapping<'db>,
    ) {
        self.analyzer.apply_remapping(info, statement_location, target_block_id, remapping);
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo<'db>,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        self.analyzer.merge_match(statement_location, match_info, infos)
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        _vars: &'a [VarUsage<'db>],
    ) -> Self::Info {
        let block_end = &self.lowered.blocks[statement_location.0].end;
        self.analyzer.initial_info(statement_location.0, block_end)
    }

    fn info_from_panic(
        &mut self,
        statement_location: StatementLocation,
        _var: &VarUsage<'db>,
    ) -> Self::Info {
        let block_end = &self.lowered.blocks[statement_location.0].end;
        self.analyzer.initial_info(statement_location.0, block_end)
    }
}
