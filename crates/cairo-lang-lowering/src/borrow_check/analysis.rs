//! This module introduced the BackAnalysis utility that allows writing analyzers that go backwards
//! in the flow of the program, on a Lowered representation.

use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, Statement, StatementMatchEnum,
    StatementMatchExtern, VarRemapping, VariableId,
};

/// Location of a statement inside a block.
pub type StatementLocation = (BlockId, usize);

/// Analyzer trait to implement for each specific analysis.
pub trait Analyzer {
    type Info: Clone;
    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, block: &FlatBlock);
    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    );
    fn visit_remapping(&mut self, info: &mut Self::Info, remapping: &VarRemapping);
    // TODO(spapini): These will become block ends instead of statements.
    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_stmt: &Statement,
        arms: &[(BlockId, Self::Info)],
    ) -> Self::Info;
    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info;
    fn info_from_unreachable(&mut self) -> Self::Info;
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
        let root_block_id = self.lowered.root_block.unwrap();
        self.get_block_info(root_block_id)
    }

    /// Gets the analysis info from the start of a block.
    fn get_block_info(&mut self, block_id: BlockId) -> TAnalyzer::Info {
        if let Some(cached_result) = self.cache.get(&block_id) {
            return cached_result.clone();
        }

        // Find real block ending.
        // This indirection and traverse_from_next_split() will removed when lowering is using
        // Gotos.
        let (block_end_offset, mut info) =
            self.get_info_from_next_split(block_id).unwrap_or_else(|| {
                // No branching statement was found, and the BlockId continues until BlockEnd.
                let res = self.get_end_info(block_id, &self.lowered.blocks[block_id].end);
                (self.lowered.blocks[block_id].statements.len(), res)
            });

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
                self.analyzer.visit_remapping(&mut info, remapping);
                info
            }
            FlatBlockEnd::Return(vars) => self.analyzer.info_from_return(statement_location, vars),
            FlatBlockEnd::Unreachable => self.analyzer.info_from_unreachable(),
        }
    }

    // Note: When lowering uses Gotos, this will be merged with get_block_end_info().
    /// Gets the analysis info from the next branching statement in a block.
    /// A block ends in either a branching statement (e.g. match) or a [FlatBlockEnd].
    /// If such a statement was found, returns its index and the Info from that point.
    /// Otherwise, returns None.
    fn get_info_from_next_split(&mut self, block_id: BlockId) -> Option<(usize, TAnalyzer::Info)> {
        for (i, stmt) in self.lowered.blocks[block_id].statements.iter().enumerate() {
            // Closure that creates a new CallsiteInfo struct for a branching statement.
            // Will be removed when lowering uses Gotos.
            let statement_location = (block_id, i);

            let info = match stmt {
                Statement::MatchExtern(StatementMatchExtern { arms, .. })
                | Statement::MatchEnum(StatementMatchEnum { arms, .. }) => {
                    let arm_infos = arms
                        .iter()
                        .map(|(_, arm_block)| (*arm_block, self.get_block_info(*arm_block)))
                        .collect_vec();
                    self.analyzer.merge_match(statement_location, stmt, &arm_infos[..])
                }
                Statement::Desnap(_)
                | Statement::Literal(_)
                | Statement::Call(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_)
                | Statement::EnumConstruct(_)
                | Statement::Snapshot(_) => continue,
            };
            return Some((i, info));
        }
        None
    }
}
