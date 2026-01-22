//! Dataflow analysis utilities for the lowering IR.
//!
//! This module provides generic analysis frameworks that can be used by various
//! optimization passes and semantic checks.

pub mod backward;
pub mod core;

pub use core::{DataflowAnalyzer, Direction};

pub use backward::{BackAnalysis, DataflowBackAnalysis};

use crate::{Block, BlockId, MatchInfo, Statement, VarRemapping, VarUsage};

/// Location of a lowering statement inside a block.
pub type StatementLocation = (BlockId, usize);

/// Analyzer trait to implement for each specific analysis.
#[allow(unused_variables)]
pub trait Analyzer<'db, 'a> {
    type Info: Clone;
    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, block: &Block<'db>) {}
    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
    }
    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        target_block_id: BlockId,
        remapping: &'a VarRemapping<'db>,
    ) {
    }
    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo<'db>,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info;
    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &'a [VarUsage<'db>],
    ) -> Self::Info;

    /// Default `info_from_panic` implementation for post 'lower_panics' phases.
    /// Earlier phases need to override this implementation.
    fn info_from_panic(
        &mut self,
        statement_location: StatementLocation,
        var: &VarUsage<'db>,
    ) -> Self::Info {
        unreachable!("Panics should have been stripped in the `lower_panics` phase.");
    }
}
