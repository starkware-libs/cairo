//! Dataflow analysis utilities for the lowering IR.
//!
//! This module provides generic analysis frameworks that can be used by various
//! optimization passes and semantic checks.

pub mod backward;
pub mod def_site;
pub use backward::{BackAnalysis, DataflowBackAnalysis};

pub mod core;
pub use core::{DataflowAnalyzer, Direction, Edge, StatementLocation};

pub mod equality_analysis;
pub mod forward;
pub use forward::ForwardDataflowAnalysis;

#[cfg(test)]
mod def_site_test;
#[cfg(test)]
mod equality_analysis_test;
#[cfg(test)]
mod test;

use crate::{Block, BlockId, MatchInfo, Statement, VarRemapping, VarUsage};

/// Where a variable is defined.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefLocation {
    /// Defined by a statement at the given location.
    Statement(StatementLocation),
    /// Defined at block entry (parameter, goto remapping, or match arm binding).
    BlockEntry(BlockId),
}

impl std::fmt::Debug for DefLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DefLocation::Statement((block, stmt_idx)) => write!(f, "stmt({block:?}, {stmt_idx})"),
            DefLocation::BlockEntry(block) => write!(f, "entry({block:?})"),
        }
    }
}

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
