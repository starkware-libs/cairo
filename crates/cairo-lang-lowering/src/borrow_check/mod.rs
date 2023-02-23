use std::collections::HashMap;

use cairo_lang_defs::ids::ModuleFileId;
use cairo_lang_diagnostics::skip_diagnostic;

pub use self::demand::Demand;
use self::demand::DemandReporter;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::LoweringDiagnostics;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, Statement, StatementMatchEnum, StatementMatchExtern,
    VarRemapping, VariableId,
};

mod demand;

/// Borrowchecking uses sequential flow block. Every branching statement splits the block.
/// When lowering uses Goto, the lowering will also use these kind of blocks, and we won't have
/// this indirection.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct RealBlock(BlockId, usize);

/// Information about the callsite of the current block. When lowering uses Goto, this won't be
/// necessary.
#[derive(Clone)]
pub struct CallsiteInfo<'a> {
    return_site: RealBlock,
    parent: Option<&'a CallsiteInfo<'a>>,
}

pub type LoweredDemand = Demand<VariableId>;
pub struct BorrowChecker<'a> {
    diagnostics: &'a mut LoweringDiagnostics,
    lowered: &'a FlatLowered,
    /// New block ends to be applied at the end of the borrow checking, for optimization.
    new_ends: HashMap<BlockId, FlatBlockEnd>,
    cache: HashMap<RealBlock, LoweredDemand>,
    success: bool,
}

impl<'a> DemandReporter<VariableId, ReportPosition> for BorrowChecker<'a> {
    fn drop(&mut self, position: ReportPosition, var: VariableId) {
        let var = &self.lowered.variables[var];
        if matches!(position, ReportPosition::Report) && !var.droppable {
            self.diagnostics.report_by_location(var.location, VariableNotDropped);
        }
    }

    fn dup(&mut self, position: ReportPosition, var: VariableId) {
        let var = &self.lowered.variables[var];
        if matches!(position, ReportPosition::Report) && !var.duplicatable {
            self.diagnostics.report_by_location(var.location, VariableMoved);
        }
    }
}

/// The position associated with the demand reporting. This is provided at every demand computation,
/// and passed to the reporter when needed. This is used here to specify if we want diagnostics
/// to be reported at the location or not.
#[derive(Copy, Clone)]
enum ReportPosition {
    Report,
    // Currently some reporting is disabled, since Drop is not properly implemented everywhere.
    // TODO(spapini): Fix this.
    DoNotReport,
}

impl<'a> BorrowChecker<'a> {
    /// Computes the variables [LoweredDemand] from the beginning of a [RealBlock], while outputting
    /// borrow checking diagnostics.
    pub fn get_demand(
        &mut self,
        callsite_info: Option<CallsiteInfo<'_>>,
        block: RealBlock,
    ) -> LoweredDemand {
        if let Some(cached_result) = self.cache.get(&block) {
            return cached_result.clone();
        }
        let RealBlock(block_id, stmt_offset) = block;

        // Find real block ending.
        // This indirection and find_block_forwards_demand() will removed when lowering is using
        // Gotos.
        let (real_block_end_offset, mut demand) = self
            .get_demand_from_next_split(block_id, stmt_offset, callsite_info.clone())
            .unwrap_or_else(|| {
                // No branching statement was found, and the RealBlock continues until BlockEnd.
                let demand = self.get_block_end_demand(
                    block_id,
                    &self.lowered.blocks[block_id].end,
                    callsite_info,
                );
                (self.lowered.blocks[block_id].statements.len(), demand)
            });

        // Go through statements backwards, and update demand.
        for stmt in self.lowered.blocks[block_id].statements[stmt_offset..real_block_end_offset]
            .iter()
            .rev()
        {
            demand.variables_introduced(self, &stmt.outputs(), ReportPosition::Report);
            demand.variables_used(self, &stmt.inputs(), ReportPosition::Report);
        }

        if stmt_offset == 0 {
            // Update block inputs.
            demand.variables_introduced(
                self,
                &self.lowered.blocks[block_id].inputs,
                ReportPosition::Report,
            );
        }

        // Cache result.
        self.cache.insert(block, demand.clone());
        demand
    }

    /// Gets the demand of the variables prior to the given remapping.
    /// Also returns a new remapping of only the entries that are used according to the demand. The
    /// caller can use it for optimizing the remapping.
    fn get_remapping_demand(
        &mut self,
        target_block_id: &BlockId,
        remapping: &VarRemapping,
        callsite_info: Option<CallsiteInfo<'_>>,
    ) -> (VarRemapping, LoweredDemand) {
        let mut demand = self.get_demand(callsite_info, RealBlock(*target_block_id, 0));
        let mut new_remapping = VarRemapping::default();
        for (dst, src) in remapping.iter() {
            if demand.vars.swap_remove(dst) {
                demand.vars.insert(*src);
                new_remapping.insert(*dst, *src);
            }
        }
        (new_remapping, demand)
    }

    /// Computes the variables [LoweredDemand] from a [FlatBlockEnd], while outputting borrow
    /// checking diagnostics.
    fn get_block_end_demand(
        &mut self,
        block_id: BlockId,
        block_end: &FlatBlockEnd,
        callsite_info: Option<CallsiteInfo<'_>>,
    ) -> LoweredDemand {
        let demand = match block_end {
            FlatBlockEnd::Fallthrough(target_block_id, remapping) => {
                let (new_remapping, demand) =
                    self.get_remapping_demand(target_block_id, remapping, callsite_info);
                assert!(
                    self.new_ends
                        .insert(
                            block_id,
                            FlatBlockEnd::Fallthrough(*target_block_id, new_remapping)
                        )
                        .is_none(),
                    "Borrow checker cannot visit a block more than once."
                );
                demand
            }
            FlatBlockEnd::Goto(target_block_id, remapping) => {
                let (new_remapping, demand) =
                    self.get_remapping_demand(target_block_id, remapping, callsite_info);
                assert!(
                    self.new_ends
                        .insert(block_id, FlatBlockEnd::Goto(*target_block_id, new_remapping))
                        .is_none(),
                    "Borrow checker cannot visit a block more than once."
                );
                demand
            }
            FlatBlockEnd::Callsite(remapping) => {
                // TODO(yuval): remove in the future, or export to function.
                let callsite_info = callsite_info.unwrap();
                let mut demand =
                    self.get_demand(callsite_info.parent.cloned(), callsite_info.return_site);
                let mut new_remapping = VarRemapping::default();
                for (dst, src) in remapping.iter() {
                    if demand.vars.swap_remove(dst) {
                        demand.vars.insert(*src);
                        new_remapping.insert(*dst, *src);
                    }
                }
                assert!(
                    self.new_ends.insert(block_id, FlatBlockEnd::Callsite(new_remapping)).is_none(),
                    "Borrow checker cannot visit a block more than once."
                );
                demand
            }
            FlatBlockEnd::Return(vars) => LoweredDemand { vars: vars.iter().copied().collect() },
            FlatBlockEnd::Unreachable => LoweredDemand::default(),
            FlatBlockEnd::NotSet => unreachable!(),
        };
        demand
    }

    // Note: When lowering uses Gotos, this will be merged with get_block_end_demand().
    /// Computes the variables [LoweredDemand] from the next branching statement in a block.
    /// A [RealBlock] ends in either a branching statement (e.g. match) or a [FlatBlockEnd].
    /// If such a statement was found, returns its index and the [LoweredDemand] from that point.
    /// Otherwise, returns None.
    fn get_demand_from_next_split(
        &mut self,
        block_id: BlockId,
        stmt_offset: usize,
        callsite_info: Option<CallsiteInfo<'_>>,
    ) -> Option<(usize, LoweredDemand)> {
        for (i, stmt) in self.lowered.blocks[block_id].statements[stmt_offset..].iter().enumerate()
        {
            // Closure that creates a new CallsiteInfo struct for a branching statement.
            // Will be removed when lowering uses Gotos.
            let new_callsite = Some(CallsiteInfo {
                return_site: RealBlock(block_id, stmt_offset + i + 1),
                parent: callsite_info.as_ref(),
            });

            let demand = match stmt {
                Statement::MatchExtern(StatementMatchExtern { arms, .. })
                | Statement::MatchEnum(StatementMatchEnum { arms, .. }) => {
                    let arm_demands = arms
                        .iter()
                        .map(|(_, arm_block)| {
                            self.get_demand(new_callsite.clone(), RealBlock(*arm_block, 0))
                        })
                        .collect();
                    let mut demand = LoweredDemand::merge_demands(
                        arm_demands,
                        self,
                        ReportPosition::DoNotReport,
                    );
                    demand.variables_used(self, &stmt.inputs()[..], ReportPosition::Report);
                    demand
                }
                Statement::Desnap(stmt) => {
                    let var = &self.lowered.variables[stmt.output];
                    if !var.duplicatable {
                        self.diagnostics
                            .report_by_location(var.location, DesnappingANonCopyableType);
                    }
                    continue;
                }
                Statement::Literal(_)
                | Statement::Call(_)
                | Statement::StructConstruct(_)
                | Statement::StructDestructure(_)
                | Statement::EnumConstruct(_)
                | Statement::Snapshot(_) => continue,
            };
            return Some((stmt_offset + i, demand));
        }
        None
    }
}

/// Report borrow checking diagnostics.
pub fn borrow_check(module_file_id: ModuleFileId, lowered: &mut FlatLowered) {
    let mut diagnostics = LoweringDiagnostics::new(module_file_id);
    diagnostics.diagnostics.extend(std::mem::take(&mut lowered.diagnostics));

    if let Ok(root) = lowered.root_block {
        let mut checker = BorrowChecker {
            diagnostics: &mut diagnostics,
            lowered,
            cache: Default::default(),
            new_ends: Default::default(),
            success: true,
        };
        let root_demand = checker.get_demand(None, RealBlock(root, 0));
        let success = checker.success;
        assert!(root_demand.vars.is_empty(), "Undefined variable should not happen at this stage");
        for (block_id, new_end) in checker.new_ends {
            lowered.blocks[block_id].end = new_end;
        }
        if !success {
            lowered.root_block = Err(skip_diagnostic());
        }
    }

    lowered.diagnostics = diagnostics.build();
}
