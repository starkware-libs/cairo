use cairo_lang_defs::ids::ModuleFileId;
use cairo_lang_diagnostics::skip_diagnostic;
use itertools::Itertools;

use self::analysis::Analyzer;
pub use self::demand::Demand;
use self::demand::DemandReporter;
use crate::borrow_check::analysis::BackAnalysis;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::LoweringDiagnostics;
use crate::{FlatLowered, Statement, VarRemapping, VariableId};

pub mod analysis;
pub mod demand;

pub type LoweredDemand = Demand<VariableId>;
pub struct BorrowChecker<'a> {
    diagnostics: &'a mut LoweringDiagnostics,
    lowered: &'a FlatLowered,
    success: bool,
}

impl<'a> DemandReporter<VariableId> for BorrowChecker<'a> {
    type IntroducePosition = ReportPosition;
    type UsePosition = ();

    fn drop(&mut self, position: ReportPosition, var: VariableId) {
        let var = &self.lowered.variables[var];
        if matches!(position, ReportPosition::Report) && !var.droppable {
            self.diagnostics.report_by_location(var.location, VariableNotDropped);
            // Currently some reporting is disabled, since Drop is not properly implemented
            // everywhere.

            // TODO(spapini): self.success = false;
        }
    }

    fn dup(&mut self, _position: (), var: VariableId) {
        let var = &self.lowered.variables[var];
        if !var.duplicatable {
            self.diagnostics.report_by_location(var.location, VariableMoved);
            self.success = false;
        }
    }

    fn last_use(&mut self, _position: Self::UsePosition, _var: VariableId) {}
}

/// The position associated with the demand reporting. This is provided at every demand computation,
/// and passed to the reporter when needed. This is used here to specify if we want diagnostics
/// to be reported at the location or not.
#[derive(Copy, Clone)]
pub enum ReportPosition {
    Report,
    // Currently some reporting is disabled, since Drop is not properly implemented everywhere.
    // TODO(spapini): Fix this.
    DoNotReport,
}

impl<'a> Analyzer for BorrowChecker<'a> {
    type Info = LoweredDemand;

    fn visit_block_start(&mut self, info: &mut Self::Info, block: &crate::FlatBlock) {
        info.variables_introduced(self, &block.inputs, ReportPosition::Report);
    }

    fn visit_stmt(&mut self, info: &mut Self::Info, stmt: &Statement) {
        if let Statement::Desnap(stmt) = stmt {
            let var = &self.lowered.variables[stmt.output];
            if !var.duplicatable {
                self.diagnostics.report_by_location(var.location, DesnappingANonCopyableType);
            }
        }
        info.variables_introduced(self, &stmt.outputs(), ReportPosition::Report);
        info.variables_used(self, &stmt.inputs(), ());
    }

    fn visit_remapping(&mut self, info: &mut Self::Info, remapping: &VarRemapping) {
        info.apply_remapping(remapping.iter().map(|(dst, src)| (*dst, *src)));
    }

    fn merge_match(&mut self, stmt: &Statement, arms: &[Self::Info]) -> Self::Info {
        let arm_demands =
            arms.iter().map(|demand| (demand.clone(), ReportPosition::DoNotReport)).collect_vec();
        let mut info = LoweredDemand::merge_demands(&arm_demands, self);
        info.variables_used(self, &stmt.inputs(), ());
        info
    }

    fn info_from_return(&mut self, vars: &[VariableId]) -> Self::Info {
        LoweredDemand::return_demand(vars)
    }

    fn info_from_unreachable(&mut self) -> Self::Info {
        LoweredDemand::default()
    }
}

/// Report borrow checking diagnostics.
pub fn borrow_check(module_file_id: ModuleFileId, lowered: &mut FlatLowered) {
    let mut diagnostics = LoweringDiagnostics::new(module_file_id);
    diagnostics.diagnostics.extend(std::mem::take(&mut lowered.diagnostics));

    if lowered.root_block.is_ok() {
        let checker = BorrowChecker { diagnostics: &mut diagnostics, lowered, success: true };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: checker };
        let root_demand = analysis.get_root_info();
        let success = analysis.analyzer.success;
        assert!(root_demand.finalize(), "Undefined variable should not happen at this stage");
        if !success {
            lowered.root_block = Err(skip_diagnostic());
        }
    }

    lowered.diagnostics = diagnostics.build();
}
