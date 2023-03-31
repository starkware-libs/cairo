use cairo_lang_defs::ids::ModuleFileId;
use cairo_lang_diagnostics::Maybe;
use itertools::{zip_eq, Itertools};

use self::analysis::{Analyzer, StatementLocation};
pub use self::demand::Demand;
use self::demand::DemandReporter;
use crate::blocks::Blocks;
use crate::borrow_check::analysis::BackAnalysis;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::LoweringDiagnostics;
use crate::{BlockId, FlatLowered, MatchInfo, Statement, VarRemapping, VariableId};

pub mod analysis;
pub mod demand;

pub type LoweredDemand = Demand<VariableId>;
pub struct BorrowChecker<'a> {
    db: &'a dyn LoweringGroup,
    diagnostics: &'a mut LoweringDiagnostics,
    lowered: &'a FlatLowered,
    success: Maybe<()>,
}

impl<'a> DemandReporter<VariableId> for BorrowChecker<'a> {
    type IntroducePosition = ();
    type UsePosition = ();

    fn drop(&mut self, _position: (), var_id: VariableId) {
        let var = &self.lowered.variables[var_id];
        let Err(drop_err) = var.droppable.clone() else { return; };
        let Err(destruct_err) = var.destruct_impl.clone() else { return; };
        self.success = Err(self
            .diagnostics
            .report_by_location(var.location, VariableNotDropped { drop_err, destruct_err }));
    }

    fn dup(&mut self, _position: (), var: VariableId) {
        let var = &self.lowered.variables[var];
        if let Err(inference_error) = var.duplicatable.clone() {
            self.success = Err(self
                .diagnostics
                .report_by_location(var.location, VariableMoved { inference_error }));
        }
    }
}

impl<'a> Analyzer<'_> for BorrowChecker<'a> {
    type Info = LoweredDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        info.variables_introduced(self, &stmt.outputs(), ());
        match stmt {
            Statement::Call(stmt) => {
                if let Ok(signature) = stmt.function.signature(self.db) {
                    if signature.panicable {
                        // Be prepared to panic here.
                        let panic_demand = LoweredDemand::default();
                        *info = LoweredDemand::merge_demands(
                            &[(panic_demand, ()), (info.clone(), ())],
                            self,
                        );
                    }
                }
            }
            Statement::Desnap(stmt) => {
                let var = &self.lowered.variables[stmt.output];
                if let Err(inference_error) = var.duplicatable.clone() {
                    self.success = Err(self.diagnostics.report_by_location(
                        var.location,
                        DesnappingANonCopyableType { inference_error },
                    ));
                }
            }
            _ => {}
        }
        info.variables_used(self, &stmt.inputs(), ());
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        info.apply_remapping(self, remapping.iter().map(|(dst, src)| (*dst, *src)));
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let arm_demands = zip_eq(match_info.arms(), infos)
            .map(|(arm, demand)| {
                let mut demand = demand.clone();
                demand.variables_introduced(self, &arm.var_ids, ());
                (demand, ())
            })
            .collect_vec();
        let mut demand = LoweredDemand::merge_demands(&arm_demands, self);
        demand.variables_used(self, &match_info.inputs(), ());
        demand
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info {
        let mut info = LoweredDemand::default();
        info.variables_used(self, vars, ());
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        data: &VariableId,
    ) -> Self::Info {
        let mut info = LoweredDemand::default();
        info.variables_used(self, &[*data], ());
        info
    }
}

/// Report borrow checking diagnostics.
pub fn borrow_check(
    db: &dyn LoweringGroup,
    module_file_id: ModuleFileId,
    lowered: &mut FlatLowered,
) {
    let mut diagnostics = LoweringDiagnostics::new(module_file_id);
    diagnostics.diagnostics.extend(std::mem::take(&mut lowered.diagnostics));

    if lowered.blocks.has_root().is_ok() {
        let checker = BorrowChecker { db, diagnostics: &mut diagnostics, lowered, success: Ok(()) };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: checker };
        let mut root_demand = analysis.get_root_info();
        root_demand.variables_introduced(&mut analysis.analyzer, &lowered.parameters, ());
        let success = analysis.analyzer.success;
        assert!(root_demand.finalize(), "Undefined variable should not happen at this stage");

        if let Err(diag_added) = success {
            lowered.blocks = Blocks::new_errored(diag_added);
        }
    }

    lowered.diagnostics = diagnostics.build();
}
