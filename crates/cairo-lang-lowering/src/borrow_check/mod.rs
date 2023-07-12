#[cfg(test)]
#[path = "test.rs"]
mod test;

use cairo_lang_defs::ids::ModuleFileId;
use cairo_lang_diagnostics::Maybe;
use itertools::{zip_eq, Itertools};

use self::analysis::{Analyzer, StatementLocation};
pub use self::demand::Demand;
use self::demand::{AuxCombine, DemandReporter};
use crate::blocks::Blocks;
use crate::borrow_check::analysis::BackAnalysis;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::LoweringDiagnostics;
use crate::ids::LocationId;
use crate::{BlockId, FlatLowered, MatchInfo, Statement, VarRemapping, VarUsage, VariableId};

pub mod analysis;
pub mod demand;

pub type BorrowCheckerDemand = Demand<VariableId, LocationId, PanicState>;
pub struct BorrowChecker<'a> {
    db: &'a dyn LoweringGroup,
    diagnostics: &'a mut LoweringDiagnostics,
    lowered: &'a FlatLowered,
    success: Maybe<()>,
}

/// A state saved for each position in the back analysis.
/// Used to determine if this flow is guaranteed to end in a panic.
#[derive(Copy, Clone, Default)]
pub enum PanicState {
    EndsWithPanic,
    #[default]
    Otherwise,
}
impl AuxCombine for PanicState {
    fn merge<'a, I: Iterator<Item = &'a Self>>(mut iter: I) -> Self
    where
        Self: 'a,
    {
        if iter.all(|x| matches!(x, Self::EndsWithPanic)) {
            Self::EndsWithPanic
        } else {
            Self::Otherwise
        }
    }
}

impl<'a> DemandReporter<VariableId, PanicState> for BorrowChecker<'a> {
    type IntroducePosition = ();
    type UsePosition = LocationId;

    fn drop_aux(&mut self, _position: (), var_id: VariableId, panic_state: PanicState) {
        let var = &self.lowered.variables[var_id];
        let Err(drop_err) = var.droppable.clone() else {
            return;
        };
        let Err(destruct_err) = var.destruct_impl.clone() else {
            return;
        };
        if matches!(panic_state, PanicState::EndsWithPanic) {
            let Err(_panic_destruct_err) = var.panic_destruct_impl.clone() else {
                return;
            };
        }
        self.success = Err(self.diagnostics.report_by_location(
            var.location.get(self.db),
            VariableNotDropped { drop_err, destruct_err },
        ));
    }

    fn dup(&mut self, _position: LocationId, var_id: VariableId, next_usage_position: LocationId) {
        let var = &self.lowered.variables[var_id];
        if let Err(inference_error) = var.duplicatable.clone() {
            self.success = Err(self.diagnostics.report_by_location(
                next_usage_position.get(self.db),
                VariableMoved { inference_error },
            ));
        }
    }
}

impl<'a> Analyzer<'_> for BorrowChecker<'a> {
    type Info = BorrowCheckerDemand;

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
                        let panic_demand = BorrowCheckerDemand {
                            aux: PanicState::EndsWithPanic,
                            ..Default::default()
                        };
                        *info = BorrowCheckerDemand::merge_demands(
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
                        var.location.get(self.db),
                        DesnappingANonCopyableType { inference_error },
                    ));
                }
            }
            _ => {}
        }
        info.variables_used(
            self,
            stmt.inputs().iter().map(|VarUsage { var_id, location }| (var_id, *location)),
        );
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        info.apply_remapping(
            self,
            remapping
                .iter()
                .map(|(dst, VarUsage { var_id: src, location })| (dst, (src, *location))),
        );
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
        let mut demand = BorrowCheckerDemand::merge_demands(&arm_demands, self);
        demand.variables_used(
            self,
            match_info.inputs().iter().map(|VarUsage { var_id, location }| (var_id, *location)),
        );
        demand
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut info = BorrowCheckerDemand::default();
        info.variables_used(
            self,
            vars.iter().map(|VarUsage { var_id, location }| (var_id, *location)),
        );
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        data: &VarUsage,
    ) -> Self::Info {
        let mut info = BorrowCheckerDemand { aux: PanicState::EndsWithPanic, ..Default::default() };
        info.variables_used(self, std::iter::once((&data.var_id, data.location)));
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
            BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: checker };
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
