#[cfg(test)]
#[path = "test.rs"]
mod test;

use cairo_lang_defs::ids::TraitFunctionId;
use cairo_lang_diagnostics::{DiagnosticNote, Maybe};
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use itertools::{Itertools, zip_eq};

use self::analysis::{Analyzer, StatementLocation};
pub use self::demand::Demand;
use self::demand::{AuxCombine, DemandReporter};
use crate::blocks::Blocks;
use crate::borrow_check::analysis::BackAnalysis;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostics, LoweringDiagnosticsBuilder};
use crate::ids::{FunctionId, LocationId, SemanticFunctionIdEx};
use crate::{BlockId, FlatLowered, MatchInfo, Statement, VarRemapping, VarUsage, VariableId};

pub mod analysis;
pub mod demand;

pub type BorrowCheckerDemand = Demand<VariableId, LocationId, PanicState>;
pub struct BorrowChecker<'a> {
    db: &'a dyn LoweringGroup,
    diagnostics: &'a mut LoweringDiagnostics,
    lowered: &'a FlatLowered,
    success: Maybe<()>,
    potential_destruct_calls: PotentialDestructCalls,
    destruct_fn: TraitFunctionId,
    panic_destruct_fn: TraitFunctionId,
    is_panic_destruct_fn: bool,
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

// Represents the item that caused the triggered the need for a drop.
#[derive(Copy, Clone, Debug)]
pub enum DropPosition {
    // The trigger is a call to a panicable function.
    Panic(LocationId),
    // The trigger is a divergence in control flow.
    Diverge(LocationId),
}
impl DropPosition {
    fn as_note(self, db: &dyn LoweringGroup) -> DiagnosticNote {
        let (text, location) = match self {
            Self::Panic(location) => {
                ("the variable needs to be dropped due to the potential panic here", location)
            }
            Self::Diverge(location) => {
                ("the variable needs to be dropped due to the divergence here", location)
            }
        };
        DiagnosticNote::with_location(
            text.into(),
            location.lookup_intern(db).stable_location.diagnostic_location(db.upcast()),
        )
    }
}

impl DemandReporter<VariableId, PanicState> for BorrowChecker<'_> {
    // Note that for in BorrowChecker `IntroducePosition` is used to pass the cause of
    // the drop.
    type IntroducePosition = (Option<DropPosition>, BlockId);
    type UsePosition = LocationId;

    fn drop_aux(
        &mut self,
        (opt_drop_position, block_id): (Option<DropPosition>, BlockId),
        var_id: VariableId,
        panic_state: PanicState,
    ) {
        let var = &self.lowered.variables[var_id];
        let Err(drop_err) = var.droppable.clone() else {
            return;
        };
        let mut add_called_fn = |impl_id, function| {
            self.potential_destruct_calls.entry(block_id).or_default().push(
                cairo_lang_semantic::FunctionLongId {
                    function: cairo_lang_semantic::ConcreteFunction {
                        generic_function: GenericFunctionId::Impl(ImplGenericFunctionId {
                            impl_id,
                            function,
                        }),
                        generic_args: vec![],
                    },
                }
                .intern(self.db)
                .lowered(self.db),
            );
        };
        let destruct_err = match var.destruct_impl.clone() {
            Ok(impl_id) => {
                add_called_fn(impl_id, self.destruct_fn);
                return;
            }
            Err(err) => err,
        };
        let panic_destruct_err = if matches!(panic_state, PanicState::EndsWithPanic) {
            match var.panic_destruct_impl.clone() {
                Ok(impl_id) => {
                    add_called_fn(impl_id, self.panic_destruct_fn);
                    return;
                }
                Err(err) => Some(err),
            }
        } else {
            None
        };

        let mut location = var.location.lookup_intern(self.db);
        if let Some(drop_position) = opt_drop_position {
            location = location.with_note(drop_position.as_note(self.db));
        }
        let semantic_db = self.db.upcast();
        self.success = Err(self.diagnostics.report_by_location(
            location
                .with_note(DiagnosticNote::text_only(drop_err.format(semantic_db)))
                .with_note(DiagnosticNote::text_only(destruct_err.format(semantic_db)))
                .maybe_with_note(
                    panic_destruct_err
                        .map(|err| DiagnosticNote::text_only(err.format(semantic_db))),
                ),
            VariableNotDropped { drop_err, destruct_err },
        ));
    }

    fn dup(&mut self, position: LocationId, var_id: VariableId, next_usage_position: LocationId) {
        let var = &self.lowered.variables[var_id];
        if let Err(inference_error) = var.copyable.clone() {
            self.success = Err(self.diagnostics.report_by_location(
                next_usage_position
                    .lookup_intern(self.db)
                    .add_note_with_location(self.db, "variable was previously used here", position)
                    .with_note(DiagnosticNote::text_only(inference_error.format(self.db.upcast()))),
                VariableMoved { inference_error },
            ));
        }
    }
}

impl Analyzer<'_> for BorrowChecker<'_> {
    type Info = BorrowCheckerDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        (block_id, _): StatementLocation,
        stmt: &Statement,
    ) {
        info.variables_introduced(self, stmt.outputs(), (None, block_id));
        match stmt {
            Statement::Call(stmt) => {
                if let Ok(signature) = stmt.function.signature(self.db) {
                    if signature.panicable {
                        // Be prepared to panic here.
                        let panic_demand = BorrowCheckerDemand {
                            aux: PanicState::EndsWithPanic,
                            ..Default::default()
                        };
                        let location = (Some(DropPosition::Panic(stmt.location)), block_id);
                        *info = BorrowCheckerDemand::merge_demands(
                            &[(panic_demand, location), (info.clone(), location)],
                            self,
                        );
                    }
                }
            }
            Statement::Desnap(stmt) => {
                let var = &self.lowered.variables[stmt.output];
                if let Err(inference_error) = var.copyable.clone() {
                    self.success = Err(self.diagnostics.report_by_location(
                        var.location.lookup_intern(self.db).with_note(DiagnosticNote::text_only(
                            inference_error.format(self.db.upcast()),
                        )),
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
        (block_id, _): StatementLocation,
        match_info: &MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let infos: Vec<_> = infos.collect();
        let arm_demands = zip_eq(match_info.arms(), &infos)
            .map(|(arm, demand)| {
                let mut demand = demand.clone();
                demand.variables_introduced(self, &arm.var_ids, (None, block_id));
                (demand, (Some(DropPosition::Diverge(*match_info.location())), block_id))
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
        let mut info = if self.is_panic_destruct_fn {
            BorrowCheckerDemand { aux: PanicState::EndsWithPanic, ..Default::default() }
        } else {
            BorrowCheckerDemand::default()
        };

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

/// The possible destruct calls per block.
pub type PotentialDestructCalls = UnorderedHashMap<BlockId, Vec<FunctionId>>;

/// Report borrow checking diagnostics.
/// Returns the potential destruct function calls per block.
pub fn borrow_check(
    db: &dyn LoweringGroup,
    is_panic_destruct_fn: bool,
    lowered: &mut FlatLowered,
) -> PotentialDestructCalls {
    if lowered.blocks.has_root().is_err() {
        return Default::default();
    }
    let mut diagnostics = LoweringDiagnostics::default();
    diagnostics.extend(std::mem::take(&mut lowered.diagnostics));
    let info = db.core_info();
    let destruct_fn = info.destruct_fn;
    let panic_destruct_fn = info.panic_destruct_fn;

    let checker = BorrowChecker {
        db,
        diagnostics: &mut diagnostics,
        lowered,
        success: Ok(()),
        potential_destruct_calls: Default::default(),
        destruct_fn,
        panic_destruct_fn,
        is_panic_destruct_fn,
    };
    let mut analysis = BackAnalysis::new(lowered, checker);
    let mut root_demand = analysis.get_root_info();
    root_demand.variables_introduced(
        &mut analysis.analyzer,
        &lowered.parameters,
        (None, BlockId::root()),
    );
    let block_extra_calls = analysis.analyzer.potential_destruct_calls;
    let success = analysis.analyzer.success;
    assert!(root_demand.finalize(), "Undefined variable should not happen at this stage");

    if let Err(diag_added) = success {
        lowered.blocks = Blocks::new_errored(diag_added);
    }

    lowered.diagnostics = diagnostics.build();
    block_extra_calls
}
