#[cfg(test)]
#[path = "test.rs"]
mod test;

use cairo_lang_defs::ids::TraitFunctionId;
use cairo_lang_diagnostics::{DiagnosticNote, Diagnostics};
use cairo_lang_semantic::corelib::CorelibSemantic;
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_utils::Intern;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{Itertools, zip_eq};
use salsa::Database;

use self::analysis::{Analyzer, StatementLocation};
pub use self::demand::Demand;
use self::demand::{AuxCombine, DemandReporter};
use crate::borrow_check::analysis::BackAnalysis;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics, LoweringDiagnosticsBuilder};
use crate::ids::{FunctionId, LocationId, SemanticFunctionIdEx};
use crate::{BlockId, Lowered, MatchInfo, Statement, VarRemapping, VarUsage, VariableId};

pub mod analysis;
pub mod demand;

pub type BorrowCheckerDemand<'db> = Demand<VariableId, LocationId<'db>, PanicState>;
pub struct BorrowChecker<'db, 'mt, 'r> {
    db: &'db dyn Database,
    diagnostics: &'mt mut LoweringDiagnostics<'db>,
    lowered: &'r Lowered<'db>,
    potential_destruct_calls: PotentialDestructCalls<'db>,
    destruct_fn: TraitFunctionId<'db>,
    panic_destruct_fn: TraitFunctionId<'db>,
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

// Represents the item that caused the need for a drop.
#[derive(Copy, Clone, Debug)]
pub enum DropPosition<'db> {
    // The trigger is a call to a panicable function.
    Panic(LocationId<'db>),
    // The trigger is a divergence in control flow.
    Diverge(LocationId<'db>),
}
impl<'db> DropPosition<'db> {
    fn enrich_as_notes(self, db: &'db dyn Database, notes: &mut Vec<DiagnosticNote<'db>>) {
        let (text, location) = match self {
            Self::Panic(location) => {
                ("the variable needs to be dropped due to the potential panic here", location)
            }
            Self::Diverge(location) => {
                ("the variable needs to be dropped due to the divergence here", location)
            }
        };
        let location = location.long(db);
        notes.push(DiagnosticNote::with_location(
            text.into(),
            location.stable_location.diagnostic_location(db),
        ));
        notes.extend(location.notes.clone());
    }
}

impl<'db, 'mt> DemandReporter<VariableId, PanicState> for BorrowChecker<'db, 'mt, '_> {
    // Note that for in BorrowChecker `IntroducePosition` is used to pass the cause of
    // the drop.
    type IntroducePosition = (Option<DropPosition<'db>>, BlockId);
    type UsePosition = LocationId<'db>;

    fn drop_aux(
        &mut self,
        (opt_drop_position, block_id): (Option<DropPosition<'db>>, BlockId),
        var_id: VariableId,
        panic_state: PanicState,
    ) {
        let var = &self.lowered.variables[var_id];
        let Err(drop_err) = var.info.droppable.clone() else {
            return;
        };
        let db = self.db;
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
                .intern(db)
                .lowered(db),
            );
        };
        let destruct_err = match var.info.destruct_impl.clone() {
            Ok(impl_id) => {
                add_called_fn(impl_id, self.destruct_fn);
                return;
            }
            Err(err) => err,
        };
        let panic_destruct_err = if matches!(panic_state, PanicState::EndsWithPanic) {
            match var.info.panic_destruct_impl.clone() {
                Ok(impl_id) => {
                    add_called_fn(impl_id, self.panic_destruct_fn);
                    return;
                }
                Err(err) => Some(err),
            }
        } else {
            None
        };

        let mut location = var.location.long(db).clone();
        if let Some(drop_position) = opt_drop_position {
            drop_position.enrich_as_notes(db, &mut location.notes);
        }
        self.diagnostics.report_by_location(
            location
                .with_note(DiagnosticNote::text_only(drop_err.format(db)))
                .with_note(DiagnosticNote::text_only(destruct_err.format(db)))
                .maybe_with_note(
                    panic_destruct_err.map(|err| DiagnosticNote::text_only(err.format(db))),
                ),
            VariableNotDropped { drop_err, destruct_err },
        );
    }

    fn dup(
        &mut self,
        position: LocationId<'db>,
        var_id: VariableId,
        next_usage_position: LocationId<'db>,
    ) {
        let var = &self.lowered.variables[var_id];
        if let Err(inference_error) = var.info.copyable.clone() {
            self.diagnostics.report_by_location(
                next_usage_position
                    .long(self.db)
                    .clone()
                    .add_note_with_location(self.db, "variable was previously used here", position)
                    .with_note(DiagnosticNote::text_only(inference_error.format(self.db))),
                VariableMoved { inference_error },
            );
        }
    }
}

impl<'db, 'mt> Analyzer<'db, '_> for BorrowChecker<'db, 'mt, '_> {
    type Info = BorrowCheckerDemand<'db>;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        (block_id, _): StatementLocation,
        stmt: &Statement<'db>,
    ) {
        info.variables_introduced(self, stmt.outputs(), (None, block_id));
        match stmt {
            Statement::Call(stmt) => {
                if let Ok(signature) = stmt.function.signature(self.db)
                    && signature.panicable
                {
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
            Statement::Desnap(stmt) => {
                let var = &self.lowered.variables[stmt.output];
                if let Err(inference_error) = var.info.copyable.clone() {
                    self.diagnostics.report_by_location(
                        var.location
                            .long(self.db)
                            .clone()
                            .with_note(DiagnosticNote::text_only(inference_error.format(self.db))),
                        DesnappingANonCopyableType { inference_error },
                    );
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
        remapping: &VarRemapping<'db>,
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
        match_info: &MatchInfo<'db>,
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
        vars: &[VarUsage<'db>],
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
        data: &VarUsage<'db>,
    ) -> Self::Info {
        let mut info = BorrowCheckerDemand { aux: PanicState::EndsWithPanic, ..Default::default() };
        info.variables_used(self, std::iter::once((&data.var_id, data.location)));
        info
    }
}

/// The possible destruct calls per block.
pub type PotentialDestructCalls<'db> = UnorderedHashMap<BlockId, Vec<FunctionId<'db>>>;

/// The borrow checker result.
#[derive(Eq, PartialEq, Debug, Default, salsa::Update)]
pub struct BorrowCheckResult<'db> {
    /// The possible destruct calls per block.
    pub block_extra_calls: PotentialDestructCalls<'db>,
    /// The diagnostics generated during borrow checking.
    pub diagnostics: Diagnostics<'db, LoweringDiagnostic<'db>>,
}

/// Report borrow checking diagnostics.
/// Returns the potential destruct function calls per block.
pub fn borrow_check<'db>(
    db: &'db dyn Database,
    is_panic_destruct_fn: bool,
    lowered: &'db Lowered<'db>,
) -> BorrowCheckResult<'db> {
    if lowered.blocks.has_root().is_err() {
        return Default::default();
    }
    let mut diagnostics = LoweringDiagnostics::default();
    let info = db.core_info();
    let destruct_fn = info.destruct_fn;
    let panic_destruct_fn = info.panic_destruct_fn;

    let checker = BorrowChecker {
        db,
        diagnostics: &mut diagnostics,
        lowered,
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

    let finalize_res = root_demand.finalize();
    // If there are errors in the lowering phase, there may be undefined variables (e.g., due to
    // using a moved variable). Skip the following assert in that case.
    if !lowered.diagnostics.has_errors() {
        assert!(finalize_res, "Undefined variable should not happen at this stage");
    }

    BorrowCheckResult { block_extra_calls, diagnostics: diagnostics.build() }
}

/// Borrow check the params of the function are panic destruct, as this function may have a gas
/// withdrawal.
pub fn borrow_check_possible_withdraw_gas<'db>(
    db: &'db dyn Database,
    location_id: LocationId<'db>,
    lowered: &Lowered<'db>,
    diagnostics: &mut LoweringDiagnostics<'db>,
) {
    let info = db.core_info();
    let destruct_fn = info.destruct_fn;
    let panic_destruct_fn = info.panic_destruct_fn;
    let mut checker = BorrowChecker {
        db,
        diagnostics,
        lowered,
        potential_destruct_calls: Default::default(),
        destruct_fn,
        panic_destruct_fn,
        is_panic_destruct_fn: false,
    };
    let position = (
        Some(DropPosition::Panic(location_id.with_auto_generation_note(db, "withdraw_gas"))),
        BlockId::root(),
    );
    for param in &lowered.parameters {
        checker.drop_aux(position, *param, PanicState::EndsWithPanic);
    }
}
