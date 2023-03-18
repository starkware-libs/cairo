use cairo_lang_defs::ids::ModuleFileId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::get_core_trait;
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_semantic::items::imp::ImplId;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use itertools::{zip_eq, Itertools};

use self::analysis::{Analyzer, StatementLocation};
pub use self::demand::Demand;
use self::demand::DemandReporter;
use crate::blocks::Blocks;
use crate::borrow_check::analysis::BackAnalysis;
use crate::db::LoweringGroup;
use crate::diagnostic::LoweringDiagnosticKind::*;
use crate::diagnostic::LoweringDiagnostics;
use crate::{BlockId, FlatLowered, MatchInfo, Statement, StatementCall, VarRemapping, VariableId};

pub mod analysis;
pub mod demand;

pub type LoweredDemand = Demand<VariableId>;
pub struct BorrowChecker<'a> {
    diagnostics: &'a mut LoweringDiagnostics,
    lowered: &'a FlatLowered,
    success: Maybe<()>,
    destructions: Vec<DestructionEntry>,
}
struct DestructionEntry {
    position: StatementLocation,
    var_id: VariableId,
    impl_id: ImplId,
}

impl<'a> DemandReporter<VariableId> for BorrowChecker<'a> {
    type IntroducePosition = StatementLocation;
    type UsePosition = ();

    fn drop(&mut self, position: StatementLocation, var_id: VariableId) {
        let var = &self.lowered.variables[var_id];
        let Err(drop_err) = var.droppable.clone() else { return; };
        match var.destruct_impl.clone() {
            Ok(impl_id) => {
                // Call it
                self.destructions.push(DestructionEntry { position, var_id, impl_id });
            }
            Err(destruct_err) => {
                self.success = Err(self.diagnostics.report_by_location(
                    var.location,
                    VariableNotDropped { drop_err, destruct_err },
                ));
            }
        }
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

impl<'a> Analyzer for BorrowChecker<'a> {
    type Info = LoweredDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        // TODO(spapini): Handle calls to panicable functions.
        if let Statement::Desnap(stmt) = stmt {
            let var = &self.lowered.variables[stmt.output];
            if let Err(inference_error) = var.duplicatable.clone() {
                self.success = Err(self.diagnostics.report_by_location(
                    var.location,
                    DesnappingANonCopyableType { inference_error },
                ));
            }
        }
        info.variables_introduced(self, &stmt.outputs(), statement_location);
        info.variables_used(self, &stmt.inputs(), ());
    }

    fn visit_remapping(
        &mut self,
        info: &mut Self::Info,
        _block_id: BlockId,
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
                let use_position = (arm.block_id, 0);
                demand.variables_introduced(self, &arm.var_ids, use_position);
                (demand, use_position)
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
        let checker = BorrowChecker {
            diagnostics: &mut diagnostics,
            lowered,
            success: Ok(()),
            destructions: vec![],
        };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: checker };
        let mut root_demand = analysis.get_root_info();
        root_demand.variables_introduced(
            &mut analysis.analyzer,
            &lowered.parameters,
            (BlockId::root(), 0),
        );
        let success = analysis.analyzer.success;
        assert!(root_demand.finalize(), "Undefined variable should not happen at this stage");

        let trait_id = get_core_trait(db.upcast(), "Destruct".into());
        let trait_function =
            db.trait_function_by_name(trait_id, "destruct".into()).unwrap().unwrap();

        if let Err(diag_added) = success {
            lowered.blocks = Blocks::new_errored(diag_added);
        } else {
            // Add destructions.
            // TODO(spapini): Add destructors after panic phase.
            //  Here, only do the checking.
            for destruction in analysis.analyzer.destructions {
                let DestructionEntry { position: (block_id, statement_offset), var_id, impl_id } =
                    destruction;
                lowered.blocks[block_id].statements.insert(
                    statement_offset,
                    Statement::Call(StatementCall {
                        function: db.intern_function(FunctionLongId {
                            function: ConcreteFunction {
                                generic_function: GenericFunctionId::Impl(ImplGenericFunctionId {
                                    impl_id,
                                    function: trait_function,
                                }),
                                generic_args: vec![],
                            },
                        }),
                        inputs: vec![var_id],
                        outputs: vec![],
                        location: lowered.variables[var_id].location,
                    }),
                )
            }
        }
    }

    lowered.diagnostics = diagnostics.build();
}
