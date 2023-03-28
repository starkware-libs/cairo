//! This module implements the destructor call addition. It is assumed to run after the panic phase.
//! This is similar to the borrow checking algorithm, except we handle "undroppable drops" by adding
//! destructor calls.

use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_semantic::corelib::{get_core_trait, unit_ty};
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_semantic::items::imp::ImplId;
use cairo_lang_semantic::{ConcreteFunction, ConcreteFunctionWithBodyId, FunctionLongId};
use itertools::{zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::DemandReporter;
use crate::borrow_check::Demand;
use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContextBuilder, VarRequest};
use crate::{BlockId, FlatLowered, MatchInfo, Statement, StatementCall, VarRemapping, VariableId};

pub type LoweredDemand = Demand<VariableId>;

/// Context for the dectructor call addition phase,
pub struct DestructAdder<'a> {
    lowered: &'a FlatLowered,
    destructions: Vec<DestructionEntry>,
}

// A destructr call that needs to be added.
struct DestructionEntry {
    position: StatementLocation,
    var_id: VariableId,
    impl_id: ImplId,
}

impl<'a> DemandReporter<VariableId> for DestructAdder<'a> {
    type IntroducePosition = StatementLocation;
    type UsePosition = ();

    fn drop(&mut self, position: StatementLocation, var_id: VariableId) {
        let var = &self.lowered.variables[var_id];
        if var.droppable.is_ok() {
            return;
        };
        // If we a non droppable variable gets our of scope, add a destructor call for it.
        if let Ok(impl_id) = var.destruct_impl.clone() {
            self.destructions.push(DestructionEntry { position, var_id, impl_id });
        }

        // TODO(spapini): Panic here when everything works.
    }

    fn dup(&mut self, _position: (), _var: VariableId) {}
}

impl<'a> Analyzer<'_> for DestructAdder<'a> {
    type Info = LoweredDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        (block_id, statement_index): StatementLocation,
        stmt: &Statement,
    ) {
        info.variables_introduced(
            self,
            &stmt.outputs(),
            // Since we need to insert destructor call right after the statement.
            (block_id, statement_index + 1),
        );
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
pub fn add_destructs(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) {
    if lowered.blocks.has_root().is_ok() {
        let checker = DestructAdder { lowered, destructions: vec![] };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: checker };
        let mut root_demand = analysis.get_root_info();
        root_demand.variables_introduced(
            &mut analysis.analyzer,
            &lowered.parameters,
            (BlockId::root(), 0),
        );
        assert!(root_demand.finalize(), "Undefined variable should not happen at this stage");

        let generic_function_id = function_id.function_with_body_id(db.upcast());
        let lowering_info = LoweringContextBuilder::new(db, generic_function_id).unwrap();
        let mut lowering_ctx = lowering_info.ctx().unwrap();
        lowering_ctx.variables = lowered.variables.clone();

        let trait_id = get_core_trait(db.upcast(), "Destruct".into());
        let trait_function =
            db.trait_function_by_name(trait_id, "destruct".into()).unwrap().unwrap();

        // Add destructions.
        for destruction in analysis.analyzer.destructions {
            let output_var = lowering_ctx.new_var(VarRequest {
                ty: unit_ty(db.upcast()),
                location: lowering_ctx
                    .get_location(generic_function_id.untyped_stable_ptr(db.upcast())),
            });
            let DestructionEntry { position: (block_id, insert_index), var_id, impl_id } =
                destruction;
            lowered.blocks[block_id].statements.insert(
                insert_index,
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
                    outputs: vec![output_var],
                    location: lowered.variables[var_id].location,
                }),
            )
        }
        lowered.variables = std::mem::take(&mut lowering_ctx.variables);
    }
}
