//! This module implements the destructor call addition. It is assumed to run after the panic phase.
//! This is similar to the borrow checking algorithm, except we handle "undroppable drops" by adding
//! destructor calls.

use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::corelib::{get_core_trait, unit_ty};
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_semantic::items::imp::ImplId;
use cairo_lang_semantic::ConcreteFunction;
use itertools::{zip_eq, Itertools};
use semantic::corelib::{core_module, get_ty_by_name};
use semantic::{TypeId, TypeLongId};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::{AuxCombine, DemandReporter};
use crate::borrow_check::Demand;
use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, SemanticFunctionIdEx};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    BlockId, FlatLowered, MatchInfo, Statement, StatementCall, StatementStructConstruct,
    StatementStructDestructure, VarRemapping, VarUsage, VariableId,
};

pub type DestructAdderDemand = Demand<VariableId, (), PanicState>;

/// Context for the dectructor call addition phase,
pub struct DestructAdder<'a> {
    db: &'a dyn LoweringGroup,
    lowered: &'a FlatLowered,
    destructions: Vec<DestructionEntry>,
    panic_ty: TypeId,
}

/// A destructor call that needs to be added.
enum DestructionEntry {
    /// A normal destructor call.
    Plain(PlainDestructionEntry),
    /// A panic destructor call.
    Panic(PanicDeconstructionEntry),
}
struct PlainDestructionEntry {
    position: StatementLocation,
    var_id: VariableId,
    impl_id: ImplId,
}
struct PanicDeconstructionEntry {
    panic_location: PanicLocation,
    var_id: VariableId,
    impl_id: ImplId,
}

impl<'a> DemandReporter<VariableId, PanicState> for DestructAdder<'a> {
    type IntroducePosition = StatementLocation;
    type UsePosition = ();

    fn drop_aux(
        &mut self,
        position: StatementLocation,
        var_id: VariableId,
        panic_state: PanicState,
    ) {
        let var = &self.lowered.variables[var_id];
        if var.droppable.is_ok() {
            return;
        };
        // If a non droppable variable gets out of scope, add a destruct call for it.
        if let Ok(impl_id) = var.destruct_impl.clone() {
            self.destructions.push(DestructionEntry::Plain(PlainDestructionEntry {
                position,
                var_id,
                impl_id,
            }));
            return;
        }
        // If a non destructible variable gets out of scope, add a panic_destruct call for it.
        if let Ok(impl_id) = var.panic_destruct_impl.clone() {
            if let PanicState::EndsWithPanic(panic_locations) = panic_state {
                for panic_location in panic_locations {
                    self.destructions.push(DestructionEntry::Panic(PanicDeconstructionEntry {
                        panic_location,
                        var_id,
                        impl_id,
                    }));
                }
                return;
            }
        }

        panic!("Borrow checker should have caught this.")
    }
}

/// A state saved for each position in the back analysis.
/// Used to determine if a Panic object is guaranteed to exist or be created, an where.
#[derive(Clone, Default)]
pub enum PanicState {
    /// The flow will end with a panic. The locations are all the possible places a Panic object
    /// can be created from this flow.
    /// The flow is guaranteed to end up in one of these places.
    EndsWithPanic(Vec<PanicLocation>),
    #[default]
    Otherwise,
}
/// How to combine two panic states in a flow divergence.
impl AuxCombine for PanicState {
    fn merge<'a, I: Iterator<Item = &'a Self>>(iter: I) -> Self
    where
        Self: 'a,
    {
        let mut panic_locations = vec![];
        for state in iter {
            if let Self::EndsWithPanic(locations) = state {
                panic_locations.extend_from_slice(locations);
            } else {
                return Self::Otherwise;
            }
        }
        Self::EndsWithPanic(panic_locations)
    }
}

/// Location where a `Panic` is first available.
#[derive(Clone)]
pub enum PanicLocation {
    /// The `Panic` value is at a variable.
    PanicVar { panic_var: VariableId, statement_location: StatementLocation },
    /// The `Panic` value is the first value in a tuple.
    PanicTuple { tuple_var: VariableId, statement_location: StatementLocation },
}

impl<'a> Analyzer<'_> for DestructAdder<'a> {
    type Info = DestructAdderDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        (block_id, statement_index): StatementLocation,
        stmt: &Statement,
    ) {
        self.update_panic_state(&stmt.outputs(), info, block_id, statement_index + 1);
        info.variables_introduced(
            self,
            &stmt.outputs(),
            // Since we need to insert destructor call right after the statement.
            (block_id, statement_index + 1),
        );
        info.variables_used(self, stmt.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())));
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        info.apply_remapping(self, remapping.iter().map(|(dst, src)| (dst, (&src.var_id, ()))));
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
                self.update_panic_state(&arm.var_ids, &mut demand, arm.block_id, 0);
                demand.variables_introduced(self, &arm.var_ids, use_position);
                (demand, use_position)
            })
            .collect_vec();
        let mut demand = DestructAdderDemand::merge_demands(&arm_demands, self);
        demand.variables_used(
            self,
            match_info.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
        demand
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut info = DestructAdderDemand::default();
        info.variables_used(self, vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())));
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VarUsage,
    ) -> Self::Info {
        unreachable!("Panic should have been lowered.")
    }
}

impl<'a> DestructAdder<'a> {
    fn update_panic_state(
        &mut self,
        introductions: &[VariableId],
        info: &mut DestructAdderDemand,
        block_id: BlockId,
        statement_index: usize,
    ) {
        for output in introductions {
            let var = &self.lowered.variables[*output];
            if var.ty == self.panic_ty {
                info.aux = PanicState::EndsWithPanic(vec![PanicLocation::PanicVar {
                    panic_var: *output,
                    statement_location: (block_id, statement_index),
                }]);
            }
            let long_ty = self.db.lookup_intern_type(var.ty);
            let TypeLongId::Tuple(tys) = long_ty else { continue };
            if tys.first() == Some(&self.panic_ty) {
                info.aux = PanicState::EndsWithPanic(vec![PanicLocation::PanicTuple {
                    tuple_var: *output,
                    statement_location: (block_id, statement_index),
                }]);
            }
        }
    }
}

fn panic_ty(db: &dyn LoweringGroup) -> semantic::TypeId {
    get_ty_by_name(db.upcast(), core_module(db.upcast()), "Panic".into(), vec![])
}

/// Report borrow checking diagnostics.
pub fn add_destructs(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) {
    if lowered.blocks.has_root().is_ok() {
        let checker = DestructAdder { db, lowered, destructions: vec![], panic_ty: panic_ty(db) };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: checker };
        let mut root_demand = analysis.get_root_info();
        root_demand.variables_introduced(
            &mut analysis.analyzer,
            &lowered.parameters,
            (BlockId::root(), 0),
        );
        assert!(root_demand.finalize(), "Undefined variable should not happen at this stage");

        let mut variables = VariableAllocator::new(
            db,
            function_id.function_with_body_id(db).base_semantic_function(db),
            lowered.variables.clone(),
        )
        .unwrap();

        let destruct_trait_id = get_core_trait(db.upcast(), "Destruct".into());
        let plain_trait_function =
            db.trait_function_by_name(destruct_trait_id, "destruct".into()).unwrap().unwrap();
        let panic_destruct_trait_id = get_core_trait(db.upcast(), "PanicDestruct".into());
        let panic_trait_function = db
            .trait_function_by_name(panic_destruct_trait_id, "panic_destruct".into())
            .unwrap()
            .unwrap();

        // Add destructions.
        let stable_ptr = function_id
            .function_with_body_id(db.upcast())
            .base_semantic_function(db)
            .untyped_stable_ptr(db.upcast());
        for destruction in analysis.analyzer.destructions {
            let location = variables.get_location(stable_ptr);
            let output_var = variables.new_var(VarRequest { ty: unit_ty(db.upcast()), location });
            match destruction {
                DestructionEntry::Plain(PlainDestructionEntry {
                    position: (block_id, insert_index),
                    var_id,
                    impl_id,
                }) => {
                    let semantic_function = db.intern_function(semantic::FunctionLongId {
                        function: ConcreteFunction {
                            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId {
                                impl_id,
                                function: plain_trait_function,
                            }),
                            generic_args: vec![],
                        },
                    });
                    lowered.blocks[block_id].statements.insert(
                        insert_index,
                        Statement::Call(StatementCall {
                            function: semantic_function.lowered(db),
                            inputs: vec![VarUsage { var_id, location }],
                            outputs: vec![output_var],
                            location: lowered.variables[var_id].location,
                        }),
                    )
                }
                DestructionEntry::Panic(PanicDeconstructionEntry {
                    panic_location,
                    var_id,
                    impl_id,
                }) => {
                    let semantic_function = db.intern_function(semantic::FunctionLongId {
                        function: ConcreteFunction {
                            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId {
                                impl_id,
                                function: panic_trait_function,
                            }),
                            generic_args: vec![],
                        },
                    });
                    match panic_location {
                        PanicLocation::PanicVar {
                            panic_var,
                            statement_location: (block_id, insert_index),
                        } => lowered.blocks[block_id].statements.insert(
                            insert_index,
                            Statement::Call(StatementCall {
                                function: semantic_function.lowered(db),
                                inputs: vec![panic_var, var_id]
                                    .into_iter()
                                    .map(|var_id| VarUsage { var_id, location })
                                    .collect(),
                                outputs: vec![panic_var, output_var],
                                location: lowered.variables[panic_var].location,
                            }),
                        ),
                        PanicLocation::PanicTuple {
                            tuple_var,
                            statement_location: (block_id, insert_index),
                        } => {
                            let long_ty = db.lookup_intern_type(lowered.variables[tuple_var].ty);
                            let TypeLongId::Tuple(tys) = long_ty else { unreachable!() };

                            let location = variables.get_location(stable_ptr);
                            let vars = tys
                                .iter()
                                .copied()
                                .map(|ty| variables.new_var(VarRequest { ty, location }))
                                .collect::<Vec<_>>();
                            let output_var = variables
                                .new_var(VarRequest { ty: unit_ty(db.upcast()), location });
                            let statements = vec![
                                Statement::StructDestructure(StatementStructDestructure {
                                    input: VarUsage { var_id: tuple_var, location },
                                    outputs: vars.clone(),
                                }),
                                Statement::Call(StatementCall {
                                    function: semantic_function.lowered(db),
                                    inputs: vec![
                                        VarUsage { var_id: vars[0], location },
                                        VarUsage { var_id, location },
                                    ],
                                    outputs: vec![vars[0], output_var],
                                    location: lowered.variables[tuple_var].location,
                                }),
                                Statement::StructConstruct(StatementStructConstruct {
                                    inputs: vars
                                        .into_iter()
                                        .map(|var_id| VarUsage { var_id, location })
                                        .collect(),
                                    output: tuple_var,
                                }),
                            ];
                            lowered.blocks[block_id]
                                .statements
                                .splice(insert_index..insert_index, statements.into_iter());
                        }
                    }
                }
            }
        }
        lowered.variables = variables.variables;
    }
}
