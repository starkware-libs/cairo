//! This module implements the destructor call addition.
//!
//! It is assumed to run after the panic phase.
//! This is similar to the borrow checking algorithm, except we handle "undroppable drops" by adding
//! destructor calls.

use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::ConcreteFunction;
use cairo_lang_semantic::corelib::{core_module, get_ty_by_name, unit_ty};
use cairo_lang_semantic::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use cairo_lang_semantic::items::imp::ImplId;
use cairo_lang_utils::{Intern, LookupIntern, extract_matches};
use itertools::{Itertools, chain, zip_eq};
use semantic::{TypeId, TypeLongId};

use crate::borrow_check::Demand;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::{AuxCombine, DemandReporter};
use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, SemanticFunctionIdEx};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchInfo, Statement, StatementCall,
    StatementStructConstruct, StatementStructDestructure, VarRemapping, VarUsage, VariableId,
};

pub type DestructAdderDemand = Demand<VariableId, (), PanicState>;

/// The add destruct flow type, used for grouping of destruct calls.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum AddDestructFlowType {
    /// Plain destruct
    Plain,
    /// Panic destruct following the creation of a panic variable (or return of a panic variable)
    PanicVar,
    /// Panic destruct following a match of PanicResult.
    PanicPostMatch,
}

/// Context for the destructor call addition phase,
pub struct DestructAdder<'a> {
    db: &'a dyn LoweringGroup,
    lowered: &'a FlatLowered,
    destructions: Vec<DestructionEntry>,
    panic_ty: TypeId,
    is_panic_destruct_fn: bool,
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

impl DestructAdder<'_> {
    /// Checks if the statement introduces a panic variable and sets the panic state accordingly.
    fn set_post_stmt_destruct(
        &mut self,
        introductions: &[VariableId],
        info: &mut DestructAdderDemand,
        block_id: BlockId,
        statement_index: usize,
    ) {
        if let [panic_var] = introductions[..] {
            let var = &self.lowered.variables[panic_var];
            if var.ty == self.panic_ty {
                info.aux = PanicState::EndsWithPanic(vec![PanicLocation::PanicVar {
                    statement_location: (block_id, statement_index),
                }]);
            }
        }
    }

    /// Check if the match arm introduces a `PanicResult::Err` variable and sets the panic state
    /// accordingly.
    fn set_post_match_state(
        &mut self,
        introduced_vars: &[VariableId],
        info: &mut DestructAdderDemand,
        match_block_id: BlockId,
        target_block_id: BlockId,
        arm_idx: usize,
    ) {
        if arm_idx != 1 {
            // The post match panic should be on the second arm of a match on a PanicResult.
            return;
        }
        if let [err_var] = introduced_vars[..] {
            let var = &self.lowered.variables[err_var];

            let long_ty = var.ty.lookup_intern(self.db);
            let TypeLongId::Tuple(tys) = long_ty else {
                return;
            };
            if tys.first() == Some(&self.panic_ty) {
                info.aux = PanicState::EndsWithPanic(vec![PanicLocation::PanicMatch {
                    match_block_id,
                    target_block_id,
                }]);
            }
        }
    }
}

impl DemandReporter<VariableId, PanicState> for DestructAdder<'_> {
    type IntroducePosition = StatementLocation;
    type UsePosition = ();

    fn drop_aux(
        &mut self,
        position: StatementLocation,
        var_id: VariableId,
        panic_state: PanicState,
    ) {
        let var = &self.lowered.variables[var_id];
        // Note that droppable here means droppable before monomorphization.
        // I.e. it is possible that T was substituted with a unit type, but T was not droppable
        // and therefore the unit type var is not droppable here.
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
/// Used to determine if a Panic object is guaranteed to exist or be created, and where.
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
    /// The `Panic` value is at a variable created by a StructConstruct at `statement_location`.
    PanicVar { statement_location: StatementLocation },
    /// The `Panic` is inside a PanicResult::Err that was create by a match at `match_block_id`.
    PanicMatch { match_block_id: BlockId, target_block_id: BlockId },
}

impl Analyzer<'_> for DestructAdder<'_> {
    type Info = DestructAdderDemand;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        (block_id, statement_index): StatementLocation,
        stmt: &Statement,
    ) {
        self.set_post_stmt_destruct(stmt.outputs(), info, block_id, statement_index);
        // Since we need to insert destructor call right after the statement.
        info.variables_introduced(self, stmt.outputs(), (block_id, statement_index + 1));
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
        (block_id, _statement_index): StatementLocation,
        match_info: &MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let arm_demands = zip_eq(match_info.arms(), infos)
            .enumerate()
            .map(|(arm_idx, (arm, mut demand))| {
                let use_position = (arm.block_id, 0);
                self.set_post_match_state(
                    &arm.var_ids,
                    &mut demand,
                    block_id,
                    arm.block_id,
                    arm_idx,
                );
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
        statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut info = DestructAdderDemand::default();
        // Allow panic destructors to be called inside panic destruct functions.
        if self.is_panic_destruct_fn {
            info.aux =
                PanicState::EndsWithPanic(vec![PanicLocation::PanicVar { statement_location }]);
        }

        info.variables_used(self, vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())));
        info
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
    if lowered.blocks.is_empty() {
        return;
    }

    let Ok(is_panic_destruct_fn) = function_id.is_panic_destruct_fn(db) else {
        return;
    };

    let checker = DestructAdder {
        db,
        lowered,
        destructions: vec![],
        panic_ty: panic_ty(db.upcast()),
        is_panic_destruct_fn,
    };
    let mut analysis = BackAnalysis::new(lowered, checker);
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

    let info = db.core_info();
    let plain_trait_function = info.destruct_fn;
    let panic_trait_function = info.panic_destruct_fn;

    // Add destructions.
    let stable_ptr = function_id
        .function_with_body_id(db.upcast())
        .base_semantic_function(db)
        .untyped_stable_ptr(db.upcast());

    let location = variables.get_location(stable_ptr);

    let DestructAdder { db: _, lowered: _, destructions, panic_ty, is_panic_destruct_fn: _ } =
        analysis.analyzer;

    // We need to add the destructions in reverse order, so that they won't interfere with each
    // other.
    // For panic desturction, we need to group them by type and create chains of destruct calls
    // where each one consumes a panic variable and creates a new one.
    // To facilitate this, we convert each entry to a tuple we the relevant information for
    // ordering and grouping.
    let as_tuple = |entry: &DestructionEntry| match entry {
        DestructionEntry::Plain(plain_destruct) => {
            (plain_destruct.position.0.0, plain_destruct.position.1, AddDestructFlowType::Plain, 0)
        }
        DestructionEntry::Panic(panic_destruct) => match panic_destruct.panic_location {
            PanicLocation::PanicMatch { target_block_id, match_block_id } => {
                (target_block_id.0, 0, AddDestructFlowType::PanicPostMatch, match_block_id.0)
            }
            PanicLocation::PanicVar { statement_location } => {
                (statement_location.0.0, statement_location.1, AddDestructFlowType::PanicVar, 0)
            }
        },
    };

    for ((block_id, statement_idx, destruct_type, match_block_id), destructions) in
        destructions.into_iter().sorted_by_key(as_tuple).rev().chunk_by(as_tuple).into_iter()
    {
        let mut stmts = vec![];

        let first_panic_var = variables.new_var(VarRequest { ty: panic_ty, location });
        let mut last_panic_var = first_panic_var;

        for destruction in destructions {
            let output_var = variables.new_var(VarRequest { ty: unit_ty(db.upcast()), location });

            match destruction {
                DestructionEntry::Plain(plain_destruct) => {
                    let semantic_function = semantic::FunctionLongId {
                        function: ConcreteFunction {
                            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId {
                                impl_id: plain_destruct.impl_id,
                                function: plain_trait_function,
                            }),
                            generic_args: vec![],
                        },
                    }
                    .intern(db);

                    stmts.push(StatementCall {
                        function: semantic_function.lowered(db),
                        inputs: vec![VarUsage { var_id: plain_destruct.var_id, location }],
                        with_coupon: false,
                        outputs: vec![output_var],
                        location: lowered.variables[plain_destruct.var_id].location,
                    })
                }

                DestructionEntry::Panic(panic_destruct) => {
                    let semantic_function = semantic::FunctionLongId {
                        function: ConcreteFunction {
                            generic_function: GenericFunctionId::Impl(ImplGenericFunctionId {
                                impl_id: panic_destruct.impl_id,
                                function: panic_trait_function,
                            }),
                            generic_args: vec![],
                        },
                    }
                    .intern(db);

                    let new_panic_var = variables.new_var(VarRequest { ty: panic_ty, location });

                    stmts.push(StatementCall {
                        function: semantic_function.lowered(db),
                        inputs: vec![
                            VarUsage { var_id: panic_destruct.var_id, location },
                            VarUsage { var_id: last_panic_var, location },
                        ],
                        with_coupon: false,
                        outputs: vec![new_panic_var, output_var],
                        location,
                    });
                    last_panic_var = new_panic_var;
                }
            }
        }

        match destruct_type {
            AddDestructFlowType::Plain => {
                let block = &mut lowered.blocks[BlockId(block_id)];
                block
                    .statements
                    .splice(statement_idx..statement_idx, stmts.into_iter().map(Statement::Call));
            }
            AddDestructFlowType::PanicPostMatch => {
                let block = &mut lowered.blocks[BlockId(match_block_id)];
                let FlatBlockEnd::Match { info: MatchInfo::Enum(info) } = &mut block.end else {
                    unreachable!();
                };

                let arm = &mut info.arms[1];
                let tuple_var = &mut arm.var_ids[0];
                let tuple_ty = lowered.variables[*tuple_var].ty;
                let new_tuple_var = variables.new_var(VarRequest { ty: tuple_ty, location });
                let orig_tuple_var = *tuple_var;
                *tuple_var = new_tuple_var;
                let long_ty = tuple_ty.lookup_intern(db);
                let TypeLongId::Tuple(tys) = long_ty else { unreachable!() };

                let vars = tys
                    .iter()
                    .copied()
                    .map(|ty| variables.new_var(VarRequest { ty, location }))
                    .collect::<Vec<_>>();

                *stmts.last_mut().unwrap().outputs.get_mut(0).unwrap() = vars[0];

                let target_block_id = arm.block_id;

                let block = &mut lowered.blocks[target_block_id];

                block.statements.splice(
                    0..0,
                    chain!(
                        [Statement::StructDestructure(StatementStructDestructure {
                            input: VarUsage { var_id: new_tuple_var, location },
                            outputs: chain!([first_panic_var], vars.iter().skip(1).cloned())
                                .collect(),
                        })],
                        stmts.into_iter().map(Statement::Call),
                        [Statement::StructConstruct(StatementStructConstruct {
                            inputs: vars
                                .into_iter()
                                .map(|var_id| VarUsage { var_id, location })
                                .collect(),
                            output: orig_tuple_var,
                        })]
                    ),
                );
            }
            AddDestructFlowType::PanicVar => {
                let block = &mut lowered.blocks[BlockId(block_id)];

                let idx = match block.statements.get_mut(statement_idx) {
                    Some(stmt) => {
                        let panic_var =
                            &mut extract_matches!(stmt, Statement::StructConstruct).output;
                        *stmts.last_mut().unwrap().outputs.get_mut(0).unwrap() = *panic_var;
                        *panic_var = first_panic_var;

                        statement_idx + 1
                    }
                    None => {
                        assert_eq!(statement_idx, block.statements.len());
                        let panic_var = match &mut block.end {
                            FlatBlockEnd::Return(vars, _) => &mut vars[0].var_id,
                            _ => unreachable!("Expected a return statement."),
                        };

                        stmts.first_mut().unwrap().inputs.get_mut(1).unwrap().var_id = *panic_var;
                        *panic_var = last_panic_var;
                        statement_idx
                    }
                };

                block.statements.splice(idx..idx, stmts.into_iter().map(Statement::Call));
            }
        };
    }

    lowered.variables = variables.variables;
}
