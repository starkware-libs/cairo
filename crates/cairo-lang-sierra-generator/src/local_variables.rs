#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use std::collections::HashSet;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::{BlockId, VariableId};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::Itertools;
use lowering::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use lowering::borrow_check::demand::DemandReporter;
use lowering::borrow_check::Demand;
use lowering::{FlatBlock, FlatLowered, Statement, VarRemapping};

use crate::db::SierraGenGroup;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::utils::{
    enum_init_libfunc_id, get_concrete_libfunc_id, get_libfunc_signature,
    struct_construct_libfunc_id, struct_deconstruct_libfunc_id,
};

/// Given the lowering of a function, returns the set of variables which should be stored as local
/// variables.
pub fn find_local_variables(
    db: &dyn SierraGenGroup,
    lowered_function: &FlatLowered,
) -> Maybe<OrderedHashSet<VariableId>> {
    let ctx = FindLocalsContext {
        db,
        lowered_function,
        used_after_revoke: Default::default(),
        constants: Default::default(),
    };
    let mut analysis =
        BackAnalysis { lowered: lowered_function, cache: Default::default(), analyzer: ctx };
    analysis.get_root_info();
    let FindLocalsContext { used_after_revoke, constants, .. } = analysis.analyzer;

    let inputs: HashSet<_> =
        lowered_function.blocks[lowered_function.root_block?].inputs.iter().copied().collect();
    let locals = used_after_revoke
        .iter()
        .copied()
        .filter(|var| !constants.contains(var) && !inputs.contains(var))
        .collect();
    Ok(locals)
}

/// Context for the find_local_variables logic.
struct FindLocalsContext<'a> {
    db: &'a dyn SierraGenGroup,
    lowered_function: &'a FlatLowered,
    used_after_revoke: OrderedHashSet<VariableId>,
    constants: OrderedHashSet<VariableId>,
}

pub type LoweredDemand = Demand<VariableId>;
impl<'a> DemandReporter<VariableId> for FindLocalsContext<'a> {
    type UsePosition = ();
    type IntroducePosition = ();

    fn drop(&mut self, _position: Self::IntroducePosition, _var: VariableId) {}
    fn dup(&mut self, _position: Self::UsePosition, _var: VariableId) {}
    fn last_use(&mut self, _position: Self::UsePosition, _var_index: usize, _var: VariableId) {}
    fn unused_mapped_var(&mut self, _var: VariableId) {}
}
impl<'a> Analyzer for FindLocalsContext<'a> {
    type Info = LoweredDemand;

    fn visit_block_start(&mut self, info: &mut Self::Info, _block_id: BlockId, block: &FlatBlock) {
        info.variables_introduced(self, &block.inputs, ());
    }

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        info.variables_introduced(self, &stmt.outputs(), ());
        // Revoke if needed.
        if self.is_ap_change_known_in_statement(stmt) != Ok(true) {
            // Revoke all demanded variables.
            for var in info.vars.iter() {
                self.used_after_revoke.insert(*var);
            }
        }
        if let Statement::Literal(_) = stmt {
            for var in stmt.outputs() {
                self.constants.insert(var);
            }
        }
        info.variables_used(self, &stmt.inputs(), ());
    }

    fn visit_remapping(&mut self, info: &mut Self::Info, remapping: &VarRemapping) {
        info.apply_remapping(self, remapping.iter().map(|(dst, src)| (*dst, *src)));
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        stmt: &Statement,
        arms: &[(BlockId, Self::Info)],
    ) -> Self::Info {
        let arm_demands = arms.iter().map(|(_block_id, demand)| (demand.clone(), ())).collect_vec();
        let mut info = LoweredDemand::merge_demands(&arm_demands, self);
        info.variables_used(self, &stmt.inputs(), ());
        info
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

    fn info_from_unreachable(&mut self) -> Self::Info {
        LoweredDemand::default()
    }
}

impl<'a> FindLocalsContext<'a> {
    fn is_ap_change_known_in_call(
        &self,
        concrete_function_id: cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> bool {
        let libfunc_signature = get_libfunc_signature(self.db, concrete_function_id.clone());
        assert_eq!(
            libfunc_signature.branch_signatures.len(),
            1,
            "Unexpected branches in '{}'.",
            DebugReplacer { db: self.db }.replace_libfunc_id(&concrete_function_id)
        );

        matches!(
            libfunc_signature.branch_signatures[0].ap_change,
            cairo_lang_sierra::extensions::lib_func::SierraApChange::Known { .. }
        )
    }

    fn is_ap_change_known_in_statement(&self, statement: &Statement) -> Maybe<bool> {
        Ok(match statement {
            lowering::Statement::Literal(_) => true,
            lowering::Statement::Call(statement_call) => {
                let (_, concrete_function_id) =
                    get_concrete_libfunc_id(self.db, statement_call.function);

                self.is_ap_change_known_in_call(concrete_function_id)
            }
            lowering::Statement::MatchExtern(_) => true,
            lowering::Statement::MatchEnum(_) => true,
            lowering::Statement::StructConstruct(statement_struct_construct) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_struct_construct.output].ty,
                )?;
                self.is_ap_change_known_in_call(struct_construct_libfunc_id(self.db, ty))
            }
            lowering::Statement::StructDestructure(statement_struct_destructure) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_struct_destructure.input].ty,
                )?;
                self.is_ap_change_known_in_call(struct_deconstruct_libfunc_id(self.db, ty)?)
            }
            lowering::Statement::EnumConstruct(statement_enum_construct) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_enum_construct.output].ty,
                )?;
                self.is_ap_change_known_in_call(enum_init_libfunc_id(
                    self.db,
                    ty,
                    statement_enum_construct.variant.idx,
                ))
            }
            lowering::Statement::Snapshot(_) => true,
            lowering::Statement::Desnap(_) => true,
        })
    }
}
