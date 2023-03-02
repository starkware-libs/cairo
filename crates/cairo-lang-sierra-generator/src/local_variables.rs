#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use std::collections::HashSet;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::{BlockId, VariableId};
use cairo_lang_sierra::extensions::lib_func::{BranchSignature, LibfuncSignature};
use cairo_lang_sierra::extensions::OutputVarReferenceInfo;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{zip_eq, Itertools};
use lowering::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use lowering::borrow_check::demand::DemandReporter;
use lowering::borrow_check::Demand;
use lowering::{FlatBlock, FlatLowered, MatchInfo, Statement, VarRemapping};

use crate::db::SierraGenGroup;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::utils::{
    enum_init_libfunc_id, get_concrete_libfunc_id, get_libfunc_signature, match_enum_libfunc_id,
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
        block_callers: Default::default(),
        prune_from_locals: Default::default(),
        aliases: Default::default(),
    };
    let mut analysis =
        BackAnalysis { lowered: lowered_function, cache: Default::default(), analyzer: ctx };
    lowered_function.blocks.has_root()?;
    let root_info = analysis.get_root_info()?;

    if !root_info.known_ap_change {
        // Revoke all convergances.
        for (block_id, callers) in analysis.analyzer.block_callers.clone() {
            if callers.len() <= 1 {
                continue;
            }
            let mut info = analysis.cache[&block_id].as_ref().map_err(|v| *v)?.clone();
            let introducd_vars = callers[0].1.keys().cloned().collect_vec();
            info.demand.variables_introduced(&mut analysis.analyzer, &introducd_vars, ());
            analysis.analyzer.revoke_if_needed(&mut info, BranchInfo { known_ap_change: false });
        }
    }

    let FindLocalsContext { used_after_revoke, prune_from_locals, aliases, .. } = analysis.analyzer;

    let function_inputs: HashSet<_> =
        lowered_function.blocks[BlockId::root()].inputs.iter().copied().collect();

    let mut locals = OrderedHashSet::default();
    for mut var in used_after_revoke.iter() {
        while let Some(alias) = aliases.get(var) {
            var = alias;
        }
        if prune_from_locals.contains(var) {
            continue;
        }
        if function_inputs.contains(var) {
            continue;
        }
        locals.insert(*var);
    }
    Ok(locals)
}

/// Context for the find_local_variables logic.
struct FindLocalsContext<'a> {
    db: &'a dyn SierraGenGroup,
    lowered_function: &'a FlatLowered,
    used_after_revoke: OrderedHashSet<VariableId>,
    block_callers: OrderedHashMap<BlockId, Vec<(BlockId, VarRemapping)>>,
    prune_from_locals: UnorderedHashSet<VariableId>,
    aliases: UnorderedHashMap<VariableId, VariableId>,
}

pub type LoweredDemand = Demand<VariableId>;
#[derive(Clone)]
struct AnalysisInfo {
    demand: LoweredDemand,
    known_ap_change: bool,
}
impl<'a> DemandReporter<VariableId> for FindLocalsContext<'a> {
    type UsePosition = ();
    type IntroducePosition = ();

    fn drop(&mut self, _position: Self::IntroducePosition, _var: VariableId) {}
    fn dup(&mut self, _position: Self::UsePosition, _var: VariableId) {}
    fn last_use(&mut self, _position: Self::UsePosition, _var_index: usize, _var: VariableId) {}
    fn unused_mapped_var(&mut self, _var: VariableId) {}
}
impl<'a> Analyzer for FindLocalsContext<'a> {
    type Info = Maybe<AnalysisInfo>;

    fn visit_block_start(&mut self, info: &mut Self::Info, _block_id: BlockId, block: &FlatBlock) {
        let Ok(info) = info else {return;};
        info.demand.variables_introduced(self, &block.inputs, ());
    }

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        let Ok(info) = info else {return;};
        let Ok(branch_info) = self.analyze_statement(stmt) else {return;};
        info.demand.variables_introduced(self, &stmt.outputs(), ());
        self.revoke_if_needed(info, branch_info);
        info.demand.variables_used(self, &stmt.inputs(), ());
    }

    fn visit_remapping(
        &mut self,
        info: &mut Self::Info,
        block_id: BlockId,
        target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        let Ok(info) = info else {return;};
        self.block_callers.entry(target_block_id).or_default().push((block_id, remapping.clone()));
        info.demand.apply_remapping(self, remapping.iter().map(|(dst, src)| (*dst, *src)));
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        arms: &[(BlockId, Self::Info)],
    ) -> Maybe<AnalysisInfo> {
        let mut arm_demands = vec![];
        let mut known_ap_change = true;
        let inputs = match_info.inputs();

        // Revoke if needed.
        let libfunc_signature = self.get_match_libfunc_signature(match_info)?;
        for ((block_id, info), branch_signature) in
            zip_eq(arms, libfunc_signature.branch_signatures)
        {
            let info = info.as_ref().map_err(|v| *v)?;
            let block_inputs = &self.lowered_function.blocks[*block_id].inputs;
            let branch_info = self.analyze_branch(&branch_signature, &inputs, block_inputs);
            let mut info = info.clone();
            self.revoke_if_needed(&mut info, branch_info);
            known_ap_change &= info.known_ap_change;
            arm_demands.push((info.demand, ()));
        }
        let mut demand = LoweredDemand::merge_demands(&arm_demands, self);
        demand.variables_used(self, &match_info.inputs(), ());
        Ok(AnalysisInfo { demand, known_ap_change })
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info {
        let mut demand = LoweredDemand::default();
        demand.variables_used(self, vars, ());
        Ok(AnalysisInfo { demand, known_ap_change: true })
    }
}

struct BranchInfo {
    known_ap_change: bool,
}

impl<'a> FindLocalsContext<'a> {
    fn analyze_call(
        &mut self,
        concrete_function_id: cairo_lang_sierra::ids::ConcreteLibfuncId,
        input_vars: &[VariableId],
        output_vars: &[VariableId],
    ) -> BranchInfo {
        let libfunc_signature = get_libfunc_signature(self.db, concrete_function_id.clone());
        assert_eq!(
            libfunc_signature.branch_signatures.len(),
            1,
            "Unexpected branches in '{}'.",
            DebugReplacer { db: self.db }.replace_libfunc_id(&concrete_function_id)
        );

        self.analyze_branch(&libfunc_signature.branch_signatures[0], input_vars, output_vars)
    }

    fn analyze_branch(
        &mut self,
        branch_signature: &BranchSignature,
        input_vars: &[VariableId],
        output_vars: &[VariableId],
    ) -> BranchInfo {
        let var_output_infos = &branch_signature.vars;
        for (var, output_info) in zip_eq(output_vars.iter(), var_output_infos.iter()) {
            match output_info.ref_info {
                OutputVarReferenceInfo::SameAsParam { param_idx }
                | OutputVarReferenceInfo::PartialParam { param_idx } => {
                    self.aliases.insert(*var, input_vars[param_idx]);
                }
                OutputVarReferenceInfo::NewTempVar { .. } | OutputVarReferenceInfo::Deferred(_) => {
                }
                OutputVarReferenceInfo::NewLocalVar => {
                    self.prune_from_locals.insert(*var);
                }
            }
        }

        let known_ap_change = matches!(
            branch_signature.ap_change,
            cairo_lang_sierra::extensions::lib_func::SierraApChange::Known { .. }
        );

        BranchInfo { known_ap_change }
    }

    fn analyze_statement(&mut self, statement: &Statement) -> Maybe<BranchInfo> {
        let inputs = statement.inputs();
        let outputs = statement.outputs();
        let branch_info = match statement {
            lowering::Statement::Literal(statement_literal) => {
                self.prune_from_locals.insert(statement_literal.output);
                BranchInfo { known_ap_change: true }
            }
            lowering::Statement::Call(statement_call) => {
                let (_, concrete_function_id) =
                    get_concrete_libfunc_id(self.db, statement_call.function);

                self.analyze_call(concrete_function_id, &inputs, &outputs)
            }
            lowering::Statement::StructConstruct(statement_struct_construct) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_struct_construct.output].ty,
                )?;
                self.analyze_call(struct_construct_libfunc_id(self.db, ty), &inputs, &outputs)
            }
            lowering::Statement::StructDestructure(statement_struct_destructure) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_struct_destructure.input].ty,
                )?;
                self.analyze_call(struct_deconstruct_libfunc_id(self.db, ty)?, &inputs, &outputs)
            }
            lowering::Statement::EnumConstruct(statement_enum_construct) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_enum_construct.output].ty,
                )?;
                self.analyze_call(
                    enum_init_libfunc_id(self.db, ty, statement_enum_construct.variant.idx),
                    &inputs,
                    &outputs,
                )
            }
            lowering::Statement::Snapshot(statement_snapshot) => {
                self.aliases.insert(statement_snapshot.output_original, statement_snapshot.input);
                self.aliases.insert(statement_snapshot.output_snapshot, statement_snapshot.input);
                BranchInfo { known_ap_change: true }
            }
            lowering::Statement::Desnap(statement_desnap) => {
                self.aliases.insert(statement_desnap.output, statement_desnap.input);
                BranchInfo { known_ap_change: true }
            }
        };
        Ok(branch_info)
    }

    fn revoke_if_needed(&mut self, info: &mut AnalysisInfo, branch_info: BranchInfo) {
        // Revoke if needed.
        if !branch_info.known_ap_change {
            info.known_ap_change = false;
            // Revoke all demanded variables.
            for var in info.demand.vars.iter() {
                self.used_after_revoke.insert(*var);
            }
        }
    }

    fn get_match_libfunc_signature(&self, match_info: &MatchInfo) -> Maybe<LibfuncSignature> {
        Ok(match match_info {
            MatchInfo::Extern(s) => {
                let (_, concrete_function_id) = get_concrete_libfunc_id(self.db, s.function);
                get_libfunc_signature(self.db, concrete_function_id)
            }
            MatchInfo::Enum(s) => {
                let concrete_enum_type =
                    self.db.get_concrete_type_id(self.lowered_function.variables[s.input].ty)?;
                let concrete_function_id = match_enum_libfunc_id(self.db, concrete_enum_type)?;
                get_libfunc_signature(self.db, concrete_function_id)
            }
        })
    }
}
