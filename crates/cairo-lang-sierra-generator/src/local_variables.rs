#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::{BlockId, VariableId};
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_sierra::extensions::OutputVarReferenceInfo;
use cairo_lang_sierra::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, ParamSignature,
};
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{chain, zip_eq};
use lowering::borrow_check::Demand;
use lowering::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use lowering::borrow_check::demand::DemandReporter;
use lowering::{FlatLowered, MatchInfo, Statement, VarRemapping, VarUsage};

use crate::ap_tracking::{ApTrackingConfiguration, get_ap_tracking_configuration};
use crate::db::SierraGenGroup;
use crate::replace_ids::{DebugReplacer, SierraIdReplacer};
use crate::utils::{
    enum_init_libfunc_id, get_concrete_libfunc_id, get_libfunc_signature, match_enum_libfunc_id,
    struct_construct_libfunc_id, struct_deconstruct_libfunc_id,
};

/// Information returned by [analyze_ap_changes].
pub struct AnalyzeApChangesResult {
    /// True if the function has a known_ap_change
    pub known_ap_change: bool,
    /// The variables that should be stored in locals as they are revoked during the function.
    pub local_variables: OrderedHashSet<VariableId>,
    /// Information about where ap tracking should be enabled and disabled.
    pub ap_tracking_configuration: ApTrackingConfiguration,
}

/// Does ap change related analysis for a given function.
/// See [AnalyzeApChangesResult].
pub fn analyze_ap_changes(
    db: &dyn SierraGenGroup,
    lowered_function: &FlatLowered,
) -> Maybe<AnalyzeApChangesResult> {
    lowered_function.blocks.has_root()?;
    let ctx = FindLocalsContext {
        db,
        lowered_function,
        used_after_revoke: Default::default(),
        block_callers: Default::default(),
        non_ap_based: UnorderedHashSet::from_iter(chain!(
            // Parameters are not ap based.
            lowered_function.parameters.iter().cloned(),
            // All empty variables are not ap based.
            lowered_function.variables.iter().filter_map(|(id, var)| {
                let info = db.get_type_info(db.get_concrete_type_id(var.ty).ok()?).ok()?;
                if info.zero_sized { Some(id) } else { None }
            })
        )),
        constants: Default::default(),
        aliases: Default::default(),
        const_aliases: Default::default(),
        partial_param_parents: Default::default(),
    };
    let mut analysis = BackAnalysis::new(lowered_function, ctx);
    let mut root_info = analysis.get_root_info()?;
    root_info.demand.variables_introduced(&mut analysis.analyzer, &lowered_function.parameters, ());

    let mut ctx = analysis.analyzer;
    let peeled_used_after_revoke: OrderedHashSet<_> =
        ctx.used_after_revoke.iter().map(|var| ctx.peel_aliases(var)).copied().collect();
    // Any used after revoke variable that might be revoked should be a local.
    let locals: OrderedHashSet<VariableId> = ctx
        .used_after_revoke
        .iter()
        .filter(|var| ctx.might_be_revoked(&peeled_used_after_revoke, var))
        .map(|var| ctx.peel_aliases(var))
        .cloned()
        .collect();
    let mut need_ap_alignment = OrderedHashSet::default();
    if !root_info.known_ap_change {
        // Add 'locals' to the set a variable that is not ap based.
        ctx.non_ap_based.extend(locals.iter().cloned());

        // Find all the variables that need ap alignment.
        for (_, mut info) in std::mem::take(&mut ctx.block_callers) {
            if info.caller_count <= 1 {
                continue;
            }
            info.demand.variables_introduced(&mut ctx, &info.introduced_vars, ());
            for var in info.demand.vars.keys() {
                if ctx.might_be_revoked(&peeled_used_after_revoke, var) {
                    need_ap_alignment.insert(*var);
                }
            }
        }
    }

    Ok(AnalyzeApChangesResult {
        known_ap_change: root_info.known_ap_change,
        local_variables: locals,
        ap_tracking_configuration: get_ap_tracking_configuration(
            lowered_function,
            root_info.known_ap_change,
            need_ap_alignment,
        ),
    })
}

struct CalledBlockInfo {
    caller_count: usize,
    demand: LoweredDemand,
    introduced_vars: Vec<VariableId>,
}

/// Context for the find_local_variables logic.
struct FindLocalsContext<'a> {
    db: &'a dyn SierraGenGroup,
    lowered_function: &'a FlatLowered,
    used_after_revoke: OrderedHashSet<VariableId>,
    block_callers: OrderedHashMap<BlockId, CalledBlockInfo>,
    /// Variables that are known not to be ap based, excluding constants.
    non_ap_based: UnorderedHashSet<VariableId>,
    /// Variables that are constants, i.e. created from Statement::Literal.
    constants: UnorderedHashSet<VariableId>,
    /// A mapping of variables which are the same in the context of finding locals.
    aliases: UnorderedHashMap<VariableId, VariableId>,
    /// Same as `aliases`, but for constants. This is excluding aliases which are created by
    /// libfuncs with SameAsParam output, but do not accept constants as input.
    const_aliases: UnorderedHashMap<VariableId, VariableId>,
    /// A mapping from partial param variables to the containing variable.
    partial_param_parents: UnorderedHashMap<VariableId, VariableId>,
}

pub type LoweredDemand = Demand<VariableId, ()>;
#[derive(Clone)]
struct AnalysisInfo {
    demand: LoweredDemand,
    known_ap_change: bool,
}
impl DemandReporter<VariableId> for FindLocalsContext<'_> {
    type UsePosition = ();
    type IntroducePosition = ();
}
impl Analyzer<'_> for FindLocalsContext<'_> {
    type Info = Maybe<AnalysisInfo>;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        let Ok(info) = info else {
            return;
        };
        let Ok(branch_info) = self.analyze_statement(stmt) else {
            return;
        };
        info.demand.variables_introduced(self, stmt.outputs(), ());
        self.revoke_if_needed(info, branch_info);
        info.demand
            .variables_used(self, stmt.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())));
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        let Ok(info) = info else {
            return;
        };
        match self.block_callers.entry(target_block_id) {
            Entry::Occupied(mut e) => {
                e.get_mut().caller_count += 1;
            }
            Entry::Vacant(e) => {
                e.insert(CalledBlockInfo {
                    caller_count: 1,
                    demand: info.demand.clone(),
                    introduced_vars: remapping.keys().copied().collect(),
                });
            }
        }
        info.demand
            .apply_remapping(self, remapping.iter().map(|(dst, src)| (dst, (&src.var_id, ()))));
    }

    fn merge_match(
        &mut self,
        _statement_location: StatementLocation,
        match_info: &MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Maybe<AnalysisInfo> {
        let mut arm_demands = vec![];
        let mut known_ap_change = true;
        let inputs = match_info.inputs();

        // Revoke if needed.
        let libfunc_signature = self.get_match_libfunc_signature(match_info)?;
        for (arm, (info, branch_signature)) in
            zip_eq(match_info.arms(), zip_eq(infos, libfunc_signature.branch_signatures))
        {
            let mut info = info?;
            info.demand.variables_introduced(self, &arm.var_ids, ());
            let branch_info = self.analyze_branch(
                &libfunc_signature.param_signatures,
                &branch_signature,
                inputs,
                &arm.var_ids,
            );
            self.revoke_if_needed(&mut info, branch_info);
            known_ap_change &= info.known_ap_change;
            arm_demands.push((info.demand, ()));
        }
        let mut demand = LoweredDemand::merge_demands(&arm_demands, self);
        demand.variables_used(
            self,
            match_info.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
        Ok(AnalysisInfo { demand, known_ap_change })
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut demand = LoweredDemand::default();
        demand.variables_used(self, vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())));
        Ok(AnalysisInfo { demand, known_ap_change: true })
    }
}

struct BranchInfo {
    known_ap_change: bool,
}

impl<'a> FindLocalsContext<'a> {
    /// Given a variable that might be an alias follow aliases until we get the original variable.
    pub fn peel_aliases(&'a self, mut var: &'a VariableId) -> &'a VariableId {
        while let Some(alias) = self.aliases.get(var) {
            var = alias;
        }
        var
    }
    /// Given a variable, that might be an alias of a const, follow  the const aliases until we get
    /// the root variable.
    pub fn peel_const_aliases(&'a self, mut var: &'a VariableId) -> &'a VariableId {
        while let Some(alias) = self.const_aliases.get(var) {
            var = alias;
        }
        var
    }

    /// Return true if the alias peeled variable might be revoked by ap changes.
    /// If a variable is not ap-based or one of its ancestors is not ap-based, then it can't be
    /// revoked.
    ///
    /// Note that vars in `peeled_used_after_revoke` are going to be non-ap based once we make the
    /// relevant variables local.
    pub fn might_be_revoked(
        &self,
        peeled_used_after_revoke: &OrderedHashSet<VariableId>,
        var: &VariableId,
    ) -> bool {
        if self.non_ap_based.contains(self.peel_aliases(var)) {
            return false;
        }
        if self.constants.contains(self.peel_const_aliases(var)) {
            return false;
        }
        // In the case of partial params, we check if one of its ancestors is a local variable, or
        // will be used after the revoke, and thus will be used as a local variable. If that
        // is the case, then 'var' can not be revoked.
        let mut parent_var = self.peel_aliases(var);
        while let Some(grandparent) = self.partial_param_parents.get(parent_var) {
            parent_var = self.peel_aliases(grandparent);
            if self.non_ap_based.contains(parent_var)
                || peeled_used_after_revoke.contains(parent_var)
            {
                return false;
            }
        }
        let mut parent_var = self.peel_const_aliases(var);
        while let Some(grandparent) = self.partial_param_parents.get(parent_var) {
            parent_var = self.peel_const_aliases(grandparent);
            if self.constants.contains(parent_var) || peeled_used_after_revoke.contains(parent_var)
            {
                return false;
            }
        }
        true
    }

    fn analyze_call(
        &mut self,
        concrete_function_id: cairo_lang_sierra::ids::ConcreteLibfuncId,
        input_vars: &[VarUsage],
        output_vars: &[VariableId],
    ) -> BranchInfo {
        let libfunc_signature = get_libfunc_signature(self.db, concrete_function_id.clone());
        assert_eq!(
            libfunc_signature.branch_signatures.len(),
            1,
            "Unexpected branches in '{}'.",
            DebugReplacer { db: self.db }.replace_libfunc_id(&concrete_function_id)
        );
        self.analyze_branch(
            &libfunc_signature.param_signatures,
            &libfunc_signature.branch_signatures[0],
            input_vars,
            output_vars,
        )
    }

    fn analyze_branch(
        &mut self,
        params_signatures: &[ParamSignature],
        branch_signature: &BranchSignature,
        input_vars: &[VarUsage],
        output_vars: &[VariableId],
    ) -> BranchInfo {
        let var_output_infos = &branch_signature.vars;
        for (var, output_info) in zip_eq(output_vars.iter(), var_output_infos.iter()) {
            match output_info.ref_info {
                OutputVarReferenceInfo::SameAsParam { param_idx } => {
                    // SameAsParam variables are aliased only if the libfunc accepts all the input
                    // variable states, as if the input variable state is not allowed it will be
                    // stored before the call and will then be ap-based.
                    // TODO(Gil): Maintain the input variable state (const, add const, deferred or
                    // stored, similar to the `store_variables` algorithm) and don't alias only if
                    // the libfunc does not accept the specific state of the input variable.
                    if params_signatures[param_idx].allow_const {
                        self.const_aliases.insert(*var, input_vars[param_idx].var_id);
                    }
                    self.aliases.insert(*var, input_vars[param_idx].var_id);
                }
                OutputVarReferenceInfo::PartialParam { param_idx } => {
                    if params_signatures[param_idx].allow_const {
                        self.const_aliases.insert(*var, input_vars[param_idx].var_id);
                    }
                    self.partial_param_parents.insert(*var, input_vars[param_idx].var_id);
                }
                OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const)
                | OutputVarReferenceInfo::NewLocalVar
                | OutputVarReferenceInfo::ZeroSized => {
                    self.non_ap_based.insert(*var);
                }
                OutputVarReferenceInfo::NewTempVar { .. }
                | OutputVarReferenceInfo::SimpleDerefs
                | OutputVarReferenceInfo::Deferred(_) => {}
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
            lowering::Statement::Const(statement_literal) => {
                if matches!(
                    statement_literal.value,
                    ConstValue::Int(..)
                        | ConstValue::Struct(..)
                        | ConstValue::Enum(..)
                        | ConstValue::NonZero(..)
                ) {
                    self.constants.insert(statement_literal.output);
                }
                BranchInfo { known_ap_change: true }
            }
            lowering::Statement::Call(statement_call) => {
                let (_, concrete_function_id) = get_concrete_libfunc_id(
                    self.db,
                    statement_call.function,
                    statement_call.with_coupon,
                );

                self.analyze_call(concrete_function_id, inputs, outputs)
            }
            lowering::Statement::StructConstruct(statement_struct_construct) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_struct_construct.output].ty,
                )?;
                self.analyze_call(struct_construct_libfunc_id(self.db, ty), inputs, outputs)
            }
            lowering::Statement::StructDestructure(statement_struct_destructure) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_struct_destructure.input.var_id].ty,
                )?;
                self.analyze_call(struct_deconstruct_libfunc_id(self.db, ty)?, inputs, outputs)
            }
            lowering::Statement::EnumConstruct(statement_enum_construct) => {
                let ty = self.db.get_concrete_type_id(
                    self.lowered_function.variables[statement_enum_construct.output].ty,
                )?;
                self.analyze_call(
                    enum_init_libfunc_id(self.db, ty, statement_enum_construct.variant.idx),
                    inputs,
                    outputs,
                )
            }
            lowering::Statement::Snapshot(statement_snapshot) => {
                self.aliases.insert(statement_snapshot.original(), statement_snapshot.input.var_id);
                self.aliases.insert(statement_snapshot.snapshot(), statement_snapshot.input.var_id);
                self.const_aliases
                    .insert(statement_snapshot.original(), statement_snapshot.input.var_id);
                self.const_aliases
                    .insert(statement_snapshot.snapshot(), statement_snapshot.input.var_id);
                BranchInfo { known_ap_change: true }
            }
            lowering::Statement::Desnap(statement_desnap) => {
                self.aliases.insert(statement_desnap.output, statement_desnap.input.var_id);
                self.const_aliases.insert(statement_desnap.output, statement_desnap.input.var_id);
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
            for var in info.demand.vars.keys() {
                self.used_after_revoke.insert(*var);
            }
        }
    }

    fn get_match_libfunc_signature(&self, match_info: &MatchInfo) -> Maybe<LibfuncSignature> {
        Ok(match match_info {
            MatchInfo::Extern(s) => {
                let (_, concrete_function_id) = get_concrete_libfunc_id(self.db, s.function, false);
                get_libfunc_signature(self.db, concrete_function_id)
            }
            MatchInfo::Enum(s) => {
                let concrete_enum_type = self
                    .db
                    .get_concrete_type_id(self.lowered_function.variables[s.input.var_id].ty)?;
                let concrete_function_id = match_enum_libfunc_id(self.db, concrete_enum_type)?;
                get_libfunc_signature(self.db, concrete_function_id)
            }
            MatchInfo::Value(s) => {
                let concrete_enum_type = self.db.get_index_enum_type_id(s.num_of_arms)?;
                let concrete_function_id = match_enum_libfunc_id(self.db, concrete_enum_type)?;
                get_libfunc_signature(self.db, concrete_function_id)
            }
        })
    }
}
