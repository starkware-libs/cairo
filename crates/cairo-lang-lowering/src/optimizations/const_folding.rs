#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_defs::ids::{ExternFunctionId, ModuleId};
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::{GenericArgumentId, MatchArmSelector, TypeId, corelib};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, extract_matches, try_extract_matches};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, SemanticFunctionIdEx};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo,
    Statement, StatementCall, StatementConst, StatementDesnap, StatementEnumConstruct,
    StatementStructConstruct, StatementStructDestructure, VarUsage, Variable, VariableId,
};

/// Keeps track of equivalent values that a variables might be replaced with.
/// Note: We don't keep track of types as we assume the usage is always correct.
#[derive(Debug, Clone)]
enum VarInfo {
    /// The variable is a const value.
    Const(ConstValue),
    /// The variable can be replaced by another variable.
    Var(VarUsage),
    /// The variable is a snapshot of another variable.
    Snapshot(Box<VarInfo>),
    /// The variable is a struct of other variables.
    /// `None` values represent variables that are not tracked.
    Struct(Vec<Option<VarInfo>>),
}

/// Performs constant folding on the lowered program.
/// The optimization works better when the blocks are topologically sorted.
pub fn const_folding(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if db.optimization_config().skip_const_folding || lowered.blocks.is_empty() {
        return;
    }
    let libfunc_info = priv_const_folding_info(db);
    // Note that we can keep the var_info across blocks because the lowering
    // is in static single assignment form.
    let mut ctx = ConstFoldingContext {
        db,
        var_info: UnorderedHashMap::default(),
        variables: &mut lowered.variables,
        libfunc_info: &libfunc_info,
    };
    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &mut lowered.blocks[block_id];
        let mut additional_consts = vec![];
        for stmt in block.statements.iter_mut() {
            ctx.maybe_replace_inputs(stmt.inputs_mut());
            match stmt {
                Statement::Const(StatementConst { value, output }) => {
                    // Preventing the insertion of non-member consts values (such as a `Box` of a
                    // const).
                    if matches!(
                        value,
                        ConstValue::Int(..)
                            | ConstValue::Struct(..)
                            | ConstValue::Enum(..)
                            | ConstValue::NonZero(..)
                    ) {
                        ctx.var_info.insert(*output, VarInfo::Const(value.clone()));
                    }
                }
                Statement::Snapshot(stmt) => {
                    if let Some(info) = ctx.var_info.get(&stmt.input.var_id).cloned() {
                        ctx.var_info.insert(stmt.original(), info.clone());
                        ctx.var_info.insert(stmt.snapshot(), VarInfo::Snapshot(info.into()));
                    }
                }
                Statement::Desnap(StatementDesnap { input, output }) => {
                    if let Some(VarInfo::Snapshot(info)) = ctx.var_info.get(&input.var_id) {
                        ctx.var_info.insert(*output, info.as_ref().clone());
                    }
                }
                Statement::Call(call_stmt) => {
                    if let Some(updated_stmt) =
                        ctx.handle_statement_call(call_stmt, &mut additional_consts)
                    {
                        *stmt = Statement::Const(updated_stmt);
                    }
                }
                Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                    let mut const_args = vec![];
                    let mut all_args = vec![];
                    let mut contains_info = false;
                    for input in inputs.iter() {
                        let Some(info) = ctx.var_info.get(&input.var_id) else {
                            all_args.push(
                                ctx.variables[input.var_id]
                                    .copyable
                                    .is_ok()
                                    .then_some(VarInfo::Var(*input)),
                            );
                            continue;
                        };
                        contains_info = true;
                        if let VarInfo::Const(value) = info {
                            const_args.push(value.clone());
                        }
                        all_args.push(Some(info.clone()));
                    }
                    if const_args.len() == inputs.len() {
                        let value = ConstValue::Struct(const_args, ctx.variables[*output].ty);
                        ctx.var_info.insert(*output, VarInfo::Const(value));
                    } else if contains_info {
                        ctx.var_info.insert(*output, VarInfo::Struct(all_args));
                    }
                }
                Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                    if let Some(mut info) = ctx.var_info.get(&input.var_id) {
                        let mut n_snapshot = 0;
                        while let VarInfo::Snapshot(inner) = info {
                            info = inner.as_ref();
                            n_snapshot += 1;
                        }
                        let wrap_with_snapshots = |mut info| {
                            for _ in 0..n_snapshot {
                                info = VarInfo::Snapshot(Box::new(info));
                            }
                            info
                        };
                        match info {
                            VarInfo::Const(ConstValue::Struct(member_values, _)) => {
                                for (output, value) in zip_eq(outputs, member_values.clone()) {
                                    ctx.var_info.insert(
                                        *output,
                                        wrap_with_snapshots(VarInfo::Const(value)),
                                    );
                                }
                            }
                            VarInfo::Struct(members) => {
                                for (output, member) in zip_eq(outputs, members.clone()) {
                                    if let Some(member) = member {
                                        ctx.var_info.insert(*output, wrap_with_snapshots(member));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                    if let Some(VarInfo::Const(val)) = ctx.var_info.get(&input.var_id) {
                        let value = ConstValue::Enum(variant.clone(), val.clone().into());
                        ctx.var_info.insert(*output, VarInfo::Const(value.clone()));
                    }
                }
            }
        }
        block.statements.splice(0..0, additional_consts.into_iter().map(Statement::Const));

        match &mut block.end {
            FlatBlockEnd::Goto(block_id, remappings) => {
                stack.push(*block_id);
                for (_, v) in remappings.iter_mut() {
                    ctx.maybe_replace_input(v);
                }
            }
            FlatBlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));
                ctx.maybe_replace_inputs(info.inputs_mut());
                match info {
                    MatchInfo::Enum(MatchEnumInfo { input, arms, .. }) => {
                        if let Some(VarInfo::Const(ConstValue::Enum(variant, value))) =
                            ctx.var_info.get(&input.var_id)
                        {
                            let arm = &arms[variant.idx];
                            ctx.var_info
                                .insert(arm.var_ids[0], VarInfo::Const(value.as_ref().clone()));
                        }
                    }
                    MatchInfo::Extern(info) => {
                        if let Some((extra_stmt, updated_end)) = ctx.handle_extern_block_end(info) {
                            if let Some(stmt) = extra_stmt {
                                block.statements.push(Statement::Const(stmt));
                            }
                            block.end = updated_end;
                        }
                    }
                    MatchInfo::Value(..) => {}
                }
            }
            FlatBlockEnd::Return(ref mut inputs, _) => ctx.maybe_replace_inputs(inputs),
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        }
    }
}

struct ConstFoldingContext<'a> {
    /// The used database.
    db: &'a dyn LoweringGroup,
    /// The variables arena, mostly used to get the type of variables.
    variables: &'a mut Arena<Variable>,
    /// The accumulated information about the const values of variables.
    var_info: UnorderedHashMap<VariableId, VarInfo>,
    /// The libfunc information.
    libfunc_info: &'a ConstFoldingLibfuncInfo,
}

impl ConstFoldingContext<'_> {
    /// Handles a statement call.
    ///
    /// Returns None if no additional changes are required.
    /// If changes are required, returns an updated const-statement (to override the current
    /// statement).
    /// May add an additional const to `additional_consts` if just replacing the current statement
    /// is not enough.
    fn handle_statement_call(
        &mut self,
        stmt: &mut StatementCall,
        additional_consts: &mut Vec<StatementConst>,
    ) -> Option<StatementConst> {
        let (id, _generic_args) = stmt.function.get_extern(self.db)?;
        if id == self.felt_sub {
            // (a - 0) can be replaced by a.
            let val = self.as_int(stmt.inputs[1].var_id)?;
            if val.is_zero() {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
            }
            None
        } else if self.wide_mul_fns.contains(&id) {
            let lhs = self.as_int_ex(stmt.inputs[0].var_id);
            let rhs = self.as_int(stmt.inputs[1].var_id);
            let output = stmt.outputs[0];
            if lhs.map(|(v, _)| v.is_zero()).unwrap_or_default()
                || rhs.map(Zero::is_zero).unwrap_or_default()
            {
                return Some(self.propagate_zero_and_get_statement(output));
            }
            let (lhs, nz_ty) = lhs?;
            Some(self.propagate_const_and_get_statement(lhs * rhs?, stmt.outputs[0], nz_ty))
        } else if id == self.bounded_int_add || id == self.bounded_int_sub {
            let lhs = self.as_int(stmt.inputs[0].var_id)?;
            let rhs = self.as_int(stmt.inputs[1].var_id)?;
            let value = if id == self.bounded_int_add { lhs + rhs } else { lhs - rhs };
            Some(self.propagate_const_and_get_statement(value, stmt.outputs[0], false))
        } else if self.div_rem_fns.contains(&id) {
            let lhs = self.as_int(stmt.inputs[0].var_id);
            if lhs.map(Zero::is_zero).unwrap_or_default() {
                additional_consts.push(self.propagate_zero_and_get_statement(stmt.outputs[1]));
                return Some(self.propagate_zero_and_get_statement(stmt.outputs[0]));
            }
            let rhs = self.as_int(stmt.inputs[1].var_id)?;
            let (q, r) = lhs?.div_rem(rhs);
            let q_output = stmt.outputs[0];
            let q_value = ConstValue::Int(q, self.variables[q_output].ty);
            self.var_info.insert(q_output, VarInfo::Const(q_value.clone()));
            let r_output = stmt.outputs[1];
            let r_value = ConstValue::Int(r, self.variables[r_output].ty);
            self.var_info.insert(r_output, VarInfo::Const(r_value.clone()));
            additional_consts.push(StatementConst { value: r_value, output: r_output });
            Some(StatementConst { value: q_value, output: q_output })
        } else if id == self.storage_base_address_from_felt252 {
            let input_var = stmt.inputs[0].var_id;
            if let Some(ConstValue::Int(val, ty)) = self.as_const(input_var) {
                stmt.inputs.clear();
                stmt.function =
                    ModuleHelper { db: self.db.upcast(), id: self.storage_access_module }
                        .function_id(
                            "storage_base_address_const",
                            vec![GenericArgumentId::Constant(
                                ConstValue::Int(val.clone(), *ty).intern(self.db),
                            )],
                        )
                        .lowered(self.db);
            }
            None
        } else if id == self.into_box {
            let const_value = match self.var_info.get(&stmt.inputs[0].var_id)? {
                VarInfo::Const(val) => val,
                VarInfo::Snapshot(info) => try_extract_matches!(info.as_ref(), VarInfo::Const)?,
                _ => return None,
            };
            let value = ConstValue::Boxed(const_value.clone().into());
            // Not inserting the value into the `var_info` map because the
            // resulting box isn't an actual const at the Sierra level.
            Some(StatementConst { value, output: stmt.outputs[0] })
        } else if id == self.upcast {
            let int_value = self.as_int(stmt.inputs[0].var_id)?;
            let output = stmt.outputs[0];
            let value = ConstValue::Int(int_value.clone(), self.variables[output].ty);
            self.var_info.insert(output, VarInfo::Const(value.clone()));
            Some(StatementConst { value, output })
        } else {
            None
        }
    }

    /// Adds `value` as a const to `var_info` and return a const statement for it.
    fn propagate_const_and_get_statement(
        &mut self,
        value: BigInt,
        output: VariableId,
        nz_ty: bool,
    ) -> StatementConst {
        let mut value = ConstValue::Int(value, self.variables[output].ty);
        if nz_ty {
            value = ConstValue::NonZero(Box::new(value));
        }
        self.var_info.insert(output, VarInfo::Const(value.clone()));
        StatementConst { value, output }
    }

    /// Adds 0 const to `var_info` and return a const statement for it.
    fn propagate_zero_and_get_statement(&mut self, output: VariableId) -> StatementConst {
        self.propagate_const_and_get_statement(BigInt::zero(), output, false)
    }

    /// Handles the end of an extern block.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns a possible additional const-statement to the block, as well
    /// as an updated block end.
    fn handle_extern_block_end(
        &mut self,
        info: &mut MatchExternInfo,
    ) -> Option<(Option<StatementConst>, FlatBlockEnd)> {
        let (id, generic_args) = info.function.get_extern(self.db)?;
        if self.nz_fns.contains(&id) {
            let val = self.as_const(info.inputs[0].var_id)?;
            let is_zero = match val {
                ConstValue::Int(v, _) => v.is_zero(),
                ConstValue::Struct(s, _) => s.iter().all(|v| {
                    v.clone().into_int().expect("Expected ConstValue::Int for size").is_zero()
                }),
                _ => unreachable!(),
            };
            Some(if is_zero {
                (None, FlatBlockEnd::Goto(info.arms[0].block_id, Default::default()))
            } else {
                let arm = &info.arms[1];
                let nz_var = arm.var_ids[0];
                let nz_val = ConstValue::NonZero(Box::new(val.clone()));
                self.var_info.insert(nz_var, VarInfo::Const(nz_val.clone()));
                (
                    Some(StatementConst { value: nz_val, output: nz_var }),
                    FlatBlockEnd::Goto(arm.block_id, Default::default()),
                )
            })
        } else if self.eq_fns.contains(&id) {
            let lhs = self.as_int(info.inputs[0].var_id);
            let rhs = self.as_int(info.inputs[1].var_id);
            if (lhs.map(Zero::is_zero).unwrap_or_default() && rhs.is_none())
                || (rhs.map(Zero::is_zero).unwrap_or_default() && lhs.is_none())
            {
                let db = self.db.upcast();
                let nz_input = info.inputs[if lhs.is_some() { 1 } else { 0 }];
                let var = &self.variables[nz_input.var_id].clone();
                let function = self.type_value_ranges.get(&var.ty)?.is_zero;
                let unused_nz_var = Variable::new(
                    self.db,
                    ImplLookupContext::default(),
                    corelib::core_nonzero_ty(db, var.ty),
                    var.location,
                );
                let unused_nz_var = self.variables.alloc(unused_nz_var);
                return Some((
                    None,
                    FlatBlockEnd::Match {
                        info: MatchInfo::Extern(MatchExternInfo {
                            function,
                            inputs: vec![nz_input],
                            arms: vec![
                                MatchArm {
                                    arm_selector: MatchArmSelector::VariantId(
                                        corelib::jump_nz_zero_variant(db, var.ty),
                                    ),
                                    block_id: info.arms[1].block_id,
                                    var_ids: vec![],
                                },
                                MatchArm {
                                    arm_selector: MatchArmSelector::VariantId(
                                        corelib::jump_nz_nonzero_variant(db, var.ty),
                                    ),
                                    block_id: info.arms[0].block_id,
                                    var_ids: vec![unused_nz_var],
                                },
                            ],
                            location: info.location,
                        }),
                    },
                ));
            }
            Some((
                None,
                FlatBlockEnd::Goto(
                    info.arms[if lhs? == rhs? { 1 } else { 0 }].block_id,
                    Default::default(),
                ),
            ))
        } else if self.uadd_fns.contains(&id)
            || self.usub_fns.contains(&id)
            || self.diff_fns.contains(&id)
            || self.iadd_fns.contains(&id)
            || self.isub_fns.contains(&id)
        {
            let rhs = self.as_int(info.inputs[1].var_id);
            if rhs.map(Zero::is_zero).unwrap_or_default() && !self.diff_fns.contains(&id) {
                let arm = &info.arms[0];
                self.var_info.insert(arm.var_ids[0], VarInfo::Var(info.inputs[0]));
                return Some((None, FlatBlockEnd::Goto(arm.block_id, Default::default())));
            }
            let lhs = self.as_int(info.inputs[0].var_id);
            let value = if self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id) {
                if lhs.map(Zero::is_zero).unwrap_or_default() {
                    let arm = &info.arms[0];
                    self.var_info.insert(arm.var_ids[0], VarInfo::Var(info.inputs[1]));
                    return Some((None, FlatBlockEnd::Goto(arm.block_id, Default::default())));
                }
                lhs? + rhs?
            } else {
                lhs? - rhs?
            };
            let ty = self.variables[info.arms[0].var_ids[0]].ty;
            let range = self.type_value_ranges.get(&ty)?;
            let (arm_index, value) = match range.normalized(value) {
                NormalizedResult::InRange(value) => (0, value),
                NormalizedResult::Under(value) => (1, value),
                NormalizedResult::Over(value) => (
                    if self.iadd_fns.contains(&id) || self.isub_fns.contains(&id) { 2 } else { 1 },
                    value,
                ),
            };
            let arm = &info.arms[arm_index];
            let actual_output = arm.var_ids[0];
            let value = ConstValue::Int(value, ty);
            self.var_info.insert(actual_output, VarInfo::Const(value.clone()));
            Some((
                Some(StatementConst { value, output: actual_output }),
                FlatBlockEnd::Goto(arm.block_id, Default::default()),
            ))
        } else if id == self.downcast {
            let input_var = info.inputs[0].var_id;
            let value = self.as_int(input_var)?;
            let success_output = info.arms[0].var_ids[0];
            let ty = self.variables[success_output].ty;
            let range = self.type_value_ranges.get(&ty)?;
            Some(if let NormalizedResult::InRange(value) = range.normalized(value.clone()) {
                let value = ConstValue::Int(value, ty);
                self.var_info.insert(success_output, VarInfo::Const(value.clone()));
                (
                    Some(StatementConst { value, output: success_output }),
                    FlatBlockEnd::Goto(info.arms[0].block_id, Default::default()),
                )
            } else {
                (None, FlatBlockEnd::Goto(info.arms[1].block_id, Default::default()))
            })
        } else if id == self.bounded_int_constrain {
            let input_var = info.inputs[0].var_id;
            let (value, nz_ty) = self.as_int_ex(input_var)?;
            let generic_arg = generic_args[1];
            let constrain_value = extract_matches!(generic_arg, GenericArgumentId::Constant)
                .lookup_intern(self.db)
                .into_int()
                .unwrap();
            let arm_idx = if value < &constrain_value { 0 } else { 1 };
            let output = info.arms[arm_idx].var_ids[0];
            Some((
                Some(self.propagate_const_and_get_statement(value.clone(), output, nz_ty)),
                FlatBlockEnd::Goto(info.arms[arm_idx].block_id, Default::default()),
            ))
        } else if id == self.array_get {
            if self.as_int(info.inputs[1].var_id)?.is_zero() {
                if let [success, failure] = info.arms.as_mut_slice() {
                    let arr = info.inputs[0].var_id;
                    let unused_arr_output0 = self.variables.alloc(self.variables[arr].clone());
                    let unused_arr_output1 = self.variables.alloc(self.variables[arr].clone());
                    info.inputs.truncate(1);
                    info.function = ModuleHelper { db: self.db.upcast(), id: self.array_module }
                        .function_id("array_snapshot_pop_front", generic_args)
                        .lowered(self.db);
                    success.var_ids.insert(0, unused_arr_output0);
                    failure.var_ids.insert(0, unused_arr_output1);
                }
            }
            None
        } else {
            None
        }
    }

    /// Returns the const value of a variable if it exists.
    fn as_const(&self, var_id: VariableId) -> Option<&ConstValue> {
        try_extract_matches!(self.var_info.get(&var_id)?, VarInfo::Const)
    }

    /// Return the const value as an int if it exists and is an integer, additionally, if it is of a
    /// non-zero type.
    fn as_int_ex(&self, var_id: VariableId) -> Option<(&BigInt, bool)> {
        match self.as_const(var_id)? {
            ConstValue::Int(value, _) => Some((value, false)),
            ConstValue::NonZero(const_value) => {
                if let ConstValue::Int(value, _) = const_value.as_ref() {
                    Some((value, true))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Return the const value as a int if it exists and is an integer.
    fn as_int(&self, var_id: VariableId) -> Option<&BigInt> {
        Some(self.as_int_ex(var_id)?.0)
    }

    /// Replaces the inputs in place if they are in the var_info map.
    fn maybe_replace_inputs(&mut self, inputs: &mut [VarUsage]) {
        for input in inputs {
            self.maybe_replace_input(input);
        }
    }

    /// Replaces the input in place if it is in the var_info map.
    fn maybe_replace_input(&mut self, input: &mut VarUsage) {
        if let Some(VarInfo::Var(new_var)) = self.var_info.get(&input.var_id) {
            *input = *new_var;
        }
    }
}

/// Query implementation of [LoweringGroup::priv_const_folding_info].
pub fn priv_const_folding_info(
    db: &dyn LoweringGroup,
) -> Arc<crate::optimizations::const_folding::ConstFoldingLibfuncInfo> {
    Arc::new(ConstFoldingLibfuncInfo::new(db))
}

/// Holds static information about libfuncs required for the optimization.
#[derive(Debug, PartialEq, Eq)]
pub struct ConstFoldingLibfuncInfo {
    /// The `felt252_sub` libfunc.
    felt_sub: ExternFunctionId,
    /// The `into_box` libfunc.
    into_box: ExternFunctionId,
    /// The `upcast` libfunc.
    upcast: ExternFunctionId,
    /// The `downcast` libfunc.
    downcast: ExternFunctionId,
    /// The set of functions that check if a number is zero.
    nz_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions that check if numbers are equal.
    eq_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to add unsigned ints.
    uadd_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to subtract unsigned ints.
    usub_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to get the difference of signed ints.
    diff_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to add signed ints.
    iadd_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to subtract signed ints.
    isub_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to multiply integers.
    wide_mul_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to divide and get the remainder of integers.
    div_rem_fns: OrderedHashSet<ExternFunctionId>,
    /// The `bounded_int_add` libfunc.
    bounded_int_add: ExternFunctionId,
    /// The `bounded_int_sub` libfunc.
    bounded_int_sub: ExternFunctionId,
    /// The `bounded_int_constrain` libfunc.
    bounded_int_constrain: ExternFunctionId,
    /// The array module.
    array_module: ModuleId,
    /// The `array_get` libfunc.
    array_get: ExternFunctionId,
    /// The storage access module.
    storage_access_module: ModuleId,
    /// The `storage_base_address_from_felt252` libfunc.
    storage_base_address_from_felt252: ExternFunctionId,
    /// Type ranges.
    type_value_ranges: OrderedHashMap<TypeId, TypeInfo>,
}
impl ConstFoldingLibfuncInfo {
    fn new(db: &dyn LoweringGroup) -> Self {
        let core = ModuleHelper::core(db.upcast());
        let felt_sub = core.extern_function_id("felt252_sub");
        let box_module = core.submodule("box");
        let into_box = box_module.extern_function_id("into_box");
        let integer_module = core.submodule("integer");
        let bounded_int_module = core.submodule("internal").submodule("bounded_int");
        let upcast = integer_module.extern_function_id("upcast");
        let downcast = integer_module.extern_function_id("downcast");
        let array_module = core.submodule("array");
        let array_get = array_module.extern_function_id("array_get");
        let starknet_module = core.submodule("starknet");
        let storage_access_module = starknet_module.submodule("storage_access");
        let storage_base_address_from_felt252 =
            storage_access_module.extern_function_id("storage_base_address_from_felt252");
        let nz_fns = OrderedHashSet::<_>::from_iter(chain!(
            [
                core.extern_function_id("felt252_is_zero"),
                bounded_int_module.extern_function_id("bounded_int_is_zero")
            ],
            ["u8", "u16", "u32", "u64", "u128", "u256", "i8", "i16", "i32", "i64", "i128"]
                .map(|ty| integer_module.extern_function_id(format!("{ty}_is_zero")))
        ));
        let utypes = ["u8", "u16", "u32", "u64", "u128"];
        let itypes = ["i8", "i16", "i32", "i64", "i128"];
        let eq_fns = OrderedHashSet::<_>::from_iter(
            chain!(utypes, itypes).map(|ty| integer_module.extern_function_id(format!("{ty}_eq"))),
        );
        let uadd_fns = OrderedHashSet::<_>::from_iter(
            utypes.map(|ty| integer_module.extern_function_id(format!("{ty}_overflowing_add"))),
        );
        let usub_fns = OrderedHashSet::<_>::from_iter(
            utypes.map(|ty| integer_module.extern_function_id(format!("{ty}_overflowing_sub"))),
        );
        let diff_fns = OrderedHashSet::<_>::from_iter(
            itypes.map(|ty| integer_module.extern_function_id(format!("{ty}_diff"))),
        );
        let iadd_fns = OrderedHashSet::<_>::from_iter(
            itypes
                .map(|ty| integer_module.extern_function_id(format!("{ty}_overflowing_add_impl"))),
        );
        let isub_fns = OrderedHashSet::<_>::from_iter(
            itypes
                .map(|ty| integer_module.extern_function_id(format!("{ty}_overflowing_sub_impl"))),
        );
        let wide_mul_fns = OrderedHashSet::<_>::from_iter(chain!(
            [bounded_int_module.extern_function_id("bounded_int_mul")],
            ["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64"]
                .map(|ty| integer_module.extern_function_id(format!("{ty}_wide_mul"))),
        ));
        let div_rem_fns = OrderedHashSet::<_>::from_iter(chain!(
            [bounded_int_module.extern_function_id("bounded_int_div_rem")],
            utypes.map(|ty| integer_module.extern_function_id(format!("{ty}_safe_divmod"))),
        ));
        let bounded_int_add = bounded_int_module.extern_function_id("bounded_int_add");
        let bounded_int_sub = bounded_int_module.extern_function_id("bounded_int_sub");
        let bounded_int_constrain = bounded_int_module.extern_function_id("bounded_int_constrain");
        let type_value_ranges = OrderedHashMap::from_iter(
            [
                ("u8", BigInt::ZERO, u8::MAX.into()),
                ("u16", BigInt::ZERO, u16::MAX.into()),
                ("u32", BigInt::ZERO, u32::MAX.into()),
                ("u64", BigInt::ZERO, u64::MAX.into()),
                ("u128", BigInt::ZERO, u128::MAX.into()),
                ("u256", BigInt::ZERO, BigInt::from(1) << 256),
                ("i8", i8::MIN.into(), i8::MAX.into()),
                ("i16", i16::MIN.into(), i16::MAX.into()),
                ("i32", i32::MIN.into(), i32::MAX.into()),
                ("i64", i64::MIN.into(), i64::MAX.into()),
                ("i128", i128::MIN.into(), i128::MAX.into()),
            ]
            .map(|(ty, min, max): (&str, BigInt, BigInt)| {
                let info = TypeInfo {
                    min,
                    max,
                    is_zero: integer_module
                        .function_id(format!("{ty}_is_zero"), vec![])
                        .lowered(db),
                };
                (corelib::get_core_ty_by_name(db.upcast(), ty.into(), vec![]), info)
            }),
        );
        Self {
            felt_sub,
            into_box,
            upcast,
            downcast,
            nz_fns,
            eq_fns,
            uadd_fns,
            usub_fns,
            diff_fns,
            iadd_fns,
            isub_fns,
            wide_mul_fns,
            div_rem_fns,
            bounded_int_add,
            bounded_int_sub,
            bounded_int_constrain,
            array_module: array_module.id,
            array_get,
            storage_access_module: storage_access_module.id,
            storage_base_address_from_felt252,
            type_value_ranges,
        }
    }
}

impl std::ops::Deref for ConstFoldingContext<'_> {
    type Target = ConstFoldingLibfuncInfo;
    fn deref(&self) -> &ConstFoldingLibfuncInfo {
        self.libfunc_info
    }
}

/// The information of a type required for const foldings.
#[derive(Debug, PartialEq, Eq)]
struct TypeInfo {
    /// The minimum value of the type.
    min: BigInt,
    /// The maximum value of the type.
    max: BigInt,
    /// The function to check if the value is zero for the type.
    is_zero: FunctionId,
}
impl TypeInfo {
    /// Normalizes the value to the range.
    /// Assumes the value is within size of range of the range.
    fn normalized(&self, value: BigInt) -> NormalizedResult {
        if value < self.min {
            NormalizedResult::Under(value - &self.min + &self.max + 1)
        } else if value > self.max {
            NormalizedResult::Over(value + &self.min - &self.max - 1)
        } else {
            NormalizedResult::InRange(value)
        }
    }
}

/// The result of normalizing a value to a range.
enum NormalizedResult {
    /// The original value is in the range, carries the value, or an equivalent value.
    InRange(BigInt),
    /// The original value is larger than range max, carries the normalized value.
    Over(BigInt),
    /// The original value is smaller than range min, carries the normalized value.
    Under(BigInt),
}
