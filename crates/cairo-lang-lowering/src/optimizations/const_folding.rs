#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_defs::ids::{ExternFunctionId, ModuleId, ModuleItemId};
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{corelib, GenericArgumentId, TypeId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{extract_matches, try_extract_matches, Intern, LookupIntern};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::Zero;
use smol_str::SmolStr;

use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementConst, StatementDesnap, StatementEnumConstruct,
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
        variables: &lowered.variables,
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
                    if let Some((updated_stmt, additional)) = ctx.handle_statement_call(call_stmt) {
                        *stmt = Statement::Const(updated_stmt);
                        additional_consts.extend(additional);
                    }
                }
                Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                    let mut const_args = vec![];
                    let mut all_args = vec![];
                    let mut contains_info = false;
                    for input in inputs.iter() {
                        let Some(info) = ctx.var_info.get(&input.var_id) else {
                            all_args.push(
                                lowered.variables[input.var_id]
                                    .copyable
                                    .is_ok()
                                    .then(|| VarInfo::Var(*input)),
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
                        let value = ConstValue::Struct(const_args, lowered.variables[*output].ty);
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
    variables: &'a Arena<Variable>,
    /// The accumulated information about the const values of variables.
    var_info: UnorderedHashMap<VariableId, VarInfo>,
    /// The libfunc information.
    libfunc_info: &'a ConstFoldingLibfuncInfo,
}

impl<'a> ConstFoldingContext<'a> {
    /// Handles a statement call.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns an updated const-statement (to override the current
    /// statement), and a possible additional const-statement, if multiple statements are required
    /// for replacing the existing statement.
    fn handle_statement_call(
        &mut self,
        stmt: &mut StatementCall,
    ) -> Option<(StatementConst, Option<StatementConst>)> {
        let id = stmt.function.get_extern(self.db)?;
        if id == self.felt_sub {
            // (a - 0) can be replaced by a.
            let val = self.as_int(stmt.inputs[1].var_id)?;
            if val.is_zero() {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
            }
            None
        } else if self.wide_mul_fns.contains(&id)
            || id == self.bounded_int_add
            || id == self.bounded_int_sub
        {
            let (lhs, nz_ty) = self.as_int_ex(stmt.inputs[0].var_id)?;
            let rhs = self.as_int(stmt.inputs[1].var_id)?;
            let value = if id == self.bounded_int_add {
                lhs + rhs
            } else if id == self.bounded_int_sub {
                lhs - rhs
            } else {
                lhs * rhs
            };
            let output = stmt.outputs[0];
            let mut value = ConstValue::Int(value, self.variables[output].ty);
            if nz_ty {
                value = ConstValue::NonZero(Box::new(value));
            }
            self.var_info.insert(output, VarInfo::Const(value.clone()));
            Some((StatementConst { value, output }, None))
        } else if self.div_rem_fns.contains(&id) {
            let lhs = self.as_int(stmt.inputs[0].var_id)?;
            let rhs = self.as_int(stmt.inputs[1].var_id)?;
            let (q, r) = lhs.div_rem(rhs);
            let q_output = stmt.outputs[0];
            let q_value = ConstValue::Int(q, self.variables[q_output].ty);
            self.var_info.insert(q_output, VarInfo::Const(q_value.clone()));
            let r_output = stmt.outputs[1];
            let r_value = ConstValue::Int(r, self.variables[r_output].ty);
            self.var_info.insert(r_output, VarInfo::Const(r_value.clone()));
            Some((
                StatementConst { value: q_value, output: q_output },
                Some(StatementConst { value: r_value, output: r_output }),
            ))
        } else if id == self.storage_base_address_from_felt252 {
            let input_var = stmt.inputs[0].var_id;
            if let Some(ConstValue::Int(val, ty)) = self.as_const(input_var) {
                stmt.inputs.clear();
                stmt.function = ModuleHelper { db: self.db, id: self.storage_access_module }
                    .function_id(
                        "storage_base_address_const",
                        vec![GenericArgumentId::Constant(
                            ConstValue::Int(val.clone(), *ty).intern(self.db),
                        )],
                    );
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
            Some((StatementConst { value, output: stmt.outputs[0] }, None))
        } else if id == self.upcast {
            let int_value = self.as_int(stmt.inputs[0].var_id)?;
            let output = stmt.outputs[0];
            let value = ConstValue::Int(int_value.clone(), self.variables[output].ty);
            self.var_info.insert(output, VarInfo::Const(value.clone()));
            Some((StatementConst { value, output }, None))
        } else {
            None
        }
    }

    /// Handles the end of an extern block.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns a possible additional const-statement to the block, as well
    /// as an updated block end.
    fn handle_extern_block_end(
        &mut self,
        info: &mut MatchExternInfo,
    ) -> Option<(Option<StatementConst>, FlatBlockEnd)> {
        let id = info.function.get_extern(self.db)?;
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
            let lhs = self.as_int(info.inputs[0].var_id)?;
            let rhs = self.as_int(info.inputs[1].var_id)?;
            Some((
                None,
                FlatBlockEnd::Goto(
                    info.arms[if lhs == rhs { 1 } else { 0 }].block_id,
                    Default::default(),
                ),
            ))
        } else if self.uadd_fns.contains(&id)
            || self.usub_fns.contains(&id)
            || self.iadd_fns.contains(&id)
            || self.isub_fns.contains(&id)
        {
            let lhs = self.as_int(info.inputs[0].var_id)?;
            let rhs = self.as_int(info.inputs[1].var_id)?;
            let value = if self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id) {
                lhs + rhs
            } else {
                lhs - rhs
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
            let semantic_id =
                extract_matches!(info.function.lookup_intern(self.db), FunctionLongId::Semantic);
            let generic_arg = semantic_id.get_concrete(self.db.upcast()).generic_args[1];
            let constrain_value = extract_matches!(generic_arg, GenericArgumentId::Constant)
                .lookup_intern(self.db)
                .into_int()
                .unwrap();
            let arm_idx = if value < &constrain_value { 0 } else { 1 };
            let output = info.arms[arm_idx].var_ids[0];
            let mut value = ConstValue::Int(value.clone(), self.variables[output].ty);
            if nz_ty {
                value = ConstValue::NonZero(Box::new(value));
            }
            self.var_info.insert(output, VarInfo::Const(value.clone()));
            Some((
                Some(StatementConst { value, output }),
                FlatBlockEnd::Goto(info.arms[arm_idx].block_id, Default::default()),
            ))
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

/// Helper for getting functions in the corelib.
struct ModuleHelper<'a> {
    /// The db.
    db: &'a dyn LoweringGroup,
    /// The current module id.
    id: ModuleId,
}
impl<'a> ModuleHelper<'a> {
    /// Returns a helper for the core module.
    fn core(db: &'a dyn LoweringGroup) -> Self {
        Self { db, id: corelib::core_module(db.upcast()) }
    }
    /// Returns a helper for a submodule named `name` of the current module.
    fn submodule(&self, name: &str) -> Self {
        let id = corelib::get_submodule(self.db.upcast(), self.id, name).unwrap_or_else(|| {
            panic!("`{name}` missing in `{}`.", self.id.full_path(self.db.upcast()))
        });
        Self { db: self.db, id }
    }
    /// Returns the id of an extern function named `name` in the current module.
    fn extern_function_id(&self, name: impl Into<SmolStr>) -> ExternFunctionId {
        let name = name.into();
        let Ok(Some(ModuleItemId::ExternFunction(id))) =
            self.db.module_item_by_name(self.id, name.clone())
        else {
            panic!("`{}` not found in `{}`.", name, self.id.full_path(self.db.upcast()));
        };
        id
    }
    /// Returns the id of a function named `name` in the current module, with the given
    /// `generic_args`.
    fn function_id(
        &self,
        name: impl Into<SmolStr>,
        generic_args: Vec<GenericArgumentId>,
    ) -> FunctionId {
        FunctionLongId::Semantic(corelib::get_function_id(
            self.db.upcast(),
            self.id,
            name.into(),
            generic_args,
        ))
        .intern(self.db)
    }
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
    /// The `storage_base_address_from_felt252` libfunc.
    storage_base_address_from_felt252: ExternFunctionId,
    /// The set of functions that check if a number is zero.
    nz_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions that check if numbers are equal.
    eq_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to add unsigned ints.
    uadd_fns: OrderedHashSet<ExternFunctionId>,
    /// The set of functions to subtract unsigned ints.
    usub_fns: OrderedHashSet<ExternFunctionId>,
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
    /// The storage access module.
    storage_access_module: ModuleId,
    /// Type ranges.
    type_value_ranges: OrderedHashMap<TypeId, TypeRange>,
}
impl ConstFoldingLibfuncInfo {
    fn new(db: &dyn LoweringGroup) -> Self {
        let core = ModuleHelper::core(db);
        let felt_sub = core.extern_function_id("felt252_sub");
        let box_module = core.submodule("box");
        let into_box = box_module.extern_function_id("into_box");
        let integer_module = core.submodule("integer");
        let bounded_int_module = core.submodule("internal").submodule("bounded_int");
        let upcast = integer_module.extern_function_id("upcast");
        let downcast = integer_module.extern_function_id("downcast");
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
        let usub_fns = OrderedHashSet::<_>::from_iter(chain!(
            utypes.map(|ty| integer_module.extern_function_id(format!("{ty}_overflowing_sub"))),
            // Considering `i*_diff` as `usub` operations - as they act exactly the same.
            itypes.map(|ty| integer_module.extern_function_id(format!("{ty}_diff"))),
        ));
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
                ("u8", TypeRange::closed(0, u8::MAX)),
                ("u16", TypeRange::closed(0, u16::MAX)),
                ("u32", TypeRange::closed(0, u32::MAX)),
                ("u64", TypeRange::closed(0, u64::MAX)),
                ("u128", TypeRange::closed(0, u128::MAX)),
                ("u256", TypeRange::closed(0, BigInt::from(1) << 256)),
                ("i8", TypeRange::closed(i8::MIN, i8::MAX)),
                ("i16", TypeRange::closed(i16::MIN, i16::MAX)),
                ("i32", TypeRange::closed(i32::MIN, i32::MAX)),
                ("i64", TypeRange::closed(i64::MIN, i64::MAX)),
                ("i128", TypeRange::closed(i128::MIN, i128::MAX)),
            ]
            .map(|(ty, range)| {
                (corelib::get_core_ty_by_name(db.upcast(), ty.into(), vec![]), range)
            }),
        );
        Self {
            felt_sub,
            into_box,
            upcast,
            downcast,
            storage_base_address_from_felt252,
            nz_fns,
            eq_fns,
            uadd_fns,
            usub_fns,
            iadd_fns,
            isub_fns,
            wide_mul_fns,
            div_rem_fns,
            bounded_int_add,
            bounded_int_sub,
            bounded_int_constrain,
            storage_access_module: storage_access_module.id,
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

/// The range of a type for normalizations.
#[derive(Debug, PartialEq, Eq)]
struct TypeRange {
    min: BigInt,
    max: BigInt,
}
impl TypeRange {
    fn closed(min: impl Into<BigInt>, max: impl Into<BigInt>) -> Self {
        Self { min: min.into(), max: max.into() }
    }
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
