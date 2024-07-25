#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use cairo_lang_defs::ids::{ExternFunctionId, ModuleId, ModuleItemId};
use cairo_lang_semantic::corelib::{self};
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::GenericArgumentId;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::{try_extract_matches, Intern};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
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
enum VarInfo {
    /// The variable is a const value.
    Const(ConstValue),
    /// The variable can be replaced by another variable.
    Var(VarUsage),
}

/// Performs constant folding on the lowered program.
/// The optimization works better when the blocks are topologically sorted.
pub fn const_folding(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }

    // Note that we can keep the var_info across blocks because the lowering
    // is in static single assignment form.
    let mut ctx = ConstFoldingContext::new(db, &lowered.variables);
    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &mut lowered.blocks[block_id];
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
                    if let Some(VarInfo::Const(val)) = ctx.var_info.get(&stmt.input.var_id) {
                        let val = val.clone();
                        // TODO(Tomerstarkware): add snapshot to value type.
                        ctx.var_info.insert(stmt.original(), VarInfo::Const(val.clone()));
                        ctx.var_info.insert(stmt.snapshot(), VarInfo::Const(val));
                    }
                }
                Statement::Desnap(StatementDesnap { input, output }) => {
                    if let Some(VarInfo::Const(val)) = ctx.var_info.get(&input.var_id) {
                        let val = val.clone();
                        // TODO(Tomerstarkware): remove snapshot from value type.
                        ctx.var_info.insert(*output, VarInfo::Const(val));
                    }
                }
                Statement::Call(call_stmt) => {
                    if let Some(updated_stmt) = ctx.handle_statement_call(call_stmt) {
                        *stmt = updated_stmt;
                    }
                }
                Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                    if let Some(args) = inputs
                        .iter()
                        .map(|input| {
                            if let Some(VarInfo::Const(val)) = ctx.var_info.get(&input.var_id) {
                                Some(val.clone())
                            } else {
                                None
                            }
                        })
                        .collect::<Option<Vec<_>>>()
                    {
                        let value = ConstValue::Struct(args, lowered.variables[*output].ty);
                        ctx.var_info.insert(*output, VarInfo::Const(value));
                    }
                }
                Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                    if let Some(VarInfo::Const(ConstValue::Struct(args, _))) =
                        ctx.var_info.get(&input.var_id)
                    {
                        for (output, val) in zip_eq(outputs, args.clone()) {
                            ctx.var_info.insert(*output, VarInfo::Const(val));
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
                                block.statements.push(stmt);
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
    libfunc_info: LibfuncInfo<'a>,
}

impl<'a> ConstFoldingContext<'a> {
    fn new(db: &'a dyn LoweringGroup, variables: &'a Arena<Variable>) -> Self {
        Self {
            db,
            var_info: UnorderedHashMap::default(),
            variables,
            libfunc_info: LibfuncInfo::new(db),
        }
    }

    /// Handles a statement call.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns an updated statement.
    fn handle_statement_call(&mut self, stmt: &mut StatementCall) -> Option<Statement> {
        // (a - 0) can be replaced by a.
        if stmt.function == self.felt_sub {
            let val = self.as_int(stmt.inputs[1].var_id)?;
            if val.is_zero() {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
            }
        } else if stmt.function == self.storage_base_address_from_felt252 {
            let input_var = stmt.inputs[0].var_id;
            if let Some(ConstValue::Int(val, ty)) = self.as_const(input_var) {
                stmt.inputs.clear();
                stmt.function = self.storage_access_module.function_id(
                    "storage_base_address_const",
                    vec![GenericArgumentId::Constant(
                        ConstValue::Int(val.clone(), *ty).intern(self.db),
                    )],
                );
            }
        } else if let Some(extrn) = stmt.function.get_extern(self.db) {
            if extrn == self.into_box {
                let val = self.as_const(stmt.inputs[0].var_id)?;
                let value = ConstValue::Boxed(val.clone().into());
                // Not inserting the value into the `var_info` map because the
                // resulting box isn't an actual const at the Sierra level.
                return Some(Statement::Const(StatementConst { value, output: stmt.outputs[0] }));
            } else if extrn == self.upcast {
                let int_value = self.as_int(stmt.inputs[0].var_id)?;
                let value = ConstValue::Int(int_value.clone(), self.variables[stmt.outputs[0]].ty);
                self.var_info.insert(stmt.outputs[0], VarInfo::Const(value.clone()));
                return Some(Statement::Const(StatementConst { value, output: stmt.outputs[0] }));
            }
        }
        None
    }

    /// Handles the end of an extern block.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns a possible additional statement to the block, as well as an
    /// updated block end.
    fn handle_extern_block_end(
        &mut self,
        info: &mut MatchExternInfo,
    ) -> Option<(Option<Statement>, FlatBlockEnd)> {
        if self.nz_fns.contains(&info.function) {
            let val = self.as_const(info.inputs[0].var_id)?;
            let is_zero = match val {
                ConstValue::Int(v, _) => v.is_zero(),
                ConstValue::Struct(s, _) => s.iter().all(|v| {
                    v.clone().into_int().expect("Expected ConstValue::Int for size").is_zero()
                }),
                _ => unreachable!(),
            };
            return Some(if is_zero {
                (None, FlatBlockEnd::Goto(info.arms[0].block_id, Default::default()))
            } else {
                let arm = &info.arms[1];
                let nz_var = arm.var_ids[0];
                let nz_val = ConstValue::NonZero(Box::new(val.clone()));
                self.var_info.insert(nz_var, VarInfo::Const(nz_val.clone()));
                (
                    Some(Statement::Const(StatementConst { value: nz_val, output: nz_var })),
                    FlatBlockEnd::Goto(arm.block_id, Default::default()),
                )
            });
        } else if let Some(extrn) = info.function.get_extern(self.db) {
            if extrn == self.downcast {
                let input_var = info.inputs[0].var_id;
                let Some(VarInfo::Const(ConstValue::Int(value, _))) = self.var_info.get(&input_var)
                else {
                    return None;
                };
                let success_output = info.arms[0].var_ids[0];
                let ty = self.variables[success_output].ty;
                return Some(
                    if corelib::validate_literal(self.db.upcast(), ty, value.clone()).is_ok() {
                        let value = ConstValue::Int(value.clone(), ty);
                        self.var_info.insert(success_output, VarInfo::Const(value.clone()));
                        (
                            Some(Statement::Const(StatementConst {
                                value,
                                output: success_output,
                            })),
                            FlatBlockEnd::Goto(info.arms[0].block_id, Default::default()),
                        )
                    } else {
                        (None, FlatBlockEnd::Goto(info.arms[1].block_id, Default::default()))
                    },
                );
            }
        }
        None
    }

    /// Returns the const value of a variable if it exists.
    fn as_const(&self, var_id: VariableId) -> Option<&ConstValue> {
        try_extract_matches!(self.var_info.get(&var_id)?, VarInfo::Const)
    }

    /// Return the const value as a int if it exists and is an integer.
    fn as_int(&self, var_id: VariableId) -> Option<&BigInt> {
        if let ConstValue::Int(value, _) = self.as_const(var_id)? { Some(value) } else { None }
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
    fn extern_function_id(&self, name: &str) -> ExternFunctionId {
        let Ok(Some(ModuleItemId::ExternFunction(id))) =
            self.db.module_item_by_name(self.id, name.into())
        else {
            panic!("`{name}` not found in `{}`.", self.id.full_path(self.db.upcast()));
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
struct LibfuncInfo<'a> {
    /// The `felt252_sub` libfunc.
    felt_sub: FunctionId,
    /// The `into_box` libfunc.
    into_box: ExternFunctionId,
    /// The `upcast` libfunc.
    upcast: ExternFunctionId,
    /// The `downcast` libfunc.
    downcast: ExternFunctionId,
    /// The `storage_base_address_from_felt252` libfunc.
    storage_base_address_from_felt252: FunctionId,
    /// The set of functions that check if a number is zero.
    nz_fns: UnorderedHashSet<FunctionId>,
    /// The storage access module.
    storage_access_module: ModuleHelper<'a>,
}
impl<'a> LibfuncInfo<'a> {
    fn new(db: &'a dyn LoweringGroup) -> Self {
        let core = ModuleHelper::core(db);
        let felt_sub = core.function_id("felt252_sub", vec![]);
        let box_module = core.submodule("box");
        let into_box = box_module.extern_function_id("into_box");
        let integer_module = core.submodule("integer");
        let upcast = integer_module.extern_function_id("upcast");
        let downcast = integer_module.extern_function_id("downcast");
        let starknet_module = core.submodule("starknet");
        let storage_access_module = starknet_module.submodule("storage_access");
        let storage_base_address_from_felt252 =
            storage_access_module.function_id("storage_base_address_from_felt252", vec![]);
        let nz_fns = UnorderedHashSet::<_>::from_iter(chain!(
            [core.function_id("felt252_is_zero", vec![])],
            ["u8", "u16", "u32", "u64", "u128", "u256", "i8", "i16", "i32", "i64", "i128"]
                .map(|ty| integer_module.function_id(format!("{}_is_zero", ty), vec![]))
        ));
        Self {
            felt_sub,
            into_box,
            upcast,
            downcast,
            storage_base_address_from_felt252,
            nz_fns,
            storage_access_module,
        }
    }
}

impl<'a> std::ops::Deref for ConstFoldingContext<'a> {
    type Target = LibfuncInfo<'a>;
    fn deref(&self) -> &LibfuncInfo<'a> {
        &self.libfunc_info
    }
}
