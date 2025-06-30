#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_defs::ids::{ExternFunctionId, FreeFunctionId};
use cairo_lang_semantic::corelib::try_extract_nz_wrapped_type;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::{ConstCalcInfo, ConstValue};
use cairo_lang_semantic::items::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::types::TypeSizeInformation;
use cairo_lang_semantic::{
    ConcreteTypeId, GenericArgumentId, MatchArmSelector, TypeId, TypeLongId, corelib,
};
use cairo_lang_utils::byte_array::BYTE_ARRAY_MAGIC;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, extract_matches, try_extract_matches};
use id_arena::Arena;
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::cast::ToPrimitive;
use num_traits::{Num, One, Zero};

use crate::db::LoweringGroup;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, FunctionId, SemanticFunctionIdEx,
    SpecializedFunction,
};
use crate::specialization::SpecializationArg;
use crate::{
    Block, BlockEnd, BlockId, Lowered, MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo,
    Statement, StatementCall, StatementConst, StatementDesnap, StatementEnumConstruct,
    StatementSnapshot, StatementStructConstruct, StatementStructDestructure, VarUsage, Variable,
    VariableId,
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
    /// The variable is a box of another variable.
    Box(Box<VarInfo>),
    /// The variable is an array of known size of other variables.
    /// `None` values represent variables that are not tracked.
    Array(Vec<Option<VarInfo>>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Reachability {
    /// The block is reachable from the function start only through the goto at the end of the given
    /// block.
    FromSingleGoto(BlockId),
    /// The block is reachable from the function start after const-folding - just does not fit
    /// `FromSingleGoto`.
    Any,
}

/// Performs constant folding on the lowered program.
/// The optimization only works when the blocks are topologically sorted.
pub fn const_folding(
    db: &dyn LoweringGroup,
    function_id: ConcreteFunctionWithBodyId,
    lowered: &mut Lowered,
) {
    if lowered.blocks.is_empty() {
        return;
    }

    // Note that we can keep the var_info across blocks because the lowering
    // is in static single assignment form.
    let mut ctx = ConstFoldingContext::new(db, function_id, &mut lowered.variables);

    if ctx.should_skip_const_folding(db) {
        return;
    }

    for block_id in (0..lowered.blocks.len()).map(BlockId) {
        if !ctx.visit_block_start(block_id, |block_id| &lowered.blocks[block_id]) {
            continue;
        }

        let block = &mut lowered.blocks[block_id];
        for stmt in block.statements.iter_mut() {
            ctx.visit_statement(stmt);
        }
        ctx.visit_block_end(block_id, block);
    }
}

pub struct ConstFoldingContext<'a> {
    /// The used database.
    db: &'a dyn LoweringGroup,
    /// The variables arena, mostly used to get the type of variables.
    pub variables: &'a mut Arena<Variable>,
    /// The accumulated information about the const values of variables.
    var_info: UnorderedHashMap<VariableId, VarInfo>,
    /// The libfunc information.
    libfunc_info: Arc<ConstFoldingLibfuncInfo>,
    /// The specialization base of the caller function (or the caller if the function is not
    /// specialized).
    caller_base: ConcreteFunctionWithBodyId,
    /// Reachability of blocks from the function start.
    /// If the block is not in this map, it means that it is unreachable (or that it was already
    /// visited and its reachability won't be checked again).
    reachability: UnorderedHashMap<BlockId, Reachability>,
    /// Additional statements to add to the block.
    additional_stmts: Vec<Statement>,
}

impl<'a> ConstFoldingContext<'a> {
    pub fn new(
        db: &'a dyn LoweringGroup,
        function_id: ConcreteFunctionWithBodyId,
        variables: &'a mut Arena<Variable>,
    ) -> Self {
        let caller_base = match function_id.lookup_intern(db) {
            ConcreteFunctionWithBodyLongId::Specialized(specialized_func) => specialized_func.base,
            _ => function_id,
        };

        Self {
            db,
            var_info: UnorderedHashMap::default(),
            variables,
            libfunc_info: priv_const_folding_info(db),
            caller_base,
            reachability: UnorderedHashMap::from_iter([(BlockId::root(), Reachability::Any)]),
            additional_stmts: vec![],
        }
    }

    /// Determines if a block is reachable from the function start and propagates constant values
    /// when the block is reachable via a single goto statement.
    pub fn visit_block_start<'b>(
        &mut self,
        block_id: BlockId,
        get_block: impl FnOnce(BlockId) -> &'b Block,
    ) -> bool {
        let Some(reachability) = self.reachability.remove(&block_id) else {
            return false;
        };
        match reachability {
            Reachability::Any => {}
            Reachability::FromSingleGoto(from_block) => match &get_block(from_block).end {
                BlockEnd::Goto(_, remapping) => {
                    for (dst, src) in remapping.iter() {
                        if let Some(v) = self.as_const(src.var_id) {
                            self.var_info.insert(*dst, VarInfo::Const(v.clone()));
                        }
                    }
                }
                _ => unreachable!("Expected a goto end"),
            },
        }
        true
    }

    /// Processes a statement and applies the constant folding optimizations.
    ///
    /// This method performs the following operations:
    /// - Updates the `var_info` map with constant values of variables
    /// - Replace the input statement with optimized versions when possible
    /// - Updates `self.additional_stmts` with statements that need to be added to the block.
    ///
    /// Note: `self.visit_block_end` must be called after processing all statements
    /// in a block to actually add the additional statements.
    pub fn visit_statement(&mut self, stmt: &mut Statement) {
        self.maybe_replace_inputs(stmt.inputs_mut());
        match stmt {
            Statement::Const(StatementConst { value, output }) => match value {
                value @ (ConstValue::Int(..)
                | ConstValue::Struct(..)
                | ConstValue::Enum(..)
                | ConstValue::NonZero(..)) => {
                    self.var_info.insert(*output, VarInfo::Const(value.clone()));
                }
                ConstValue::Boxed(inner) => {
                    self.var_info.insert(
                        *output,
                        VarInfo::Box(VarInfo::Const(inner.as_ref().clone()).into()),
                    );
                }
                ConstValue::Generic(_)
                | ConstValue::ImplConstant(_)
                | ConstValue::Var(..)
                | ConstValue::Missing(_) => {}
            },
            Statement::Snapshot(stmt) => {
                if let Some(info) = self.var_info.get(&stmt.input.var_id).cloned() {
                    self.var_info.insert(stmt.original(), info.clone());
                    self.var_info.insert(stmt.snapshot(), VarInfo::Snapshot(info.into()));
                }
            }
            Statement::Desnap(StatementDesnap { input, output }) => {
                if let Some(VarInfo::Snapshot(info)) = self.var_info.get(&input.var_id) {
                    self.var_info.insert(*output, info.as_ref().clone());
                }
            }
            Statement::Call(call_stmt) => {
                if let Some(updated_stmt) = self.handle_statement_call(call_stmt) {
                    *stmt = updated_stmt;
                } else if let Some(updated_stmt) = self.try_specialize_call(call_stmt) {
                    *stmt = updated_stmt;
                }
            }
            Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                let mut const_args = vec![];
                let mut all_args = vec![];
                let mut contains_info = false;
                for input in inputs.iter() {
                    let Some(info) = self.var_info.get(&input.var_id) else {
                        all_args.push(var_info_if_copy(self.variables, *input));
                        continue;
                    };
                    contains_info = true;
                    if let VarInfo::Const(value) = info {
                        const_args.push(value.clone());
                    }
                    all_args.push(Some(info.clone()));
                }
                if const_args.len() == inputs.len() {
                    let value = ConstValue::Struct(const_args, self.variables[*output].ty);
                    self.var_info.insert(*output, VarInfo::Const(value));
                } else if contains_info {
                    self.var_info.insert(*output, VarInfo::Struct(all_args));
                }
            }
            Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                if let Some(mut info) = self.var_info.get(&input.var_id) {
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
                                self.var_info
                                    .insert(*output, wrap_with_snapshots(VarInfo::Const(value)));
                            }
                        }
                        VarInfo::Struct(members) => {
                            for (output, member) in zip_eq(outputs, members.clone()) {
                                if let Some(member) = member {
                                    self.var_info.insert(*output, wrap_with_snapshots(member));
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                if let Some(VarInfo::Const(val)) = self.var_info.get(&input.var_id) {
                    let value = ConstValue::Enum(*variant, val.clone().into());
                    self.var_info.insert(*output, VarInfo::Const(value.clone()));
                }
            }
        }
    }

    /// Processes the block's end and incorporates additional statements into the block.
    ///
    /// This method handles the following tasks:
    /// - Inserts the accumulated additional statements into the block.
    /// - Converts match endings to goto when applicable.
    /// - Updates self.reachability based on the block's ending.
    pub fn visit_block_end(&mut self, block_id: BlockId, block: &mut crate::Block) {
        block.statements.splice(0..0, self.additional_stmts.drain(..));

        match &mut block.end {
            BlockEnd::Goto(_, remappings) => {
                for (_, v) in remappings.iter_mut() {
                    self.maybe_replace_input(v);
                }
            }
            BlockEnd::Match { info } => {
                self.maybe_replace_inputs(info.inputs_mut());
                match info {
                    MatchInfo::Enum(MatchEnumInfo { input, arms, .. }) => {
                        if let Some(VarInfo::Const(ConstValue::Enum(variant, value))) =
                            self.var_info.get(&input.var_id)
                        {
                            let arm = &arms[variant.idx];
                            let value = value.as_ref().clone();
                            let output = arm.var_ids[0];
                            if self.variables[input.var_id].droppable.is_ok()
                                && self.variables[output].copyable.is_ok()
                            {
                                if let Some(stmt) =
                                    self.try_generate_const_statement(&value, output)
                                {
                                    block.statements.push(stmt);
                                    block.end = BlockEnd::Goto(arm.block_id, Default::default());
                                }
                            }
                            self.var_info.insert(output, VarInfo::Const(value));
                        }
                    }
                    MatchInfo::Value(info) => {
                        if let Some(value) =
                            self.as_int(info.input.var_id).and_then(|x| x.to_usize())
                        {
                            if let Some(arm) = info.arms.iter().find(|arm| {
                                matches!(
                                    &arm.arm_selector,
                                    MatchArmSelector::Value(v) if v.value == value
                                )
                            }) {
                                // Create the variable that was previously introduced in match arm.
                                block.statements.push(Statement::StructConstruct(
                                    StatementStructConstruct {
                                        inputs: vec![],
                                        output: arm.var_ids[0],
                                    },
                                ));
                                block.end = BlockEnd::Goto(arm.block_id, Default::default());
                            }
                        }
                    }
                    MatchInfo::Extern(info) => {
                        if let Some((extra_stmts, updated_end)) = self.handle_extern_block_end(info)
                        {
                            block.statements.extend(extra_stmts);
                            block.end = updated_end;
                        }
                    }
                }
            }
            BlockEnd::Return(ref mut inputs, _) => self.maybe_replace_inputs(inputs),
            BlockEnd::Panic(_) | BlockEnd::NotSet => unreachable!(),
        }
        match &block.end {
            BlockEnd::Goto(dst_block_id, _) => {
                match self.reachability.entry(*dst_block_id) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        e.insert(Reachability::Any)
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        *e.insert(Reachability::FromSingleGoto(block_id))
                    }
                };
            }
            BlockEnd::Match { info } => {
                for arm in info.arms() {
                    assert!(self.reachability.insert(arm.block_id, Reachability::Any).is_none());
                }
            }
            BlockEnd::NotSet | BlockEnd::Return(..) | BlockEnd::Panic(..) => {}
        }
    }

    /// Handles a statement call.
    ///
    /// Returns None if no additional changes are required.
    /// If changes are required, returns an updated statement (to override the current
    /// statement).
    /// May add additional statements to `self.additional_stmts` if just replacing the current
    /// statement is not enough.
    fn handle_statement_call(&mut self, stmt: &mut StatementCall) -> Option<Statement> {
        let db = self.db;
        if stmt.function == self.panic_with_felt252 {
            let val = self.as_const(stmt.inputs[0].var_id)?;
            stmt.inputs.clear();
            stmt.function = GenericFunctionId::Free(self.panic_with_const_felt252)
                .concretize(db, vec![GenericArgumentId::Constant(val.clone().intern(db))])
                .lowered(db);
            return None;
        } else if stmt.function == self.panic_with_byte_array {
            let snap = self.var_info.get(&stmt.inputs[0].var_id)?;
            let bytearray = try_extract_matches!(snap, VarInfo::Snapshot)?;
            let [
                Some(VarInfo::Array(data)),
                Some(VarInfo::Const(ConstValue::Int(pending_word, _))),
                Some(VarInfo::Const(ConstValue::Int(pending_len, _))),
            ] = &try_extract_matches!(bytearray.as_ref(), VarInfo::Struct)?[..]
            else {
                return None;
            };
            let mut panic_data =
                vec![BigInt::from_str_radix(BYTE_ARRAY_MAGIC, 16).unwrap(), data.len().into()];
            for word in data {
                let Some(VarInfo::Const(ConstValue::Int(word, _))) = word else {
                    continue;
                };
                panic_data.push(word.clone());
            }
            panic_data.extend([pending_word.clone(), pending_len.clone()]);
            let felt252_ty = self.felt252;
            let location = stmt.location;
            let new_var = |ty| Variable::new(db, ImplLookupContext::default(), ty, location);
            let as_usage = |var_id| VarUsage { var_id, location };
            let array_fn = |extern_id| {
                let args = vec![GenericArgumentId::Type(felt252_ty)];
                GenericFunctionId::Extern(extern_id).concretize(db, args).lowered(db)
            };
            let call_stmt = |function, inputs, outputs| {
                let with_coupon = false;
                Statement::Call(StatementCall { function, inputs, with_coupon, outputs, location })
            };
            let arr_var = new_var(corelib::core_array_felt252_ty(db));
            let mut arr = self.variables.alloc(arr_var.clone());
            self.additional_stmts.push(call_stmt(array_fn(self.array_new), vec![], vec![arr]));
            let felt252_var = new_var(felt252_ty);
            let arr_append_fn = array_fn(self.array_append);
            for word in panic_data {
                let to_append = self.variables.alloc(felt252_var.clone());
                let new_arr = self.variables.alloc(arr_var.clone());
                self.additional_stmts.push(Statement::Const(StatementConst {
                    value: ConstValue::Int(word, felt252_ty),
                    output: to_append,
                }));
                self.additional_stmts.push(call_stmt(
                    arr_append_fn,
                    vec![as_usage(arr), as_usage(to_append)],
                    vec![new_arr],
                ));
                arr = new_arr;
            }
            let panic_ty = corelib::get_core_ty_by_name(db, "Panic".into(), vec![]);
            let panic_var = self.variables.alloc(new_var(panic_ty));
            self.additional_stmts.push(Statement::StructConstruct(StatementStructConstruct {
                inputs: vec![],
                output: panic_var,
            }));
            return Some(Statement::StructConstruct(StatementStructConstruct {
                inputs: vec![as_usage(panic_var), as_usage(arr)],
                output: stmt.outputs[0],
            }));
        }
        let (id, _generic_args) = stmt.function.get_extern(db)?;
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
                let additional_stmt = self.propagate_zero_and_get_statement(stmt.outputs[1]);
                self.additional_stmts.push(additional_stmt);
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
            self.additional_stmts
                .push(Statement::Const(StatementConst { value: r_value, output: r_output }));
            Some(Statement::Const(StatementConst { value: q_value, output: q_output }))
        } else if id == self.storage_base_address_from_felt252 {
            let input_var = stmt.inputs[0].var_id;
            if let Some(ConstValue::Int(val, ty)) = self.as_const(input_var) {
                stmt.inputs.clear();
                let arg = GenericArgumentId::Constant(ConstValue::Int(val.clone(), *ty).intern(db));
                stmt.function =
                    self.storage_base_address_const.concretize(db, vec![arg]).lowered(db);
            }
            None
        } else if id == self.into_box {
            let input = stmt.inputs[0];
            let var_info = self.var_info.get(&input.var_id);
            let const_value = match var_info {
                Some(VarInfo::Const(val)) => Some(val.clone()),
                Some(VarInfo::Snapshot(info)) => {
                    try_extract_matches!(info.as_ref(), VarInfo::Const).cloned()
                }
                _ => None,
            };
            let var_info = var_info.cloned().or_else(|| var_info_if_copy(self.variables, input))?;
            self.var_info.insert(stmt.outputs[0], VarInfo::Box(var_info.into()));
            Some(Statement::Const(StatementConst {
                value: ConstValue::Boxed(const_value?.into()),
                output: stmt.outputs[0],
            }))
        } else if id == self.unbox {
            if let VarInfo::Box(inner) = self.var_info.get(&stmt.inputs[0].var_id)? {
                let inner = inner.as_ref().clone();
                if let VarInfo::Const(inner) =
                    self.var_info.entry(stmt.outputs[0]).insert_entry(inner).get()
                {
                    return Some(Statement::Const(StatementConst {
                        value: inner.clone(),
                        output: stmt.outputs[0],
                    }));
                }
            }
            None
        } else if self.upcast_fns.contains(&id) {
            let int_value = self.as_int(stmt.inputs[0].var_id)?;
            let output = stmt.outputs[0];
            let value = ConstValue::Int(int_value.clone(), self.variables[output].ty);
            self.var_info.insert(output, VarInfo::Const(value.clone()));
            Some(Statement::Const(StatementConst { value, output }))
        } else if id == self.array_new {
            self.var_info.insert(stmt.outputs[0], VarInfo::Array(vec![]));
            None
        } else if id == self.array_append {
            let mut var_infos =
                if let VarInfo::Array(var_infos) = self.var_info.get(&stmt.inputs[0].var_id)? {
                    var_infos.clone()
                } else {
                    return None;
                };
            let appended = stmt.inputs[1];
            var_infos.push(match self.var_info.get(&appended.var_id) {
                Some(var_info) => Some(var_info.clone()),
                None => var_info_if_copy(self.variables, appended),
            });
            self.var_info.insert(stmt.outputs[0], VarInfo::Array(var_infos));
            None
        } else if id == self.array_len {
            let info = self.var_info.get(&stmt.inputs[0].var_id)?;
            let desnapped = try_extract_matches!(info, VarInfo::Snapshot)?;
            let length = try_extract_matches!(desnapped.as_ref(), VarInfo::Array)?.len();
            Some(self.propagate_const_and_get_statement(length.into(), stmt.outputs[0], false))
        } else {
            None
        }
    }

    /// Tries to specialize the call.
    /// Returns The specialized call statement if it was specialized, or None otherwise.
    ///
    /// Specialization occurs only if `priv_should_specialize` returns true.
    /// Additionally specialization of a callee the with the same base as the caller is currently
    /// not supported.
    fn try_specialize_call(&mut self, call_stmt: &mut StatementCall) -> Option<Statement> {
        if call_stmt.with_coupon {
            return None;
        }

        let Ok(Some(func_with_body)) = call_stmt.function.body(self.db) else {
            return None;
        };

        if call_stmt
            .inputs
            .iter()
            .all(|arg| !matches!(self.var_info.get(&arg.var_id), Some(VarInfo::Const(_))))
        {
            // No const inputs
            return None;
        }

        let mut const_arg = vec![];
        let mut new_args = vec![];
        for arg in &call_stmt.inputs {
            if let Some(var_info) = self.var_info.get(&arg.var_id) {
                if let Some(c) =
                    self.try_get_specialization_arg(var_info.clone(), self.variables[arg.var_id].ty)
                {
                    const_arg.push(Some(c.clone()));
                    continue;
                }
            }
            const_arg.push(None);
            new_args.push(*arg);
        }

        if new_args.len() == call_stmt.inputs.len() {
            // No argument was assigned -> no specialization.
            return None;
        }

        let (base, args) = match func_with_body.lookup_intern(self.db) {
            ConcreteFunctionWithBodyLongId::Semantic(_)
            | ConcreteFunctionWithBodyLongId::Generated(_) => {
                (func_with_body, const_arg.into_iter().collect())
            }
            ConcreteFunctionWithBodyLongId::Specialized(specialized_function) => {
                // Canonicalize the specialization rather than adding a specialization of a
                // specializaed function.
                let mut new_args_iter = chain!(const_arg.into_iter(), std::iter::once(None));
                let args = specialized_function
                    .args
                    .iter()
                    .map(|arg| {
                        if arg.is_none() {
                            return new_args_iter.next().unwrap();
                        }
                        arg.clone()
                    })
                    .collect();

                (specialized_function.base, args)
            }
        };

        // Avoid specializing with the same base as the current function as it may lead to infinite
        // specialization.
        if base == self.caller_base {
            return None;
        }

        let specialized_func_id =
            ConcreteFunctionWithBodyLongId::Specialized(SpecializedFunction { base, args })
                .intern(self.db);

        if self.db.priv_should_specialize(specialized_func_id) == Ok(false) {
            return None;
        }

        Some(Statement::Call(StatementCall {
            function: specialized_func_id.function_id(self.db).unwrap(),
            inputs: new_args,
            with_coupon: call_stmt.with_coupon,
            outputs: std::mem::take(&mut call_stmt.outputs),
            location: call_stmt.location,
        }))
    }

    /// Adds `value` as a const to `var_info` and return a const statement for it.
    fn propagate_const_and_get_statement(
        &mut self,
        value: BigInt,
        output: VariableId,
        nz_ty: bool,
    ) -> Statement {
        let ty = self.variables[output].ty;
        let value = if nz_ty {
            let inner_ty =
                try_extract_nz_wrapped_type(self.db, ty).expect("Expected a non-zero type");
            ConstValue::NonZero(Box::new(ConstValue::Int(value, inner_ty)))
        } else {
            ConstValue::Int(value, ty)
        };
        self.var_info.insert(output, VarInfo::Const(value.clone()));
        Statement::Const(StatementConst { value, output })
    }

    /// Adds 0 const to `var_info` and return a const statement for it.
    fn propagate_zero_and_get_statement(&mut self, output: VariableId) -> Statement {
        self.propagate_const_and_get_statement(BigInt::zero(), output, false)
    }

    /// Returns a statement that introduces the requested value into `output`, or None if fails.
    fn try_generate_const_statement(
        &self,
        value: &ConstValue,
        output: VariableId,
    ) -> Option<Statement> {
        if self.db.type_size_info(self.variables[output].ty) == Ok(TypeSizeInformation::Other) {
            Some(Statement::Const(StatementConst { value: value.clone(), output }))
        } else if matches!(value, ConstValue::Struct(members, _) if members.is_empty()) {
            // Handling const empty structs - which are not supported in sierra-gen.
            Some(Statement::StructConstruct(StatementStructConstruct { inputs: vec![], output }))
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
    ) -> Option<(Vec<Statement>, BlockEnd)> {
        let db = self.db;
        let (id, generic_args) = info.function.get_extern(db)?;
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
                (vec![], BlockEnd::Goto(info.arms[0].block_id, Default::default()))
            } else {
                let arm = &info.arms[1];
                let nz_var = arm.var_ids[0];
                let nz_val = ConstValue::NonZero(Box::new(val.clone()));
                self.var_info.insert(nz_var, VarInfo::Const(nz_val.clone()));
                (
                    vec![Statement::Const(StatementConst { value: nz_val, output: nz_var })],
                    BlockEnd::Goto(arm.block_id, Default::default()),
                )
            })
        } else if self.eq_fns.contains(&id) {
            let lhs = self.as_int(info.inputs[0].var_id);
            let rhs = self.as_int(info.inputs[1].var_id);
            if (lhs.map(Zero::is_zero).unwrap_or_default() && rhs.is_none())
                || (rhs.map(Zero::is_zero).unwrap_or_default() && lhs.is_none())
            {
                let nz_input = info.inputs[if lhs.is_some() { 1 } else { 0 }];
                let var = &self.variables[nz_input.var_id].clone();
                let function = self.type_value_ranges.get(&var.ty)?.is_zero;
                let unused_nz_var = Variable::new(
                    db,
                    ImplLookupContext::default(),
                    corelib::core_nonzero_ty(db, var.ty),
                    var.location,
                );
                let unused_nz_var = self.variables.alloc(unused_nz_var);
                return Some((
                    vec![],
                    BlockEnd::Match {
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
                vec![],
                BlockEnd::Goto(
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
            let lhs = self.as_int(info.inputs[0].var_id);
            if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
                let ty = self.variables[info.arms[0].var_ids[0]].ty;
                let range = &self.type_value_ranges.get(&ty)?.range;
                let value = if self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id) {
                    lhs + rhs
                } else {
                    lhs - rhs
                };
                let (arm_index, value) = match range.normalized(value) {
                    NormalizedResult::InRange(value) => (0, value),
                    NormalizedResult::Under(value) => (1, value),
                    NormalizedResult::Over(value) => (
                        if self.iadd_fns.contains(&id) || self.isub_fns.contains(&id) {
                            2
                        } else {
                            1
                        },
                        value,
                    ),
                };
                let arm = &info.arms[arm_index];
                let actual_output = arm.var_ids[0];
                let value = ConstValue::Int(value, ty);
                self.var_info.insert(actual_output, VarInfo::Const(value.clone()));
                return Some((
                    vec![Statement::Const(StatementConst { value, output: actual_output })],
                    BlockEnd::Goto(arm.block_id, Default::default()),
                ));
            }
            if let Some(rhs) = rhs {
                if rhs.is_zero() && !self.diff_fns.contains(&id) {
                    let arm = &info.arms[0];
                    self.var_info.insert(arm.var_ids[0], VarInfo::Var(info.inputs[0]));
                    return Some((vec![], BlockEnd::Goto(arm.block_id, Default::default())));
                }
                if rhs.is_one() && !self.diff_fns.contains(&id) {
                    let ty = self.variables[info.arms[0].var_ids[0]].ty;
                    let ty_info = self.type_value_ranges.get(&ty)?;
                    let function = if self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id) {
                        ty_info.inc?
                    } else {
                        ty_info.dec?
                    };
                    let enum_ty = function.signature(db).ok()?.return_type;
                    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) =
                        enum_ty.lookup_intern(db)
                    else {
                        return None;
                    };
                    let result = self.variables.alloc(Variable::new(
                        db,
                        ImplLookupContext::default(),
                        function.signature(db).unwrap().return_type,
                        info.location,
                    ));
                    return Some((
                        vec![Statement::Call(StatementCall {
                            function,
                            inputs: vec![info.inputs[0]],
                            with_coupon: false,
                            outputs: vec![result],
                            location: info.location,
                        })],
                        BlockEnd::Match {
                            info: MatchInfo::Enum(MatchEnumInfo {
                                concrete_enum_id,
                                input: VarUsage { var_id: result, location: info.location },
                                arms: core::mem::take(&mut info.arms),
                                location: info.location,
                            }),
                        },
                    ));
                }
            }
            if let Some(lhs) = lhs {
                if lhs.is_zero() && (self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id)) {
                    let arm = &info.arms[0];
                    self.var_info.insert(arm.var_ids[0], VarInfo::Var(info.inputs[1]));
                    return Some((vec![], BlockEnd::Goto(arm.block_id, Default::default())));
                }
            }
            None
        } else if let Some(reversed) = self.downcast_fns.get(&id) {
            let range = |ty: TypeId| {
                Some(if let Some(ti) = self.type_value_ranges.get(&ty) {
                    ti.range.clone()
                } else {
                    let (min, max) = corelib::try_extract_bounded_int_type_ranges(db, ty)?;
                    TypeRange { min, max }
                })
            };
            let (success_arm, failure_arm) = if *reversed { (1, 0) } else { (0, 1) };
            let input_var = info.inputs[0].var_id;
            let success_output = info.arms[success_arm].var_ids[0];
            let out_ty = self.variables[success_output].ty;
            let out_range = range(out_ty)?;
            let Some(value) = self.as_int(input_var) else {
                let in_ty = self.variables[input_var].ty;
                let in_range = range(in_ty)?;
                return if in_range.min < out_range.min || in_range.max > out_range.max {
                    None
                } else {
                    let generic_args = [in_ty, out_ty].map(GenericArgumentId::Type).to_vec();
                    let function = db.core_info().upcast_fn.concretize(db, generic_args);
                    return Some((
                        vec![Statement::Call(StatementCall {
                            function: function.lowered(db),
                            inputs: vec![info.inputs[0]],
                            with_coupon: false,
                            outputs: vec![success_output],
                            location: info.location,
                        })],
                        BlockEnd::Goto(info.arms[success_arm].block_id, Default::default()),
                    ));
                };
            };
            Some(if let NormalizedResult::InRange(value) = out_range.normalized(value.clone()) {
                let value = ConstValue::Int(value, out_ty);
                self.var_info.insert(success_output, VarInfo::Const(value.clone()));
                (
                    vec![Statement::Const(StatementConst { value, output: success_output })],
                    BlockEnd::Goto(info.arms[success_arm].block_id, Default::default()),
                )
            } else {
                (vec![], BlockEnd::Goto(info.arms[failure_arm].block_id, Default::default()))
            })
        } else if id == self.bounded_int_constrain {
            let input_var = info.inputs[0].var_id;
            let (value, nz_ty) = self.as_int_ex(input_var)?;
            let generic_arg = generic_args[1];
            let constrain_value = extract_matches!(generic_arg, GenericArgumentId::Constant)
                .lookup_intern(db)
                .into_int()
                .unwrap();
            let arm_idx = if value < &constrain_value { 0 } else { 1 };
            let output = info.arms[arm_idx].var_ids[0];
            Some((
                vec![self.propagate_const_and_get_statement(value.clone(), output, nz_ty)],
                BlockEnd::Goto(info.arms[arm_idx].block_id, Default::default()),
            ))
        } else if id == self.array_get {
            let index = self.as_int(info.inputs[1].var_id)?.to_usize()?;
            if let Some(VarInfo::Snapshot(arr_info)) = self.var_info.get(&info.inputs[0].var_id) {
                if let VarInfo::Array(infos) = arr_info.as_ref() {
                    if let Some(Some(output_var_info)) = infos.get(index) {
                        let arm = &info.arms[0];
                        let output_var_info = output_var_info.clone();
                        let box_info =
                            VarInfo::Box(VarInfo::Snapshot(output_var_info.clone().into()).into());
                        self.var_info.insert(arm.var_ids[0], box_info);
                        if let VarInfo::Const(value) = output_var_info {
                            let value_ty = value.ty(db).ok()?;
                            let value_box_ty = corelib::core_box_ty(db, value_ty);
                            let location = info.location;
                            let boxed_var = Variable::new(
                                db,
                                ImplLookupContext::default(),
                                value_box_ty,
                                location,
                            );
                            let boxed = self.variables.alloc(boxed_var.clone());
                            let unused_boxed = self.variables.alloc(boxed_var);
                            let snapped = self.variables.alloc(Variable::new(
                                db,
                                ImplLookupContext::default(),
                                TypeLongId::Snapshot(value_box_ty).intern(db),
                                location,
                            ));
                            return Some((
                                vec![
                                    Statement::Const(StatementConst {
                                        value: ConstValue::Boxed(value.into()),
                                        output: boxed,
                                    }),
                                    Statement::Snapshot(StatementSnapshot {
                                        input: VarUsage { var_id: boxed, location },
                                        outputs: [unused_boxed, snapped],
                                    }),
                                    Statement::Call(StatementCall {
                                        function: self
                                            .box_forward_snapshot
                                            .concretize(db, vec![GenericArgumentId::Type(value_ty)])
                                            .lowered(db),
                                        inputs: vec![VarUsage { var_id: snapped, location }],
                                        with_coupon: false,
                                        outputs: vec![arm.var_ids[0]],
                                        location: info.location,
                                    }),
                                ],
                                BlockEnd::Goto(arm.block_id, Default::default()),
                            ));
                        }
                    } else {
                        return Some((
                            vec![],
                            BlockEnd::Goto(info.arms[1].block_id, Default::default()),
                        ));
                    }
                }
            }
            if index.is_zero() {
                if let [success, failure] = info.arms.as_mut_slice() {
                    let arr = info.inputs[0].var_id;
                    let unused_arr_output0 = self.variables.alloc(self.variables[arr].clone());
                    let unused_arr_output1 = self.variables.alloc(self.variables[arr].clone());
                    info.inputs.truncate(1);
                    info.function = GenericFunctionId::Extern(self.array_snapshot_pop_front)
                        .concretize(db, generic_args)
                        .lowered(db);
                    success.var_ids.insert(0, unused_arr_output0);
                    failure.var_ids.insert(0, unused_arr_output1);
                }
            }
            None
        } else if id == self.array_pop_front {
            let VarInfo::Array(var_infos) = self.var_info.get(&info.inputs[0].var_id)? else {
                return None;
            };
            if let Some(first) = var_infos.first() {
                if let Some(first) = first.as_ref().cloned() {
                    let arm = &info.arms[0];
                    self.var_info.insert(arm.var_ids[0], VarInfo::Array(var_infos[1..].to_vec()));
                    self.var_info.insert(arm.var_ids[1], VarInfo::Box(first.into()));
                }
                None
            } else {
                let arm = &info.arms[1];
                self.var_info.insert(arm.var_ids[0], VarInfo::Array(vec![]));
                Some((vec![], BlockEnd::Goto(arm.block_id, Default::default())))
            }
        } else if id == self.array_snapshot_pop_back || id == self.array_snapshot_pop_front {
            let var_info = self.var_info.get(&info.inputs[0].var_id)?;
            let desnapped = try_extract_matches!(var_info, VarInfo::Snapshot)?;
            let element_var_infos = try_extract_matches!(desnapped.as_ref(), VarInfo::Array)?;
            // TODO(orizi): Propagate success values as well.
            if element_var_infos.is_empty() {
                let arm = &info.arms[1];
                self.var_info.insert(arm.var_ids[0], VarInfo::Array(vec![]));
                Some((vec![], BlockEnd::Goto(arm.block_id, Default::default())))
            } else {
                None
            }
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

    /// Given a var_info and its type, return the corresponding specialization argument, if it
    /// exists.
    fn try_get_specialization_arg(
        &mut self,
        var_info: VarInfo,
        ty: TypeId,
    ) -> Option<SpecializationArg> {
        if self.db.type_size_info(ty).ok()? == TypeSizeInformation::ZeroSized {
            // Skip zero-sized constants as they are not supported in sierra-gen.
            return None;
        }

        match var_info {
            VarInfo::Const(c) => Some(SpecializationArg::Const(c.clone())),
            VarInfo::Array(infos) if infos.is_empty() => {
                let TypeLongId::Concrete(concrete_ty) = ty.lookup_intern(self.db) else {
                    unreachable!("Expected a concrete type");
                };
                let [GenericArgumentId::Type(inner_ty)] = &concrete_ty.generic_args(self.db)[..]
                else {
                    unreachable!("Expected a single type generic argument");
                };
                Some(SpecializationArg::EmptyArray(*inner_ty))
            }
            VarInfo::Struct(infos) => {
                let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct)) =
                    ty.lookup_intern(self.db)
                else {
                    // TODO(ilya): Support closures and fixed size arrays.
                    return None;
                };

                let members = self.db.concrete_struct_members(concrete_struct).unwrap();
                let struct_args = zip_eq(members.values(), infos)
                    .map(|(member, opt_var_info)| {
                        self.try_get_specialization_arg(opt_var_info?.clone(), member.ty)
                    })
                    .collect::<Option<Vec<_>>>()?;

                Some(SpecializationArg::Struct(struct_args))
            }
            _ => None,
        }
    }

    /// Returns true if const-folding should be skipped for the current function.
    pub fn should_skip_const_folding(&self, db: &dyn LoweringGroup) -> bool {
        if db.optimization_config().skip_const_folding {
            return true;
        }

        // Skipping const-folding for `panic_with_const_felt252` - to avoid replacing a call to
        // `panic_with_felt252` with `panic_with_const_felt252` and causing accidental recursion.
        if self.caller_base.base_semantic_function(db).generic_function(db)
            == GenericFunctionWithBodyId::Free(self.libfunc_info.panic_with_const_felt252)
        {
            return true;
        }
        false
    }
}

/// Returns a `VarInfo` of a variable only if it is copyable.
fn var_info_if_copy(variables: &Arena<Variable>, input: VarUsage) -> Option<VarInfo> {
    variables[input.var_id].copyable.is_ok().then_some(VarInfo::Var(input))
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
    /// The `unbox` libfunc.
    unbox: ExternFunctionId,
    /// The `box_forward_snapshot` libfunc.
    box_forward_snapshot: GenericFunctionId,
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
    /// The `array_get` libfunc.
    array_get: ExternFunctionId,
    /// The `array_snapshot_pop_front` libfunc.
    array_snapshot_pop_front: ExternFunctionId,
    /// The `array_snapshot_pop_back` libfunc.
    array_snapshot_pop_back: ExternFunctionId,
    /// The `array_len` libfunc.
    array_len: ExternFunctionId,
    /// The `array_new` libfunc.
    array_new: ExternFunctionId,
    /// The `array_append` libfunc.
    array_append: ExternFunctionId,
    /// The `array_pop_front` libfunc.
    array_pop_front: ExternFunctionId,
    /// The `storage_base_address_from_felt252` libfunc.
    storage_base_address_from_felt252: ExternFunctionId,
    /// The `storage_base_address_const` libfunc.
    storage_base_address_const: GenericFunctionId,
    /// The `core::panic_with_felt252` function.
    panic_with_felt252: FunctionId,
    /// The `core::panic_with_const_felt252` function.
    pub panic_with_const_felt252: FreeFunctionId,
    /// The `core::panics::panic_with_byte_array` function.
    panic_with_byte_array: FunctionId,
    /// Type ranges.
    type_value_ranges: OrderedHashMap<TypeId, TypeInfo>,
    /// The info used for semantic const calculation.
    const_calculation_info: Arc<ConstCalcInfo>,
}
impl ConstFoldingLibfuncInfo {
    fn new(db: &dyn LoweringGroup) -> Self {
        let core = ModuleHelper::core(db);
        let box_module = core.submodule("box");
        let integer_module = core.submodule("integer");
        let internal_module = core.submodule("internal");
        let bounded_int_module = internal_module.submodule("bounded_int");
        let num_module = internal_module.submodule("num");
        let array_module = core.submodule("array");
        let starknet_module = core.submodule("starknet");
        let storage_access_module = starknet_module.submodule("storage_access");
        let nz_fns = OrderedHashSet::<_>::from_iter(chain!(
            [
                core.extern_function_id("felt252_is_zero"),
                bounded_int_module.extern_function_id("bounded_int_is_zero")
            ],
            ["u8", "u16", "u32", "u64", "u128", "u256"]
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
        let type_value_ranges = OrderedHashMap::from_iter(
            [
                ("u8", BigInt::ZERO, u8::MAX.into(), false, true),
                ("u16", BigInt::ZERO, u16::MAX.into(), false, true),
                ("u32", BigInt::ZERO, u32::MAX.into(), false, true),
                ("u64", BigInt::ZERO, u64::MAX.into(), false, true),
                ("u128", BigInt::ZERO, u128::MAX.into(), false, true),
                ("u256", BigInt::ZERO, BigInt::from(1) << 256, false, false),
                ("i8", i8::MIN.into(), i8::MAX.into(), true, true),
                ("i16", i16::MIN.into(), i16::MAX.into(), true, true),
                ("i32", i32::MIN.into(), i32::MAX.into(), true, true),
                ("i64", i64::MIN.into(), i64::MAX.into(), true, true),
                ("i128", i128::MIN.into(), i128::MAX.into(), true, true),
            ]
            .map(
                |(ty_name, min, max, as_bounded_int, inc_dec): (
                    &str,
                    BigInt,
                    BigInt,
                    bool,
                    bool,
                )| {
                    let ty = corelib::get_core_ty_by_name(db, ty_name.into(), vec![]);
                    let is_zero = if as_bounded_int {
                        bounded_int_module
                            .function_id("bounded_int_is_zero", vec![GenericArgumentId::Type(ty)])
                    } else {
                        integer_module.function_id(format!("{ty_name}_is_zero"), vec![])
                    }
                    .lowered(db);
                    let (inc, dec) = if inc_dec {
                        (
                            Some(
                                num_module
                                    .function_id(format!("{ty_name}_inc"), vec![])
                                    .lowered(db),
                            ),
                            Some(
                                num_module
                                    .function_id(format!("{ty_name}_dec"), vec![])
                                    .lowered(db),
                            ),
                        )
                    } else {
                        (None, None)
                    };
                    let info = TypeInfo { range: TypeRange { min, max }, is_zero, inc, dec };
                    (ty, info)
                },
            ),
        );
        Self {
            felt_sub: core.extern_function_id("felt252_sub"),
            into_box: box_module.extern_function_id("into_box"),
            unbox: box_module.extern_function_id("unbox"),
            box_forward_snapshot: box_module.generic_function_id("box_forward_snapshot"),
            nz_fns,
            eq_fns,
            uadd_fns,
            usub_fns,
            diff_fns,
            iadd_fns,
            isub_fns,
            wide_mul_fns,
            div_rem_fns,
            bounded_int_add: bounded_int_module.extern_function_id("bounded_int_add"),
            bounded_int_sub: bounded_int_module.extern_function_id("bounded_int_sub"),
            bounded_int_constrain: bounded_int_module.extern_function_id("bounded_int_constrain"),
            array_get: array_module.extern_function_id("array_get"),
            array_snapshot_pop_front: array_module.extern_function_id("array_snapshot_pop_front"),
            array_snapshot_pop_back: array_module.extern_function_id("array_snapshot_pop_back"),
            array_len: array_module.extern_function_id("array_len"),
            array_new: array_module.extern_function_id("array_new"),
            array_append: array_module.extern_function_id("array_append"),
            array_pop_front: array_module.extern_function_id("array_pop_front"),
            storage_base_address_from_felt252: storage_access_module
                .extern_function_id("storage_base_address_from_felt252"),
            storage_base_address_const: storage_access_module
                .generic_function_id("storage_base_address_const"),
            panic_with_felt252: core.function_id("panic_with_felt252", vec![]).lowered(db),
            panic_with_const_felt252: core.free_function_id("panic_with_const_felt252"),
            panic_with_byte_array: core
                .submodule("panics")
                .function_id("panic_with_byte_array", vec![])
                .lowered(db),
            type_value_ranges,
            const_calculation_info: db.const_calc_info(),
        }
    }
}

impl std::ops::Deref for ConstFoldingContext<'_> {
    type Target = ConstFoldingLibfuncInfo;
    fn deref(&self) -> &ConstFoldingLibfuncInfo {
        &self.libfunc_info
    }
}

impl std::ops::Deref for ConstFoldingLibfuncInfo {
    type Target = ConstCalcInfo;
    fn deref(&self) -> &ConstCalcInfo {
        &self.const_calculation_info
    }
}

/// The information of a type required for const foldings.
#[derive(Debug, PartialEq, Eq)]
struct TypeInfo {
    /// The value range of the type.
    range: TypeRange,
    /// The function to check if the value is zero for the type.
    is_zero: FunctionId,
    /// Inc function to increase the value by one.
    inc: Option<FunctionId>,
    /// Dec function to decrease the value by one.
    dec: Option<FunctionId>,
}

/// The range of values of a numeric type.
#[derive(Debug, PartialEq, Eq, Clone)]
struct TypeRange {
    /// The minimum value of the type.
    min: BigInt,
    /// The maximum value of the type.
    max: BigInt,
}
impl TypeRange {
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
