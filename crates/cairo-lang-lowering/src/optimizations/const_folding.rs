#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_defs::ids::{ExternFunctionId, FreeFunctionId};
use cairo_lang_filesystem::flag::flag_unsafe_panic;
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_semantic::corelib::CorelibSemantic;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::constant::{
    ConstCalcInfo, ConstValue, ConstValueId, ConstantSemantic, TypeRange, canonical_felt252,
    felt252_for_downcast,
};
use cairo_lang_semantic::items::functions::{GenericFunctionId, GenericFunctionWithBodyId};
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::types::{TypeSizeInformation, TypesSemantic};
use cairo_lang_semantic::{
    ConcreteTypeId, ConcreteVariant, GenericArgumentId, MatchArmSelector, TypeId, TypeLongId,
    corelib,
};
use cairo_lang_utils::byte_array::BYTE_ARRAY_MAGIC;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, extract_matches, try_extract_matches};
use itertools::{chain, zip_eq};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::cast::ToPrimitive;
use num_traits::{Num, One, Zero};
use salsa::Database;
use starknet_types_core::felt::Felt as Felt252;

use crate::db::LoweringGroup;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, FunctionId, SemanticFunctionIdEx,
    SpecializedFunction,
};
use crate::specialization::SpecializationArg;
use crate::utils::InliningStrategy;
use crate::{
    Block, BlockEnd, BlockId, DependencyType, Lowered, LoweringStage, MatchArm, MatchEnumInfo,
    MatchExternInfo, MatchInfo, Statement, StatementCall, StatementConst, StatementDesnap,
    StatementEnumConstruct, StatementSnapshot, StatementStructConstruct,
    StatementStructDestructure, VarRemapping, VarUsage, Variable, VariableArena, VariableId,
};

/// Keeps track of equivalent values that variables might be replaced with.
/// Note: We don't keep track of types as we assume the usage is always correct.
#[derive(Debug, Clone)]
enum VarInfo<'db> {
    /// The variable is a const value.
    Const(ConstValueId<'db>),
    /// The variable can be replaced by another variable.
    Var(VarUsage<'db>),
    /// The variable is a snapshot of another variable.
    Snapshot(Box<VarInfo<'db>>),
    /// The variable is a struct of other variables.
    /// `None` values represent variables that are not tracked.
    Struct(Vec<Option<VarInfo<'db>>>),
    /// The variable is an enum of a known variant of other variables.
    Enum { variant: ConcreteVariant<'db>, payload: Box<VarInfo<'db>> },
    /// The variable is a box of another variable.
    Box(Box<VarInfo<'db>>),
    /// The variable is an array of known size of other variables.
    /// `None` values represent variables that are not tracked.
    Array(Vec<Option<VarInfo<'db>>>),
}
impl<'db> VarInfo<'db> {
    /// Peels the snapshots from the variable info and returns the number of snapshots performed.
    fn peel_snapshots(&self) -> (usize, &VarInfo<'db>) {
        let mut n_snapshots = 0;
        let mut info = self;
        while let VarInfo::Snapshot(inner) = info {
            info = inner.as_ref();
            n_snapshots += 1;
        }
        (n_snapshots, info)
    }
    /// Wraps the variable info with the given number of snapshots.
    fn wrap_with_snapshots(mut self, n_snapshots: usize) -> VarInfo<'db> {
        for _ in 0..n_snapshots {
            self = VarInfo::Snapshot(Box::new(self));
        }
        self
    }
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
pub fn const_folding<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
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

pub struct ConstFoldingContext<'db, 'mt> {
    /// The used database.
    db: &'db dyn Database,
    /// The variables arena, mostly used to get the type of variables.
    pub variables: &'mt mut VariableArena<'db>,
    /// The accumulated information about the const values of variables.
    var_info: UnorderedHashMap<VariableId, VarInfo<'db>>,
    /// The libfunc information.
    libfunc_info: &'db ConstFoldingLibfuncInfo<'db>,
    /// The specialization base of the caller function (or the caller if the function is not
    /// specialized).
    caller_base: ConcreteFunctionWithBodyId<'db>,
    /// Reachability of blocks from the function start.
    /// If the block is not in this map, it means that it is unreachable (or that it was already
    /// visited and its reachability won't be checked again).
    reachability: UnorderedHashMap<BlockId, Reachability>,
    /// Additional statements to add to the block.
    additional_stmts: Vec<Statement<'db>>,
}

impl<'db, 'mt> ConstFoldingContext<'db, 'mt> {
    pub fn new(
        db: &'db dyn Database,
        function_id: ConcreteFunctionWithBodyId<'db>,
        variables: &'mt mut VariableArena<'db>,
    ) -> Self {
        let caller_base = match function_id.long(db) {
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
    pub fn visit_block_start<'r, 'get>(
        &'r mut self,
        block_id: BlockId,
        get_block: impl FnOnce(BlockId) -> &'get Block<'db>,
    ) -> bool
    where
        'db: 'get,
    {
        let Some(reachability) = self.reachability.remove(&block_id) else {
            return false;
        };
        match reachability {
            Reachability::Any => {}
            Reachability::FromSingleGoto(from_block) => match &get_block(from_block).end {
                BlockEnd::Goto(_, remapping) => {
                    for (dst, src) in remapping.iter() {
                        if let Some(v) = self.as_const(src.var_id) {
                            self.var_info.insert(*dst, VarInfo::Const(v));
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
    pub fn visit_statement(&mut self, stmt: &mut Statement<'db>) {
        self.maybe_replace_inputs(stmt.inputs_mut());
        match stmt {
            Statement::Const(StatementConst { value, output, boxed }) if *boxed => {
                self.var_info.insert(*output, VarInfo::Box(VarInfo::Const(*value).into()));
            }
            Statement::Const(StatementConst { value, output, .. }) => match value.long(self.db) {
                ConstValue::Int(..)
                | ConstValue::Struct(..)
                | ConstValue::Enum(..)
                | ConstValue::NonZero(..) => {
                    self.var_info.insert(*output, VarInfo::Const(*value));
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
                        const_args.push(*value);
                    }
                    all_args.push(Some(info.clone()));
                }
                if const_args.len() == inputs.len() {
                    let value =
                        ConstValue::Struct(const_args, self.variables[*output].ty).intern(self.db);
                    self.var_info.insert(*output, VarInfo::Const(value));
                } else if contains_info {
                    self.var_info.insert(*output, VarInfo::Struct(all_args));
                }
            }
            Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                if let Some(info) = self.var_info.get(&input.var_id) {
                    let (n_snapshots, info) = info.peel_snapshots();
                    match info {
                        VarInfo::Const(const_value) => {
                            if let ConstValue::Struct(member_values, _) = const_value.long(self.db)
                            {
                                for (output, value) in zip_eq(outputs, member_values) {
                                    self.var_info.insert(
                                        *output,
                                        VarInfo::Const(*value).wrap_with_snapshots(n_snapshots),
                                    );
                                }
                            }
                        }
                        VarInfo::Struct(members) => {
                            for (output, member) in zip_eq(outputs, members.clone()) {
                                if let Some(member) = member {
                                    self.var_info
                                        .insert(*output, member.wrap_with_snapshots(n_snapshots));
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                let value = if let Some(info) = self.var_info.get(&input.var_id) {
                    if let VarInfo::Const(val) = info {
                        VarInfo::Const(ConstValue::Enum(*variant, *val).intern(self.db))
                    } else {
                        VarInfo::Enum { variant: *variant, payload: info.clone().into() }
                    }
                } else {
                    VarInfo::Enum { variant: *variant, payload: VarInfo::Var(*input).into() }
                };
                self.var_info.insert(*output, value);
            }
        }
    }

    /// Processes the block's end and incorporates additional statements into the block.
    ///
    /// This method handles the following tasks:
    /// - Inserts the accumulated additional statements into the block.
    /// - Converts match endings to goto when applicable.
    /// - Updates self.reachability based on the block's ending.
    pub fn visit_block_end(&mut self, block_id: BlockId, block: &mut Block<'db>) {
        let statements = &mut block.statements;
        statements.splice(0..0, self.additional_stmts.drain(..));

        match &mut block.end {
            BlockEnd::Goto(_, remappings) => {
                for (_, v) in remappings.iter_mut() {
                    self.maybe_replace_input(v);
                }
            }
            BlockEnd::Match { info } => {
                self.maybe_replace_inputs(info.inputs_mut());
                match info {
                    MatchInfo::Enum(info) => {
                        if let Some(updated_end) = self.handle_enum_block_end(info, statements) {
                            block.end = updated_end;
                        }
                    }
                    MatchInfo::Extern(info) => {
                        if let Some(updated_end) = self.handle_extern_block_end(info, statements) {
                            block.end = updated_end;
                        }
                    }
                    MatchInfo::Value(info) => {
                        if let Some(value) =
                            self.as_int(info.input.var_id).and_then(|x| x.to_usize())
                            && let Some(arm) = info.arms.iter().find(|arm| {
                                matches!(
                                    &arm.arm_selector,
                                    MatchArmSelector::Value(v) if v.value == value
                                )
                            })
                        {
                            // Create the variable that was previously introduced in match arm.
                            statements.push(Statement::StructConstruct(StatementStructConstruct {
                                inputs: vec![],
                                output: arm.var_ids[0],
                            }));
                            block.end = BlockEnd::Goto(arm.block_id, Default::default());
                        }
                    }
                }
            }
            BlockEnd::Return(inputs, _) => self.maybe_replace_inputs(inputs),
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
    fn handle_statement_call(&mut self, stmt: &mut StatementCall<'db>) -> Option<Statement<'db>> {
        let db = self.db;
        if stmt.function == self.panic_with_felt252 {
            let val = self.as_const(stmt.inputs[0].var_id)?;
            stmt.inputs.clear();
            stmt.function = GenericFunctionId::Free(self.panic_with_const_felt252)
                .concretize(db, vec![GenericArgumentId::Constant(val)])
                .lowered(db);
            return None;
        } else if stmt.function == self.panic_with_byte_array && !flag_unsafe_panic(db) {
            let snap = self.var_info.get(&stmt.inputs[0].var_id)?;
            let bytearray = try_extract_matches!(snap, VarInfo::Snapshot)?;
            let [
                Some(VarInfo::Array(data)),
                Some(VarInfo::Const(pending_word)),
                Some(VarInfo::Const(pending_len)),
            ] = &try_extract_matches!(bytearray.as_ref(), VarInfo::Struct)?[..]
            else {
                return None;
            };
            let mut panic_data =
                vec![BigInt::from_str_radix(BYTE_ARRAY_MAGIC, 16).unwrap(), data.len().into()];
            for word in data {
                let Some(VarInfo::Const(word)) = word else {
                    return None;
                };
                panic_data.push(word.long(db).to_int()?.clone());
            }
            panic_data.extend([
                pending_word.long(db).to_int()?.clone(),
                pending_len.long(db).to_int()?.clone(),
            ]);
            let felt252_ty = self.felt252;
            let location = stmt.location;
            let new_var = |ty| Variable::with_default_context(db, ty, location);
            let as_usage = |var_id| VarUsage { var_id, location };
            let array_fn = |extern_id| {
                let args = vec![GenericArgumentId::Type(felt252_ty)];
                GenericFunctionId::Extern(extern_id).concretize(db, args).lowered(db)
            };
            let call_stmt = |function, inputs, outputs| {
                let with_coupon = false;
                Statement::Call(StatementCall {
                    function,
                    inputs,
                    with_coupon,
                    outputs,
                    location,
                    is_specialization_base_call: false,
                })
            };
            let arr_var = new_var(corelib::core_array_felt252_ty(db));
            let mut arr = self.variables.alloc(arr_var.clone());
            self.additional_stmts.push(call_stmt(array_fn(self.array_new), vec![], vec![arr]));
            let felt252_var = new_var(felt252_ty);
            let arr_append_fn = array_fn(self.array_append);
            for word in panic_data {
                let to_append = self.variables.alloc(felt252_var.clone());
                let new_arr = self.variables.alloc(arr_var.clone());
                self.additional_stmts.push(Statement::Const(StatementConst::new_flat(
                    ConstValue::Int(word, felt252_ty).intern(db),
                    to_append,
                )));
                self.additional_stmts.push(call_stmt(
                    arr_append_fn,
                    vec![as_usage(arr), as_usage(to_append)],
                    vec![new_arr],
                ));
                arr = new_arr;
            }
            let panic_ty = corelib::get_core_ty_by_name(db, SmolStrId::from(db, "Panic"), vec![]);
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
            if let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
                && rhs.is_zero()
            {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
                None
            } else if let Some(lhs) = self.as_int(stmt.inputs[0].var_id)
                && let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
            {
                let value = canonical_felt252(&(lhs - rhs));
                Some(self.propagate_const_and_get_statement(value, stmt.outputs[0]))
            } else {
                None
            }
        } else if id == self.felt_add {
            if let Some(lhs) = self.as_int(stmt.inputs[0].var_id)
                && lhs.is_zero()
            {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[1]));
                None
            } else if let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
                && rhs.is_zero()
            {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
                None
            } else if let Some(lhs) = self.as_int(stmt.inputs[0].var_id)
                && let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
            {
                let value = canonical_felt252(&(lhs + rhs));
                Some(self.propagate_const_and_get_statement(value, stmt.outputs[0]))
            } else {
                None
            }
        } else if id == self.felt_mul {
            let lhs = self.as_int(stmt.inputs[0].var_id);
            let rhs = self.as_int(stmt.inputs[1].var_id);
            if lhs.map(Zero::is_zero).unwrap_or_default()
                || rhs.map(Zero::is_zero).unwrap_or_default()
            {
                Some(self.propagate_zero_and_get_statement(stmt.outputs[0]))
            } else if let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
                && rhs.is_one()
            {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
                None
            } else if let Some(lhs) = self.as_int(stmt.inputs[0].var_id)
                && lhs.is_one()
            {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[1]));
                None
            } else if let Some(lhs) = lhs
                && let Some(rhs) = rhs
            {
                let value = canonical_felt252(&(lhs * rhs));
                Some(self.propagate_const_and_get_statement(value, stmt.outputs[0]))
            } else {
                None
            }
        } else if id == self.felt_div {
            // Note that divisor is never 0, due to NonZero type always being the divisor.
            if let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
                // Returns the original value when dividing by 1.
                && rhs.is_one()
            {
                self.var_info.insert(stmt.outputs[0], VarInfo::Var(stmt.inputs[0]));
                None
            } else if let Some(lhs) = self.as_int(stmt.inputs[0].var_id)
                // If the value is 0, result is 0 regardless of the divisor.
                && lhs.is_zero()
            {
                Some(self.propagate_zero_and_get_statement(stmt.outputs[0]))
            } else if let Some(lhs) = self.as_int(stmt.inputs[0].var_id)
                && let Some(rhs) = self.as_int(stmt.inputs[1].var_id)
                && let Ok(rhs_nonzero) = Felt252::from(rhs).try_into()
            {
                // Constant fold when both operands are constants

                // Use field_div for Felt252 division
                let lhs_felt = Felt252::from(lhs);
                let value = lhs_felt.field_div(&rhs_nonzero).to_bigint();
                Some(self.propagate_const_and_get_statement(value, stmt.outputs[0]))
            } else {
                None
            }
        } else if self.wide_mul_fns.contains(&id) {
            let lhs = self.as_int(stmt.inputs[0].var_id);
            let rhs = self.as_int(stmt.inputs[1].var_id);
            let output = stmt.outputs[0];
            if lhs.map(Zero::is_zero).unwrap_or_default()
                || rhs.map(Zero::is_zero).unwrap_or_default()
            {
                return Some(self.propagate_zero_and_get_statement(output));
            }
            let lhs = lhs?;
            Some(self.propagate_const_and_get_statement(lhs * rhs?, stmt.outputs[0]))
        } else if id == self.bounded_int_add || id == self.bounded_int_sub {
            let lhs = self.as_int(stmt.inputs[0].var_id)?;
            let rhs = self.as_int(stmt.inputs[1].var_id)?;
            let value = if id == self.bounded_int_add { lhs + rhs } else { lhs - rhs };
            Some(self.propagate_const_and_get_statement(value, stmt.outputs[0]))
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
            let q_value = ConstValue::Int(q, self.variables[q_output].ty).intern(db);
            self.var_info.insert(q_output, VarInfo::Const(q_value));
            let r_output = stmt.outputs[1];
            let r_value = ConstValue::Int(r, self.variables[r_output].ty).intern(db);
            self.var_info.insert(r_output, VarInfo::Const(r_value));
            self.additional_stmts
                .push(Statement::Const(StatementConst::new_flat(r_value, r_output)));
            Some(Statement::Const(StatementConst::new_flat(q_value, q_output)))
        } else if id == self.storage_base_address_from_felt252 {
            let input_var = stmt.inputs[0].var_id;
            if let Some(const_value) = self.as_const(input_var)
                && let ConstValue::Int(val, ty) = const_value.long(db)
            {
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
                Some(VarInfo::Const(val)) => Some(*val),
                Some(VarInfo::Snapshot(info)) => {
                    try_extract_matches!(info.as_ref(), VarInfo::Const).copied()
                }
                _ => None,
            };
            let var_info = var_info.cloned().or_else(|| var_info_if_copy(self.variables, input))?;
            self.var_info.insert(stmt.outputs[0], VarInfo::Box(var_info.into()));
            Some(Statement::Const(StatementConst::new_boxed(const_value?, stmt.outputs[0])))
        } else if id == self.unbox {
            if let VarInfo::Box(inner) = self.var_info.get(&stmt.inputs[0].var_id)? {
                let inner = inner.as_ref().clone();
                if let VarInfo::Const(inner) =
                    self.var_info.entry(stmt.outputs[0]).insert_entry(inner).get()
                {
                    return Some(Statement::Const(StatementConst::new_flat(
                        *inner,
                        stmt.outputs[0],
                    )));
                }
            }
            None
        } else if self.upcast_fns.contains(&id) {
            let int_value = self.as_int(stmt.inputs[0].var_id)?;
            let output = stmt.outputs[0];
            let value = ConstValue::Int(int_value.clone(), self.variables[output].ty).intern(db);
            self.var_info.insert(output, VarInfo::Const(value));
            Some(Statement::Const(StatementConst::new_flat(value, output)))
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
            Some(self.propagate_const_and_get_statement(length.into(), stmt.outputs[0]))
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
    fn try_specialize_call(&self, call_stmt: &mut StatementCall<'db>) -> Option<Statement<'db>> {
        if call_stmt.with_coupon {
            return None;
        }
        // No specialization when avoiding inlining.
        if matches!(self.db.optimizations().inlining_strategy(), InliningStrategy::Avoid) {
            return None;
        }

        let Ok(Some(mut base)) = call_stmt.function.body(self.db) else {
            return None;
        };

        if self.db.priv_never_inline(base).ok()? {
            return None;
        }

        // Avoid specializing with a function that is in the same SCC as the caller.
        if base == self.caller_base {
            return None;
        }

        let scc = self.db.lowered_scc(base, DependencyType::Call, LoweringStage::Monomorphized);
        if scc.len() > 1 && scc.contains(&self.caller_base) {
            return None;
        }

        if call_stmt.inputs.iter().all(|arg| self.var_info.get(&arg.var_id).is_none()) {
            // No const inputs
            return None;
        }
        let mut specialization_args = vec![];
        let mut new_args = vec![];
        for arg in &call_stmt.inputs {
            if let Some(var_info) = self.var_info.get(&arg.var_id)
                && self.variables[arg.var_id].info.droppable.is_ok()
                && let Some(specialization_arg) = self.try_get_specialization_arg(
                    var_info.clone(),
                    self.variables[arg.var_id].ty,
                    &mut new_args,
                )
            {
                specialization_args.push(specialization_arg);
            } else {
                specialization_args.push(SpecializationArg::NotSpecialized);
                new_args.push(*arg);
                continue;
            };
        }

        if specialization_args.iter().all(|arg| matches!(arg, SpecializationArg::NotSpecialized)) {
            // No argument was assigned -> no specialization.
            return None;
        }
        if let ConcreteFunctionWithBodyLongId::Specialized(specialized_function) =
            base.long(self.db)
        {
            // Canonicalize the specialization rather than adding a specialization of a specialized
            // function.
            base = specialized_function.base;
            let mut new_args_iter = specialization_args.into_iter();
            let mut old_args = specialized_function.args.to_vec();
            let mut stack = vec![];
            for arg in old_args.iter_mut().rev() {
                stack.push(arg);
            }
            while let Some(arg) = stack.pop() {
                match arg {
                    SpecializationArg::Const { .. } => {}
                    SpecializationArg::Snapshot(inner) => {
                        stack.push(inner.as_mut());
                    }
                    SpecializationArg::Enum { payload, .. } => {
                        stack.push(payload.as_mut());
                    }
                    SpecializationArg::Array(_, values) => {
                        for value in values.iter_mut().rev() {
                            stack.push(value);
                        }
                    }
                    SpecializationArg::Struct(specialization_args) => {
                        for arg in specialization_args.iter_mut().rev() {
                            stack.push(arg);
                        }
                    }
                    SpecializationArg::NotSpecialized => {
                        *arg = new_args_iter.next().unwrap_or(SpecializationArg::NotSpecialized);
                    }
                }
            }
            specialization_args = old_args;
        }
        let specialized = SpecializedFunction { base, args: specialization_args.into() };
        let specialized_func_id =
            ConcreteFunctionWithBodyLongId::Specialized(specialized).intern(self.db);

        if self.db.priv_should_specialize(specialized_func_id) == Ok(false) {
            return None;
        }

        Some(Statement::Call(StatementCall {
            function: specialized_func_id.function_id(self.db).unwrap(),
            inputs: new_args,
            with_coupon: call_stmt.with_coupon,
            outputs: std::mem::take(&mut call_stmt.outputs),
            location: call_stmt.location,
            is_specialization_base_call: false,
        }))
    }

    /// Adds `value` as a const to `var_info` and return a const statement for it.
    fn propagate_const_and_get_statement(
        &mut self,
        value: BigInt,
        output: VariableId,
    ) -> Statement<'db> {
        let ty = self.variables[output].ty;
        let value = ConstValueId::from_int(self.db, ty, &value);
        self.var_info.insert(output, VarInfo::Const(value));
        Statement::Const(StatementConst::new_flat(value, output))
    }

    /// Adds 0 const to `var_info` and return a const statement for it.
    fn propagate_zero_and_get_statement(&mut self, output: VariableId) -> Statement<'db> {
        self.propagate_const_and_get_statement(BigInt::zero(), output)
    }

    /// Returns a statement that introduces the requested value into `output`, or None if fails.
    fn try_generate_const_statement(
        &self,
        value: ConstValueId<'db>,
        output: VariableId,
    ) -> Option<Statement<'db>> {
        if self.db.type_size_info(self.variables[output].ty) == Ok(TypeSizeInformation::Other) {
            Some(Statement::Const(StatementConst::new_flat(value, output)))
        } else if matches!(value.long(self.db), ConstValue::Struct(members, _) if members.is_empty())
        {
            // Handling const empty structs - which are not supported in sierra-gen.
            Some(Statement::StructConstruct(StatementStructConstruct { inputs: vec![], output }))
        } else {
            None
        }
    }

    /// Handles the end of block matching on an enum.
    /// Possibly extends the blocks statements as well.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns the updated block end.
    fn handle_enum_block_end(
        &mut self,
        info: &mut MatchEnumInfo<'db>,
        statements: &mut Vec<Statement<'db>>,
    ) -> Option<BlockEnd<'db>> {
        let input = info.input.var_id;
        let (n_snapshots, var_info) = self.var_info.get(&input)?.peel_snapshots();
        let location = info.location;
        let as_usage = |var_id| VarUsage { var_id, location };
        let db = self.db;
        let snapshot_stmt = |vars: &mut VariableArena<'_>, pre_snap, post_snap| {
            let ignored = vars.alloc(vars[pre_snap].clone());
            Statement::Snapshot(StatementSnapshot::new(as_usage(pre_snap), ignored, post_snap))
        };
        // Checking whether we have actual const info on the enum.
        if let VarInfo::Const(const_value) = var_info
            && let ConstValue::Enum(variant, value) = const_value.long(db)
        {
            let arm = &info.arms[variant.idx];
            let output = arm.var_ids[0];
            // Propagating the const value information.
            self.var_info.insert(output, VarInfo::Const(*value).wrap_with_snapshots(n_snapshots));
            if self.variables[input].info.droppable.is_ok()
                && self.variables[output].info.copyable.is_ok()
                && let Ok(mut ty) = value.ty(db)
                && let Some(mut stmt) = self.try_generate_const_statement(*value, output)
            {
                // Adding snapshot taking statements for snapshots.
                for _ in 0..n_snapshots {
                    let non_snap_var = Variable::with_default_context(db, ty, location);
                    ty = TypeLongId::Snapshot(ty).intern(db);
                    let pre_snap = self.variables.alloc(non_snap_var);
                    stmt.outputs_mut()[0] = pre_snap;
                    let take_snap = snapshot_stmt(self.variables, pre_snap, output);
                    statements.push(core::mem::replace(&mut stmt, take_snap));
                }
                statements.push(stmt);
                return Some(BlockEnd::Goto(arm.block_id, Default::default()));
            }
        } else if let VarInfo::Enum { variant, payload } = var_info {
            let arm = &info.arms[variant.idx];
            let variant_ty = variant.ty;
            let output = arm.var_ids[0];
            let payload = payload.as_ref().clone();
            let unwrapped =
                self.variables[input].info.droppable.is_ok().then_some(()).and_then(|_| {
                    let (extra_snapshots, inner) = payload.peel_snapshots();
                    match inner {
                        VarInfo::Var(var) if self.variables[var.var_id].info.copyable.is_ok() => {
                            Some((var.var_id, extra_snapshots))
                        }
                        VarInfo::Const(value) => {
                            let const_var = self
                                .variables
                                .alloc(Variable::with_default_context(db, variant_ty, location));
                            statements.push(self.try_generate_const_statement(*value, const_var)?);
                            Some((const_var, extra_snapshots))
                        }
                        _ => None,
                    }
                });
            // Propagating the const value information.
            self.var_info.insert(output, payload.wrap_with_snapshots(n_snapshots));
            if let Some((mut unwrapped, extra_snapshots)) = unwrapped {
                let total_snapshots = n_snapshots + extra_snapshots;
                if total_snapshots != 0 {
                    // Adding snapshot taking statements for snapshots.
                    for _ in 1..total_snapshots {
                        let ty = TypeLongId::Snapshot(self.variables[unwrapped].ty).intern(db);
                        let non_snap_var = Variable::with_default_context(self.db, ty, location);
                        let snapped = self.variables.alloc(non_snap_var);
                        statements.push(snapshot_stmt(self.variables, unwrapped, snapped));
                        unwrapped = snapped;
                    }
                    statements.push(snapshot_stmt(self.variables, unwrapped, output));
                };
                return Some(BlockEnd::Goto(arm.block_id, Default::default()));
            }
        }
        None
    }

    /// Handles the end of a block based on an extern function call.
    /// Possibly extends the blocks statements as well.
    /// Returns None if no additional changes are required.
    /// If changes are required, returns the updated block end.
    fn handle_extern_block_end(
        &mut self,
        info: &mut MatchExternInfo<'db>,
        statements: &mut Vec<Statement<'db>>,
    ) -> Option<BlockEnd<'db>> {
        let db = self.db;
        let (id, generic_args) = info.function.get_extern(db)?;
        if self.nz_fns.contains(&id) {
            let val = self.as_const(info.inputs[0].var_id)?;
            let is_zero = match val.long(db) {
                ConstValue::Int(v, _) => v.is_zero(),
                ConstValue::Struct(s, _) => s.iter().all(|v| {
                    v.long(db).to_int().expect("Expected ConstValue::Int for size").is_zero()
                }),
                _ => unreachable!(),
            };
            Some(if is_zero {
                BlockEnd::Goto(info.arms[0].block_id, Default::default())
            } else {
                let arm = &info.arms[1];
                let nz_var = arm.var_ids[0];
                let nz_val = ConstValue::NonZero(val).intern(db);
                self.var_info.insert(nz_var, VarInfo::Const(nz_val));
                statements.push(Statement::Const(StatementConst::new_flat(nz_val, nz_var)));
                BlockEnd::Goto(arm.block_id, Default::default())
            })
        } else if self.eq_fns.contains(&id) {
            let lhs = self.as_int(info.inputs[0].var_id);
            let rhs = self.as_int(info.inputs[1].var_id);
            if (lhs.map(Zero::is_zero).unwrap_or_default() && rhs.is_none())
                || (rhs.map(Zero::is_zero).unwrap_or_default() && lhs.is_none())
            {
                let nz_input = info.inputs[if lhs.is_some() { 1 } else { 0 }];
                let var = &self.variables[nz_input.var_id].clone();
                let function = self.type_info.get(&var.ty)?.is_zero;
                let unused_nz_var = Variable::with_default_context(
                    db,
                    corelib::core_nonzero_ty(db, var.ty),
                    var.location,
                );
                let unused_nz_var = self.variables.alloc(unused_nz_var);
                return Some(BlockEnd::Match {
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
                });
            }
            Some(BlockEnd::Goto(
                info.arms[if lhs? == rhs? { 1 } else { 0 }].block_id,
                Default::default(),
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
                let range = self.type_value_ranges.get(&ty)?;
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
                let value = ConstValue::Int(value, ty).intern(db);
                self.var_info.insert(actual_output, VarInfo::Const(value));
                statements.push(Statement::Const(StatementConst::new_flat(value, actual_output)));
                return Some(BlockEnd::Goto(arm.block_id, Default::default()));
            }
            if let Some(rhs) = rhs {
                if rhs.is_zero() && !self.diff_fns.contains(&id) {
                    let arm = &info.arms[0];
                    self.var_info.insert(arm.var_ids[0], VarInfo::Var(info.inputs[0]));
                    return Some(BlockEnd::Goto(arm.block_id, Default::default()));
                }
                if rhs.is_one() && !self.diff_fns.contains(&id) {
                    let ty = self.variables[info.arms[0].var_ids[0]].ty;
                    let ty_info = self.type_info.get(&ty)?;
                    let function = if self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id) {
                        ty_info.inc?
                    } else {
                        ty_info.dec?
                    };
                    let enum_ty = function.signature(db).ok()?.return_type;
                    let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) =
                        enum_ty.long(db)
                    else {
                        return None;
                    };
                    let result = self.variables.alloc(Variable::with_default_context(
                        db,
                        function.signature(db).unwrap().return_type,
                        info.location,
                    ));
                    statements.push(Statement::Call(StatementCall {
                        function,
                        inputs: vec![info.inputs[0]],
                        with_coupon: false,
                        outputs: vec![result],
                        location: info.location,
                        is_specialization_base_call: false,
                    }));
                    return Some(BlockEnd::Match {
                        info: MatchInfo::Enum(MatchEnumInfo {
                            concrete_enum_id: *concrete_enum_id,
                            input: VarUsage { var_id: result, location: info.location },
                            arms: core::mem::take(&mut info.arms),
                            location: info.location,
                        }),
                    });
                }
            }
            if let Some(lhs) = lhs
                && lhs.is_zero()
                && (self.uadd_fns.contains(&id) || self.iadd_fns.contains(&id))
            {
                let arm = &info.arms[0];
                self.var_info.insert(arm.var_ids[0], VarInfo::Var(info.inputs[1]));
                return Some(BlockEnd::Goto(arm.block_id, Default::default()));
            }
            None
        } else if let Some(reversed) = self.downcast_fns.get(&id) {
            let range = |ty: TypeId<'_>| {
                Some(if let Some(range) = self.type_value_ranges.get(&ty) {
                    range.clone()
                } else {
                    let (min, max) = corelib::try_extract_bounded_int_type_ranges(db, ty)?;
                    TypeRange { min, max }
                })
            };
            let (success_arm, failure_arm) = if *reversed { (1, 0) } else { (0, 1) };
            let input_var = info.inputs[0].var_id;
            let in_ty = self.variables[input_var].ty;
            let success_output = info.arms[success_arm].var_ids[0];
            let out_ty = self.variables[success_output].ty;
            let out_range = range(out_ty)?;
            let Some(value) = self.as_int(input_var) else {
                let in_range = range(in_ty)?;
                return if in_range.min < out_range.min || in_range.max > out_range.max {
                    None
                } else {
                    let generic_args = [in_ty, out_ty].map(GenericArgumentId::Type).to_vec();
                    let function = db.core_info().upcast_fn.concretize(db, generic_args);
                    statements.push(Statement::Call(StatementCall {
                        function: function.lowered(db),
                        inputs: vec![info.inputs[0]],
                        with_coupon: false,
                        outputs: vec![success_output],
                        location: info.location,
                        is_specialization_base_call: false,
                    }));
                    return Some(BlockEnd::Goto(
                        info.arms[success_arm].block_id,
                        Default::default(),
                    ));
                };
            };
            let value = if in_ty == self.felt252 {
                felt252_for_downcast(value, &out_range.min)
            } else {
                value.clone()
            };
            Some(if let NormalizedResult::InRange(value) = out_range.normalized(value) {
                let value = ConstValue::Int(value, out_ty).intern(db);
                self.var_info.insert(success_output, VarInfo::Const(value));
                statements.push(Statement::Const(StatementConst::new_flat(value, success_output)));
                BlockEnd::Goto(info.arms[success_arm].block_id, Default::default())
            } else {
                BlockEnd::Goto(info.arms[failure_arm].block_id, Default::default())
            })
        } else if id == self.bounded_int_constrain {
            let input_var = info.inputs[0].var_id;
            let value = self.as_int(input_var)?;
            let generic_arg = generic_args[1];
            let constrain_value = extract_matches!(generic_arg, GenericArgumentId::Constant)
                .long(db)
                .to_int()
                .expect("Expected ConstValue::Int for size");
            let arm_idx = if value < constrain_value { 0 } else { 1 };
            let output = info.arms[arm_idx].var_ids[0];
            statements.push(self.propagate_const_and_get_statement(value.clone(), output));
            Some(BlockEnd::Goto(info.arms[arm_idx].block_id, Default::default()))
        } else if id == self.bounded_int_trim_min {
            let input_var = info.inputs[0].var_id;
            let ConstValue::Int(value, ty) = self.as_const(input_var)?.long(self.db) else {
                return None;
            };
            let is_trimmed = if let Some(range) = self.type_value_ranges.get(ty) {
                range.min == *value
            } else {
                corelib::try_extract_bounded_int_type_ranges(db, *ty)?.0 == *value
            };
            let arm_idx = if is_trimmed {
                0
            } else {
                let output = info.arms[1].var_ids[0];
                statements.push(self.propagate_const_and_get_statement(value.clone(), output));
                1
            };
            Some(BlockEnd::Goto(info.arms[arm_idx].block_id, Default::default()))
        } else if id == self.bounded_int_trim_max {
            let input_var = info.inputs[0].var_id;
            let ConstValue::Int(value, ty) = self.as_const(input_var)?.long(self.db) else {
                return None;
            };
            let is_trimmed = if let Some(range) = self.type_value_ranges.get(ty) {
                range.max == *value
            } else {
                corelib::try_extract_bounded_int_type_ranges(db, *ty)?.1 == *value
            };
            let arm_idx = if is_trimmed {
                0
            } else {
                let output = info.arms[1].var_ids[0];
                statements.push(self.propagate_const_and_get_statement(value.clone(), output));
                1
            };
            Some(BlockEnd::Goto(info.arms[arm_idx].block_id, Default::default()))
        } else if id == self.array_get {
            let index = self.as_int(info.inputs[1].var_id)?.to_usize()?;
            if let Some(VarInfo::Snapshot(arr_info)) = self.var_info.get(&info.inputs[0].var_id)
                && let VarInfo::Array(infos) = arr_info.as_ref()
            {
                match infos.get(index) {
                    Some(Some(output_var_info)) => {
                        let arm = &info.arms[0];
                        let output_var_info = output_var_info.clone();
                        let box_info =
                            VarInfo::Box(VarInfo::Snapshot(output_var_info.clone().into()).into());
                        self.var_info.insert(arm.var_ids[0], box_info);
                        if let VarInfo::Const(value) = output_var_info {
                            let value_ty = value.ty(db).ok()?;
                            let value_box_ty = corelib::core_box_ty(db, value_ty);
                            let location = info.location;
                            let boxed_var =
                                Variable::with_default_context(db, value_box_ty, location);
                            let boxed = self.variables.alloc(boxed_var.clone());
                            let unused_boxed = self.variables.alloc(boxed_var);
                            let snapped = self.variables.alloc(Variable::with_default_context(
                                db,
                                TypeLongId::Snapshot(value_box_ty).intern(db),
                                location,
                            ));
                            statements.extend([
                                Statement::Const(StatementConst::new_boxed(value, boxed)),
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
                                    is_specialization_base_call: false,
                                }),
                            ]);
                            return Some(BlockEnd::Goto(arm.block_id, Default::default()));
                        }
                    }
                    None => {
                        return Some(BlockEnd::Goto(info.arms[1].block_id, Default::default()));
                    }
                    Some(None) => {}
                }
            }
            if index.is_zero()
                && let [success, failure] = info.arms.as_mut_slice()
            {
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
                Some(BlockEnd::Goto(
                    arm.block_id,
                    VarRemapping {
                        remapping: FromIterator::from_iter([(arm.var_ids[0], info.inputs[0])]),
                    },
                ))
            }
        } else if id == self.array_snapshot_pop_back || id == self.array_snapshot_pop_front {
            let var_info = self.var_info.get(&info.inputs[0].var_id)?;
            let desnapped = try_extract_matches!(var_info, VarInfo::Snapshot)?;
            let element_var_infos = try_extract_matches!(desnapped.as_ref(), VarInfo::Array)?;
            // TODO(orizi): Propagate success values as well.
            if element_var_infos.is_empty() {
                let arm = &info.arms[1];
                self.var_info.insert(arm.var_ids[0], VarInfo::Array(vec![]));
                Some(BlockEnd::Goto(
                    arm.block_id,
                    VarRemapping {
                        remapping: FromIterator::from_iter([(arm.var_ids[0], info.inputs[0])]),
                    },
                ))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Returns the const value of a variable if it exists.
    fn as_const(&self, var_id: VariableId) -> Option<ConstValueId<'db>> {
        try_extract_matches!(self.var_info.get(&var_id)?, VarInfo::Const).copied()
    }

    /// Return the const value as a int if it exists and is an integer.
    fn as_int(&self, var_id: VariableId) -> Option<&BigInt> {
        match self.as_const(var_id)?.long(self.db) {
            ConstValue::Int(value, _) => Some(value),
            ConstValue::NonZero(const_value) => {
                if let ConstValue::Int(value, _) = const_value.long(self.db) {
                    Some(value)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Replaces the inputs in place if they are in the var_info map.
    fn maybe_replace_inputs(&self, inputs: &mut [VarUsage<'db>]) {
        for input in inputs {
            self.maybe_replace_input(input);
        }
    }

    /// Replaces the input in place if it is in the var_info map.
    fn maybe_replace_input(&self, input: &mut VarUsage<'db>) {
        if let Some(VarInfo::Var(new_var)) = self.var_info.get(&input.var_id) {
            *input = *new_var;
        }
    }

    /// Given a var_info and its type, return the corresponding specialization argument, if it
    /// exists.
    fn try_get_specialization_arg(
        &self,
        var_info: VarInfo<'db>,
        ty: TypeId<'db>,
        unknown_vars: &mut Vec<VarUsage<'db>>,
    ) -> Option<SpecializationArg<'db>> {
        if self.db.type_size_info(ty).ok()? == TypeSizeInformation::ZeroSized {
            // Skip zero-sized constants as they are not supported in sierra-gen.
            return None;
        }

        match var_info {
            VarInfo::Const(value) => Some(SpecializationArg::Const { value, boxed: false }),
            VarInfo::Box(info) => try_extract_matches!(info.as_ref(), VarInfo::Const)
                .map(|value| SpecializationArg::Const { value: *value, boxed: true }),
            VarInfo::Snapshot(info) => {
                let desnap_ty = *extract_matches!(ty.long(self.db), TypeLongId::Snapshot);
                // Use a local accumulator to avoid mutating unknown_vars if we return None.
                let mut local_unknown_vars: Vec<VarUsage<'db>> = Vec::new();
                let inner = self.try_get_specialization_arg(
                    info.as_ref().clone(),
                    desnap_ty,
                    &mut local_unknown_vars,
                )?;
                unknown_vars.extend(local_unknown_vars);
                Some(SpecializationArg::Snapshot(Box::new(inner)))
            }
            VarInfo::Array(infos) => {
                let TypeLongId::Concrete(concrete_ty) = ty.long(self.db) else {
                    unreachable!("Expected a concrete type");
                };
                let [GenericArgumentId::Type(inner_ty)] = &concrete_ty.generic_args(self.db)[..]
                else {
                    unreachable!("Expected a single type generic argument");
                };
                // Accumulate into locals first; only extend unknown_vars if we end up specializing.
                let mut vars = vec![];
                let mut args = vec![];
                for info in infos {
                    let info = info?;
                    let arg = self.try_get_specialization_arg(info, *inner_ty, &mut vars)?;
                    args.push(arg);
                }
                if !args.is_empty()
                    && args.iter().all(|arg| matches!(arg, SpecializationArg::NotSpecialized))
                {
                    return None;
                }
                unknown_vars.extend(vars);
                Some(SpecializationArg::Array(*inner_ty, args))
            }
            VarInfo::Struct(infos) => {
                let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct)) =
                    ty.long(self.db)
                else {
                    // TODO(ilya): Support closures and fixed size arrays.
                    return None;
                };

                let members = self.db.concrete_struct_members(*concrete_struct).unwrap();
                let mut struct_args = Vec::new();
                // Accumulate into locals first; only extend unknown_vars if we end up specializing.
                let mut vars = vec![];
                for (member, opt_var_info) in zip_eq(members.values(), infos) {
                    let var_info = opt_var_info?;
                    let arg = self.try_get_specialization_arg(var_info, member.ty, &mut vars)?;
                    struct_args.push(arg);
                }
                if !struct_args.is_empty()
                    && struct_args
                        .iter()
                        .all(|arg| matches!(arg, SpecializationArg::NotSpecialized))
                {
                    return None;
                }
                unknown_vars.extend(vars);
                Some(SpecializationArg::Struct(struct_args))
            }
            VarInfo::Enum { variant, payload } => {
                let mut local_unknown_vars = vec![];
                let payload_arg = self.try_get_specialization_arg(
                    payload.as_ref().clone(),
                    variant.ty,
                    &mut local_unknown_vars,
                )?;

                unknown_vars.extend(local_unknown_vars);
                Some(SpecializationArg::Enum { variant, payload: Box::new(payload_arg) })
            }
            VarInfo::Var(var_usage) => {
                unknown_vars.push(var_usage);
                Some(SpecializationArg::NotSpecialized)
            }
        }
    }

    /// Returns true if const-folding should be skipped for the current function.
    pub fn should_skip_const_folding(&self, db: &'db dyn Database) -> bool {
        if db.optimizations().skip_const_folding() {
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
fn var_info_if_copy<'db>(
    variables: &VariableArena<'db>,
    input: VarUsage<'db>,
) -> Option<VarInfo<'db>> {
    variables[input.var_id].info.copyable.is_ok().then_some(VarInfo::Var(input))
}

/// Internal query for the libfuncs information required for const folding.
#[salsa::tracked(returns(ref))]
fn priv_const_folding_info<'db>(
    db: &'db dyn Database,
) -> crate::optimizations::const_folding::ConstFoldingLibfuncInfo<'db> {
    ConstFoldingLibfuncInfo::new(db)
}

/// Holds static information about libfuncs required for the optimization.
#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct ConstFoldingLibfuncInfo<'db> {
    /// The `felt252_sub` libfunc.
    felt_sub: ExternFunctionId<'db>,
    /// The `felt252_add` libfunc.
    felt_add: ExternFunctionId<'db>,
    /// The `felt252_mul` libfunc.
    felt_mul: ExternFunctionId<'db>,
    /// The `felt252_div` libfunc.
    felt_div: ExternFunctionId<'db>,
    /// The `into_box` libfunc.
    into_box: ExternFunctionId<'db>,
    /// The `unbox` libfunc.
    unbox: ExternFunctionId<'db>,
    /// The `box_forward_snapshot` libfunc.
    box_forward_snapshot: GenericFunctionId<'db>,
    /// The set of functions that check if numbers are equal.
    eq_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to add unsigned ints.
    uadd_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to subtract unsigned ints.
    usub_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to get the difference of signed ints.
    diff_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to add signed ints.
    iadd_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to subtract signed ints.
    isub_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to multiply integers.
    wide_mul_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The set of functions to divide and get the remainder of integers.
    div_rem_fns: OrderedHashSet<ExternFunctionId<'db>>,
    /// The `bounded_int_add` libfunc.
    bounded_int_add: ExternFunctionId<'db>,
    /// The `bounded_int_sub` libfunc.
    bounded_int_sub: ExternFunctionId<'db>,
    /// The `bounded_int_constrain` libfunc.
    bounded_int_constrain: ExternFunctionId<'db>,
    /// The `bounded_int_trim_min` libfunc.
    bounded_int_trim_min: ExternFunctionId<'db>,
    /// The `bounded_int_trim_max` libfunc.
    bounded_int_trim_max: ExternFunctionId<'db>,
    /// The `array_get` libfunc.
    array_get: ExternFunctionId<'db>,
    /// The `array_snapshot_pop_front` libfunc.
    array_snapshot_pop_front: ExternFunctionId<'db>,
    /// The `array_snapshot_pop_back` libfunc.
    array_snapshot_pop_back: ExternFunctionId<'db>,
    /// The `array_len` libfunc.
    array_len: ExternFunctionId<'db>,
    /// The `array_new` libfunc.
    array_new: ExternFunctionId<'db>,
    /// The `array_append` libfunc.
    array_append: ExternFunctionId<'db>,
    /// The `array_pop_front` libfunc.
    array_pop_front: ExternFunctionId<'db>,
    /// The `storage_base_address_from_felt252` libfunc.
    storage_base_address_from_felt252: ExternFunctionId<'db>,
    /// The `storage_base_address_const` libfunc.
    storage_base_address_const: GenericFunctionId<'db>,
    /// The `core::panic_with_felt252` function.
    panic_with_felt252: FunctionId<'db>,
    /// The `core::panic_with_const_felt252` function.
    pub panic_with_const_felt252: FreeFunctionId<'db>,
    /// The `core::panics::panic_with_byte_array` function.
    panic_with_byte_array: FunctionId<'db>,
    /// Information per type.
    type_info: OrderedHashMap<TypeId<'db>, TypeInfo<'db>>,
    /// The info used for semantic const calculation.
    const_calculation_info: Arc<ConstCalcInfo<'db>>,
}
impl<'db> ConstFoldingLibfuncInfo<'db> {
    fn new(db: &'db dyn Database) -> Self {
        let core = ModuleHelper::core(db);
        let box_module = core.submodule("box");
        let integer_module = core.submodule("integer");
        let internal_module = core.submodule("internal");
        let bounded_int_module = internal_module.submodule("bounded_int");
        let num_module = internal_module.submodule("num");
        let array_module = core.submodule("array");
        let starknet_module = core.submodule("starknet");
        let storage_access_module = starknet_module.submodule("storage_access");
        let utypes = ["u8", "u16", "u32", "u64", "u128"];
        let itypes = ["i8", "i16", "i32", "i64", "i128"];
        let eq_fns = OrderedHashSet::<_>::from_iter(
            chain!(utypes, itypes).map(|ty| integer_module.extern_function_id(&format!("{ty}_eq"))),
        );
        let uadd_fns = OrderedHashSet::<_>::from_iter(
            utypes.map(|ty| integer_module.extern_function_id(&format!("{ty}_overflowing_add"))),
        );
        let usub_fns = OrderedHashSet::<_>::from_iter(
            utypes.map(|ty| integer_module.extern_function_id(&format!("{ty}_overflowing_sub"))),
        );
        let diff_fns = OrderedHashSet::<_>::from_iter(
            itypes.map(|ty| integer_module.extern_function_id(&format!("{ty}_diff"))),
        );
        let iadd_fns =
            OrderedHashSet::<_>::from_iter(itypes.map(|ty| {
                integer_module.extern_function_id(&format!("{ty}_overflowing_add_impl"))
            }));
        let isub_fns =
            OrderedHashSet::<_>::from_iter(itypes.map(|ty| {
                integer_module.extern_function_id(&format!("{ty}_overflowing_sub_impl"))
            }));
        let wide_mul_fns = OrderedHashSet::<_>::from_iter(chain!(
            [bounded_int_module.extern_function_id("bounded_int_mul")],
            ["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64"]
                .map(|ty| integer_module.extern_function_id(&format!("{ty}_wide_mul"))),
        ));
        let div_rem_fns = OrderedHashSet::<_>::from_iter(chain!(
            [bounded_int_module.extern_function_id("bounded_int_div_rem")],
            utypes.map(|ty| integer_module.extern_function_id(&format!("{ty}_safe_divmod"))),
        ));
        let type_info: OrderedHashMap<TypeId<'db>, TypeInfo<'db>> = OrderedHashMap::from_iter(
            [
                ("u8", false, true),
                ("u16", false, true),
                ("u32", false, true),
                ("u64", false, true),
                ("u128", false, true),
                ("u256", false, false),
                ("i8", true, true),
                ("i16", true, true),
                ("i32", true, true),
                ("i64", true, true),
                ("i128", true, true),
            ]
            .map(|(ty_name, as_bounded_int, inc_dec): (&'static str, bool, bool)| {
                let ty = corelib::get_core_ty_by_name(db, SmolStrId::from(db, ty_name), vec![]);
                let is_zero = if as_bounded_int {
                    bounded_int_module
                        .function_id("bounded_int_is_zero", vec![GenericArgumentId::Type(ty)])
                } else {
                    integer_module.function_id(
                        SmolStrId::from(db, format!("{ty_name}_is_zero")).long(db).as_str(),
                        vec![],
                    )
                }
                .lowered(db);
                let (inc, dec) = if inc_dec {
                    (
                        Some(
                            num_module
                                .function_id(
                                    SmolStrId::from(db, format!("{ty_name}_inc")).long(db).as_str(),
                                    vec![],
                                )
                                .lowered(db),
                        ),
                        Some(
                            num_module
                                .function_id(
                                    SmolStrId::from(db, format!("{ty_name}_dec")).long(db).as_str(),
                                    vec![],
                                )
                                .lowered(db),
                        ),
                    )
                } else {
                    (None, None)
                };
                let info = TypeInfo { is_zero, inc, dec };
                (ty, info)
            }),
        );
        Self {
            felt_sub: core.extern_function_id("felt252_sub"),
            felt_add: core.extern_function_id("felt252_add"),
            felt_mul: core.extern_function_id("felt252_mul"),
            felt_div: core.extern_function_id("felt252_div"),
            into_box: box_module.extern_function_id("into_box"),
            unbox: box_module.extern_function_id("unbox"),
            box_forward_snapshot: box_module.generic_function_id("box_forward_snapshot"),
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
            bounded_int_trim_min: bounded_int_module.extern_function_id("bounded_int_trim_min"),
            bounded_int_trim_max: bounded_int_module.extern_function_id("bounded_int_trim_max"),
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
            type_info,
            const_calculation_info: db.const_calc_info(),
        }
    }
}

impl<'db> std::ops::Deref for ConstFoldingContext<'db, '_> {
    type Target = ConstFoldingLibfuncInfo<'db>;
    fn deref(&self) -> &ConstFoldingLibfuncInfo<'db> {
        self.libfunc_info
    }
}

impl<'a> std::ops::Deref for ConstFoldingLibfuncInfo<'a> {
    type Target = ConstCalcInfo<'a>;
    fn deref(&self) -> &ConstCalcInfo<'a> {
        &self.const_calculation_info
    }
}

/// The information of a type required for const foldings.
#[derive(Debug, PartialEq, Eq, salsa::Update)]
struct TypeInfo<'db> {
    /// The function to check if the value is zero for the type.
    is_zero: FunctionId<'db>,
    /// Inc function to increase the value by one.
    inc: Option<FunctionId<'db>>,
    /// Dec function to decrease the value by one.
    dec: Option<FunctionId<'db>>,
}

trait TypeRangeNormalizer {
    /// Normalizes the value to the range.
    /// Assumes the value is within size of range of the range.
    fn normalized(&self, value: BigInt) -> NormalizedResult;
}
impl TypeRangeNormalizer for TypeRange {
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
