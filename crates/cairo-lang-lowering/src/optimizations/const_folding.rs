#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_semantic::corelib;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::{chain, zip_eq};
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementConst, StatementDesnap, StatementEnumConstruct,
    StatementStructConstruct, StatementStructDestructure, VarUsage, VariableId,
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
    let mut var_info = UnorderedHashMap::<_, _>::default();

    let semantic_db = db.upcast();
    let to_lowering_id = |id| db.intern_lowering_function(FunctionLongId::Semantic(id));
    let get_extern = |module, name: &str, fullpath: &str| {
        let Ok(Some(ModuleItemId::ExternFunction(id))) =
            db.module_item_by_name(module, name.into())
        else {
            unreachable!("`{fullpath}` not found");
        };
        id
    };
    let felt_sub =
        to_lowering_id(corelib::get_core_function_id(semantic_db, "felt252_sub".into(), vec![]));
    let box_module = corelib::core_submodule(db.upcast(), "box");
    let into_box = get_extern(box_module, "into_box", "core::box::into_box");
    let integer_module = corelib::core_submodule(db.upcast(), "integer");
    let upcast = get_extern(integer_module, "upcast", "core::integer::upcast");
    let nz_fns = UnorderedHashSet::<_>::from_iter(
        chain!(
            [corelib::get_core_function_id(semantic_db, "felt252_is_zero".into(), vec![])],
            ["u8", "u16", "u32", "u64", "u128", "u256", "i8", "i16", "i32", "i64", "i128"].map(
                |ty| {
                    corelib::get_function_id(
                        semantic_db,
                        integer_module,
                        format!("{}_is_zero", ty).into(),
                        vec![],
                    )
                }
            )
        )
        .map(to_lowering_id),
    );
    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &mut lowered.blocks[block_id];
        for stmt in block.statements.iter_mut() {
            maybe_replace_inputs(&var_info, stmt.inputs_mut());
            match stmt {
                Statement::Const(StatementConst { value, output }) => {
                    var_info.insert(*output, VarInfo::Const(value.clone()));
                }
                Statement::Snapshot(stmt) => {
                    if let Some(VarInfo::Const(val)) = var_info.get(&stmt.input.var_id) {
                        let val = val.clone();
                        var_info.insert(stmt.original(), VarInfo::Const(val.clone()));
                        var_info.insert(stmt.snapshot(), VarInfo::Const(val));
                    }
                }
                Statement::Desnap(StatementDesnap { input, output }) => {
                    if let Some(VarInfo::Const(val)) = var_info.get(&input.var_id) {
                        let val = val.clone();
                        var_info.insert(*output, VarInfo::Const(val));
                    }
                }
                Statement::Call(StatementCall { function, ref mut inputs, outputs, .. }) => {
                    // (a - 0) can be replaced by a.
                    if function == &felt_sub {
                        if let Some(VarInfo::Const(ConstValue::Int(val))) =
                            var_info.get(&inputs[1].var_id)
                        {
                            if val.is_zero() {
                                var_info.insert(outputs[0], VarInfo::Var(inputs[0]));
                            }
                        }
                    } else if let Some(extrn) = function.get_extern(db) {
                        if extrn == into_box {
                            if let Some(VarInfo::Const(val)) = var_info.get(&inputs[0].var_id) {
                                let value = ConstValue::Boxed(
                                    lowered.variables[inputs[0].var_id].ty,
                                    val.clone().into(),
                                );
                                var_info.insert(outputs[0], VarInfo::Const(value.clone()));
                                *stmt =
                                    Statement::Const(StatementConst { value, output: outputs[0] });
                            }
                        } else if extrn == upcast {
                            if let Some(VarInfo::Const(value)) = var_info.get(&inputs[0].var_id) {
                                let value = value.clone();
                                var_info.insert(outputs[0], VarInfo::Const(value.clone()));
                                *stmt =
                                    Statement::Const(StatementConst { value, output: outputs[0] });
                            }
                        }
                    }
                }
                Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                    if let Some(args) = inputs
                        .iter()
                        .map(|input| {
                            if let Some(VarInfo::Const(val)) = var_info.get(&input.var_id) {
                                Some((lowered.variables[input.var_id].ty, val.clone()))
                            } else {
                                None
                            }
                        })
                        .collect::<Option<Vec<_>>>()
                    {
                        let value = ConstValue::Struct(args);
                        var_info.insert(*output, VarInfo::Const(value.clone()));
                    }
                }
                Statement::StructDestructure(StatementStructDestructure { input, outputs }) => {
                    if let Some(VarInfo::Const(ConstValue::Struct(args))) =
                        var_info.get(&input.var_id)
                    {
                        for (output, (_, val)) in zip_eq(outputs, args.clone()) {
                            var_info.insert(*output, VarInfo::Const(val));
                        }
                    }
                }
                Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) => {
                    if let Some(VarInfo::Const(val)) = var_info.get(&input.var_id) {
                        let value = ConstValue::Enum(variant.clone(), val.clone().into());
                        var_info.insert(*output, VarInfo::Const(value.clone()));
                    }
                }
            }
        }

        match &mut block.end {
            FlatBlockEnd::Goto(block_id, remappings) => {
                stack.push(*block_id);
                for (_, v) in remappings.iter_mut() {
                    maybe_replace_input(&var_info, v);
                }
            }
            FlatBlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));
                maybe_replace_inputs(&var_info, info.inputs_mut());
                match info {
                    MatchInfo::Enum(MatchEnumInfo { input, arms, .. }) => {
                        if let Some(VarInfo::Const(ConstValue::Enum(variant, value))) =
                            var_info.get(&input.var_id)
                        {
                            let arm = &arms[variant.idx];
                            var_info.insert(arm.var_ids[0], VarInfo::Const(value.as_ref().clone()));
                        }
                    }
                    MatchInfo::Extern(MatchExternInfo { function, inputs, arms, .. }) => {
                        if nz_fns.contains(function) {
                            let input_var = inputs[0].var_id;
                            if let Some(VarInfo::Const(val)) = var_info.get(&input_var) {
                                let is_zero = match val {
                                    ConstValue::Int(v) => v.is_zero(),
                                    ConstValue::Struct(s) => s.iter().all(|(_, v)| {
                                        extract_matches!(v, ConstValue::Int).is_zero()
                                    }),
                                    _ => unreachable!(),
                                };
                                if is_zero {
                                    block.end =
                                        FlatBlockEnd::Goto(arms[0].block_id, Default::default());
                                } else {
                                    let arm = &arms[1];
                                    let nz_var = arm.var_ids[0];
                                    let nz_val = ConstValue::NonZero(
                                        lowered.variables[input_var].ty,
                                        Box::new(val.clone()),
                                    );
                                    var_info.insert(nz_var, VarInfo::Const(nz_val.clone()));
                                    block.statements.push(Statement::Const(StatementConst {
                                        value: nz_val,
                                        output: nz_var,
                                    }));
                                    block.end =
                                        FlatBlockEnd::Goto(arm.block_id, Default::default());
                                }
                            }
                        }
                    }
                    MatchInfo::Value(..) => {}
                }
            }
            FlatBlockEnd::Return(ref mut inputs, _) => maybe_replace_inputs(&var_info, inputs),
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        }
    }
}

/// Replaces the inputs in place if they are in the var_info map.
fn maybe_replace_inputs(var_info: &UnorderedHashMap<VariableId, VarInfo>, inputs: &mut [VarUsage]) {
    for input in inputs {
        maybe_replace_input(var_info, input);
    }
}

/// Replaces the input in place if it is in the var_info map.
fn maybe_replace_input(var_info: &UnorderedHashMap<VariableId, VarInfo>, input: &mut VarUsage) {
    if let Some(VarInfo::Var(new_var)) = var_info.get(&input.var_id) {
        *input = *new_var;
    }
}
