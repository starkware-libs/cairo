#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_semantic::corelib;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::zip_eq;
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchInfo, Statement, StatementCall,
    StatementConst, StatementDesnap, StatementEnumConstruct, StatementStructConstruct,
    StatementStructDestructure, VarUsage, VariableId,
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
    let felt_sub = db.intern_lowering_function(FunctionLongId::Semantic(
        corelib::get_core_function_id(semantic_db, "felt252_sub".into(), vec![]),
    ));
    let box_module = corelib::core_submodule(db.upcast(), "box");
    let Ok(Some(ModuleItemId::ExternFunction(into_box))) =
        db.module_item_by_name(box_module, "into_box".into())
    else {
        unreachable!("core::box::into_box not found");
    };
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
                if let MatchInfo::Enum(MatchEnumInfo { input, arms, .. }) = info {
                    if let Some(VarInfo::Const(ConstValue::Enum(variant, value))) =
                        var_info.get(&input.var_id)
                    {
                        let arm = &arms[variant.idx];
                        var_info.insert(arm.var_ids[0], VarInfo::Const(value.as_ref().clone()));
                    }
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
