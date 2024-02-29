#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use cairo_lang_semantic::corelib;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use num_bigint::BigInt;
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, Statement, StatementCall, StatementDesnap,
    StatementLiteral, VarUsage, VariableId,
};

/// Keeps track of equivalent values that a variables might be replaced with.
/// Note: We don't keep track of types as we assume the usage is always correct.
enum VarInfo {
    /// The variable is a literal value.
    Literal(BigInt),
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
                Statement::Literal(StatementLiteral { value, output }) => {
                    var_info.insert(*output, VarInfo::Literal(value.clone()));
                }
                Statement::Snapshot(stmt) => {
                    if let Some(VarInfo::Literal(val)) = var_info.get(&stmt.input.var_id) {
                        let val = val.clone();
                        var_info.insert(stmt.original(), VarInfo::Literal(val.clone()));
                        var_info.insert(stmt.snapshot(), VarInfo::Literal(val));
                    }
                }
                Statement::Desnap(StatementDesnap { input, output }) => {
                    if let Some(VarInfo::Literal(val)) = var_info.get(&input.var_id) {
                        let val = val.clone();
                        var_info.insert(*output, VarInfo::Literal(val));
                    }
                }
                Statement::Call(StatementCall { function, ref mut inputs, outputs, .. }) => {
                    // (a - 0) can be replaced by a.
                    if function == &felt_sub {
                        if let Some(VarInfo::Literal(val)) = var_info.get(&inputs[1].var_id) {
                            if val.is_zero() {
                                var_info.insert(outputs[0], VarInfo::Var(inputs[0]));
                            }
                        }
                    }
                }
                _ => {}
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
            }
            FlatBlockEnd::Return(ref mut inputs, ..) => {
                maybe_replace_inputs(&var_info, inputs.as_mut_slice());
            }
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        };
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
