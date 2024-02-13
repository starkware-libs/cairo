#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use std::collections::HashMap;

use cairo_lang_semantic::corelib;
use num_bigint::BigInt;
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchEnumValue, MatchExternInfo, MatchInfo,
    Statement, StatementCall, StatementDesnap, StatementLiteral, StatementSnapshot, VarUsage,
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
    let mut var_info = HashMap::new();

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
            match stmt {
                Statement::Literal(StatementLiteral { value, output }) => {
                    var_info.insert(*output, VarInfo::Literal(value.clone()));
                }
                Statement::Snapshot(StatementSnapshot {
                    input,
                    output_original,
                    output_snapshot,
                }) => {
                    if let Some(VarInfo::Literal(val)) = var_info.get(&input.var_id) {
                        let val = val.clone();
                        var_info.insert(*output_original, VarInfo::Literal(val.clone()));
                        var_info.insert(*output_snapshot, VarInfo::Literal(val));
                    }
                }
                Statement::Desnap(StatementDesnap { input, output }) => {
                    if let Some(VarInfo::Literal(val)) = var_info.get(&input.var_id) {
                        let val = val.clone();
                        var_info.insert(*output, VarInfo::Literal(val));
                    }
                }
                Statement::Call(StatementCall { function, ref mut inputs, outputs, .. }) => {
                    for input in &mut inputs.iter_mut() {
                        match var_info.get(&input.var_id) {
                            Some(VarInfo::Var(new_var)) => {
                                *input = *new_var;
                            }
                            Some(VarInfo::Literal(_)) | None => {}
                        }
                    }

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

        let maybe_replace_input = |input: &mut VarUsage| {
            if let Some(VarInfo::Var(new_var)) = var_info.get(&input.var_id) {
                *input = *new_var;
            }
        };

        let maybe_replace_inputs = |inputs: &mut [VarUsage]| {
            for input in inputs.iter_mut() {
                maybe_replace_input(input);
            }
        };

        match &mut block.end {
            FlatBlockEnd::Goto(block_id, _remappings) => {
                stack.push(*block_id);
            }
            FlatBlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));

                match info {
                    MatchInfo::Extern(MatchExternInfo { ref mut inputs, .. }) => {
                        maybe_replace_inputs(inputs);
                    }
                    MatchInfo::Enum(MatchEnumInfo { ref mut input, .. })
                    | MatchInfo::Value(MatchEnumValue { ref mut input, .. }) => {
                        maybe_replace_input(input);
                    }
                };
            }
            FlatBlockEnd::Return(ref mut inputs) => {
                maybe_replace_inputs(inputs);
            }
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        };
    }
}
