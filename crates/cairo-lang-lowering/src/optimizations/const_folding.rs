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
    FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchExternInfo, MatchInfo, Statement, StatementCall,
    StatementDesnap, StatementLiteral, StatementSnapshot, VarUsage,
};

// We keep track of two var states:
// - Literal: the variable is a literal value
// - Var: the variable can be replaced by another variable
// We don't keep track of snap/desnap as we assume that they are cancelled out before usage.
enum VarState {
    Literal(BigInt),
    Var(VarUsage),
}

/// Performs constant folding on the lowered program.
pub fn const_folding(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }

    let mut var_states = HashMap::new();

    let semantic_db = db.upcast();
    let felt_sub = db.intern_lowering_function(FunctionLongId::Semantic(
        corelib::get_core_function_id(semantic_db, "felt252_sub".into(), vec![]),
    ));

    for block in lowered.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                Statement::Literal(StatementLiteral { value, output }) => {
                    var_states.insert(*output, VarState::Literal(value.clone()));
                }
                Statement::Snapshot(StatementSnapshot {
                    input,
                    output_original,
                    output_snapshot,
                }) => {
                    if let Some(VarState::Literal(val)) = var_states.get(&input.var_id) {
                        let val = val.clone();
                        var_states.insert(*output_original, VarState::Literal(val.clone()));
                        var_states.insert(*output_snapshot, VarState::Literal(val));
                    }
                }
                Statement::Desnap(StatementDesnap { input, output }) => {
                    if let Some(VarState::Literal(val)) = var_states.get(&input.var_id) {
                        let val = val.clone();
                        var_states.insert(*output, VarState::Literal(val));
                    }
                }
                Statement::Call(StatementCall { function, ref mut inputs, outputs, .. }) => {
                    for input in &mut inputs.iter_mut() {
                        match var_states.get(&input.var_id) {
                            Some(VarState::Var(new_var)) => {
                                *input = *new_var;
                            }
                            Some(VarState::Literal(_)) | None => {}
                        }
                    }

                    // (a - 0) can be repalced by a.
                    if function == &felt_sub {
                        if let Some(VarState::Literal(val)) = var_states.get(&inputs[1].var_id) {
                            if val.is_zero() {
                                var_states.insert(outputs[0], VarState::Var(inputs[0]));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        match &mut block.end {
            FlatBlockEnd::Return(ref mut inputs)
            | FlatBlockEnd::Match {
                info: MatchInfo::Extern(MatchExternInfo { ref mut inputs, .. }),
            } => {
                for input in inputs.iter_mut() {
                    if let Some(VarState::Var(new_var)) = var_states.get(&input.var_id) {
                        *input = *new_var;
                    }
                }
            }
            FlatBlockEnd::Match { info: MatchInfo::Enum(MatchEnumInfo { ref mut input, .. }) } => {
                if let Some(VarState::Var(new_var)) = var_states.get(&input.var_id) {
                    *input = *new_var;
                }
            }
            _ => {}
        };
    }
}
