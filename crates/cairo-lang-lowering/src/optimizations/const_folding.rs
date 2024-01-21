#[cfg(test)]
#[path = "const_folding_test.rs"]
mod test;

use std::collections::HashMap;

use cairo_lang_semantic::corelib;
use itertools::izip;
use num_bigint::BigInt;
use num_traits::Zero;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{
    FlatBlockEnd, FlatLowered, MatchEnumInfo, MatchExternInfo, MatchInfo, Statement, StatementCall,
    StatementDesnap, StatementLiteral, StatementSnapshot, StatementStructConstruct,
    StatementStructDestructure, VarUsage,
};

/// Keeps track of equivalent values that a variables might be replaced with.
/// Note: We don't keep track of types as we assume the usage is always correct.
enum VarInfo {
    /// The variable is a literal value.
    Literal(BigInt),
    /// The variable can be replaced by another variable.
    Var(VarUsage),

    /// The value is the result of an StructConstruct statement.
    StructConstruct {
        /// The inputs to the StructConstruct.
        inputs: Vec<VarUsage>,
    },
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

    for block in lowered.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            match stmt {
                Statement::Literal(StatementLiteral { value, output }) => {
                    var_info.insert(*output, VarInfo::Literal(value.clone()));
                }

                Statement::StructDestructure(StatementStructDestructure { outputs, input }) => {
                    let mut input_var = &input.var_id;

                    // Flow VarInfo until we get something that is not a Var.
                    let input_info = loop {
                        match var_info.get(input_var) {
                            Some(VarInfo::Var(var_usage)) => {
                                input_var = &var_usage.var_id;
                            }
                            input_info => break input_info,
                        }
                    };
                    let Some(VarInfo::StructConstruct { inputs }) = input_info else {
                        continue;
                    };

                    for (output, input) in izip!(outputs.iter(), inputs.clone().iter()) {
                        var_info.insert(*output, VarInfo::Var(*input));
                    }
                }
                Statement::StructConstruct(StatementStructConstruct { inputs, output }) => {
                    var_info.insert(
                        *output,
                        VarInfo::StructConstruct {
                            inputs: inputs
                                .iter()
                                .map(|input| match var_info.get(&input.var_id) {
                                    Some(VarInfo::Var(new_var)) => *new_var,
                                    Some(VarInfo::Literal(_))
                                    | Some(VarInfo::StructConstruct { .. })
                                    | None => *input,
                                })
                                .collect(),
                        },
                    );
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
                            Some(VarInfo::Literal(_))
                            | Some(VarInfo::StructConstruct { .. })
                            | None => {}
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

        match &mut block.end {
            FlatBlockEnd::Return(ref mut inputs)
            | FlatBlockEnd::Match {
                info: MatchInfo::Extern(MatchExternInfo { ref mut inputs, .. }),
            } => {
                for input in inputs.iter_mut() {
                    if let Some(VarInfo::Var(new_var)) = var_info.get(&input.var_id) {
                        *input = *new_var;
                    }
                }
            }
            FlatBlockEnd::Match { info: MatchInfo::Enum(MatchEnumInfo { ref mut input, .. }) } => {
                if let Some(VarInfo::Var(new_var)) = var_info.get(&input.var_id) {
                    *input = *new_var;
                }
            }
            _ => {}
        };
    }
}
