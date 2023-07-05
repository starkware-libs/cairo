#[cfg(test)]
#[path = "branch_inversion_test.rs"]
mod test;

use cairo_lang_semantic::corelib;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{FlatBlockEnd, FlatLowered, MatchInfo, Statement, StatementCall};

/// Performs branch inversion optimization on lowered function.
///
/// The branch inversion optimization finds a MatchEnum whose input is the output of a call to
/// bool_not_impl, It then swaps the arms of the match enum, it changes the input to the match enum
/// to be the input before the negation and swaps the arms.
pub fn branch_inversion(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }
    let semantic_db = db.upcast();
    let bool_not_func_id = db.intern_lowering_function(FunctionLongId::Semantic(
        corelib::get_core_function_id(semantic_db, "bool_not_impl".into(), vec![]),
    ));

    for block in lowered.blocks.iter_mut() {
        if let FlatBlockEnd::Match { info: MatchInfo::Enum(ref mut info) } = &mut block.end {
            if let Some(negated_condition) = block
                .statements
                .iter()
                .rev()
                .filter_map(|stmt| match stmt {
                    Statement::Call(StatementCall { function, inputs, outputs, .. })
                        if function == &bool_not_func_id && outputs[..] == [info.input.var_id] =>
                    {
                        Some(inputs[0])
                    }
                    _ => None,
                })
                .next()
            {
                info.input = negated_condition;

                // Swap arms.
                let [ref mut false_arm, ref mut true_arm] = &mut info.arms[..] else {
                    panic!("Match on bool should have 2 arms.");
                };

                std::mem::swap(false_arm, true_arm);
                std::mem::swap(&mut false_arm.variant_id, &mut true_arm.variant_id);
            }
        }
    }
}
