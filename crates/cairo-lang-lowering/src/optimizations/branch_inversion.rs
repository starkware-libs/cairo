#[cfg(test)]
#[path = "branch_inversion_test.rs"]
mod test;

use cairo_lang_semantic::corelib;

use crate::db::LoweringGroup;
use crate::ids::FunctionLongId;
use crate::{FlatBlockEnd, FlatLowered, MatchInfo, Statement, StatementCall};

/// Performs branch inversion optimization on a lowered function.
///
/// The branch inversion optimization finds a match enum whose input is the output of a call to
/// `bool_not_impl`.
/// It swaps the arms of the match enum and changes its input to be the input before the negation.
///
/// This optimization is valid only if all paths leading to the match enum pass through the call to
/// `bool_not_impl`. Therefore, the call to `bool_not_impl` should be in the same block as the match
/// enum.
///
/// The call to `bool_not_impl` is not deleted as we don't know if its output
/// is used by other statements (or block ending).
///
/// Due to the limitations above, the `reorder_statements` function should be called before this
/// optimization and between this optimization and the match optimization.
///
/// The first call to `reorder_statement`s moves the call to `bool_not_impl` into the block whose
/// match enum we want to optimize.
/// The second call to `reorder_statements` removes the call to `bool_not_impl` if it is unused,
/// allowing the match optimization to be applied to enum_init statements that appeared before the
/// `bool_not_impl`.
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
                    Statement::Call(StatementCall {
                        function,
                        inputs,
                        outputs,
                        with_coupon: false,
                        ..
                    }) if function == &bool_not_func_id && outputs[..] == [info.input.var_id] => {
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
                std::mem::swap(&mut false_arm.arm_selector, &mut true_arm.arm_selector);
            }
        }
    }
}
