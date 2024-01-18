#[cfg(test)]
#[path = "scrub_units_test.rs"]
mod test;

use cairo_lang_semantic::corelib;

use crate::db::LoweringGroup;
use crate::{FlatBlockEnd, FlatLowered, Statement, StatementCall};

/// Removes unit values from returns and call statements.
pub fn scrub_units(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }

    let unit_ty = corelib::unit_ty(db.upcast());

    for block in lowered.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            let Statement::Call(StatementCall { function, outputs, .. }) = stmt else {
                continue;
            };

            // Unit scrubbing is only valid for user functions.
            if function.body(db).unwrap().is_none() {
                continue;
            }

            if lowered.variables[*outputs.last().unwrap()].ty == unit_ty {
                outputs.pop();
            }
        }

        if let FlatBlockEnd::Return(ref mut inputs) = block.end {
            if let Some(return_val) = inputs.last() {
                if lowered.variables[return_val.var_id].ty == unit_ty {
                    inputs.pop();
                }
            }
        };
    }
}
