#[cfg(test)]
#[path = "scrub_units_test.rs"]
mod test;

use cairo_lang_semantic::corelib;
use salsa::Database;

use crate::{
    BlockEnd, Lowered, Statement, StatementCall, StatementStructConstruct,
    StatementStructDestructure,
};

/// Removes unit values from returns and call statements.
pub fn scrub_units(db: &dyn Database, lowered: &mut Lowered<'_>) {
    if lowered.blocks.is_empty() {
        return;
    }

    let unit_ty = corelib::unit_ty(db);

    let mut fixes = vec![];
    for block in lowered.blocks.iter_mut() {
        for (idx, stmt) in block.statements.iter_mut().enumerate() {
            let Statement::Call(StatementCall { function, outputs, .. }) = stmt else {
                continue;
            };

            // Unit scrubbing is only valid for user functions.
            if function.body(db).unwrap().is_none() {
                continue;
            }

            if lowered.variables[*outputs.last().unwrap()].ty == unit_ty {
                fixes.push((idx, outputs.pop().unwrap()));
            }
        }

        for (idx, output) in fixes.drain(..).rev() {
            block.statements.insert(
                idx + 1,
                Statement::StructConstruct(StatementStructConstruct { inputs: vec![], output }),
            )
        }

        if let BlockEnd::Return(ref mut inputs, _location) = block.end
            && let Some(return_val) = inputs.last()
            && lowered.variables[return_val.var_id].ty == unit_ty
        {
            block.statements.push(Statement::StructDestructure(StatementStructDestructure {
                input: inputs.pop().unwrap(),
                outputs: vec![],
            }));
        };
    }
}
