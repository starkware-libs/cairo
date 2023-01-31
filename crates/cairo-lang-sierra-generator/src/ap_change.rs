#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_semantic::items::functions::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::program::GenStatement;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::utils::get_libfunc_signature;

/// Query implementation of [SierraGenGroup::contains_cycle].
pub fn contains_cycle(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    let lowered_function = &*db.concrete_function_with_body_lowered(function_id)?;
    for (_, block) in &lowered_function.blocks {
        for statement in &block.statements {
            if let lowering::Statement::Call(statement_call) = statement {
                let concrete = db.lookup_intern_function(statement_call.function).function;
                if let Some(function_id) = concrete.get_body(db.upcast()) {
                    if db.contains_cycle(function_id)? {
                        return Ok(true);
                    }
                }
            }
        }
    }

    Ok(false)
}

/// Cycle handling for [SierraGenGroup::contains_cycle].
pub fn contains_cycle_handle_cycle(
    _db: &dyn SierraGenGroup,
    _cycle: &[String],
    _function_id: &ConcreteFunctionWithBodyId,
) -> Maybe<bool> {
    Ok(true)
}

/// Query implementation of [SierraGenGroup::get_ap_change].
pub fn get_ap_change(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<SierraApChange> {
    // The implementation of get_ap_change() may call this function recursively. To guarantee no
    // salsa query cycles are created, we first verify that there are no cycles.
    if db.contains_cycle(function_id)? {
        return Ok(SierraApChange::Unknown);
    }

    let function = &*db.function_with_body_sierra(function_id)?;
    for statement in &function.body {
        if let pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) = statement {
            let signature = get_libfunc_signature(db, invocation.libfunc_id.clone());
            // Go over the branches.
            for branch_signature in signature.branch_signatures {
                if matches!(
                    branch_signature.ap_change,
                    SierraApChange::Unknown | SierraApChange::NotImplemented
                ) {
                    return Ok(SierraApChange::Unknown);
                }
            }
        }
    }
    Ok(SierraApChange::Known { new_vars_only: false })
}
