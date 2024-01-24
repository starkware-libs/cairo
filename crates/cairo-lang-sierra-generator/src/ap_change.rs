#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_lowering::DependencyType;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::program::GenStatement;

use crate::db::SierraGenGroup;
use crate::pre_sierra;
use crate::utils::get_libfunc_signature;

/// Query implementation of [SierraGenGroup::get_ap_change].
pub fn get_ap_change(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<SierraApChange> {
    // The implementation of get_ap_change() may call this function recursively. To guarantee no
    // salsa query cycles are created, we first verify that there are no cycles.
    if db.contains_cycle(function_id, DependencyType::Call)? {
        return Ok(SierraApChange::Unknown);
    }

    let function = &*db.function_with_body_sierra(function_id)?;
    for statement in &function.body {
        if let pre_sierra::Statement::Sierra(GenStatement::Invocation(invocation)) =
            &statement.statement
        {
            let signature = get_libfunc_signature(db, invocation.libfunc_id.clone());
            // Go over the branches.
            for branch_signature in signature.branch_signatures {
                if matches!(branch_signature.ap_change, SierraApChange::Unknown) {
                    return Ok(SierraApChange::Unknown);
                }
            }
        }
    }
    Ok(SierraApChange::Known { new_vars_only: false })
}
