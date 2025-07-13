#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;

use crate::db::SierraGenGroup;

/// Query implementation of [SierraGenGroup::get_ap_change].
pub fn get_ap_change(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<SierraApChange> {
    // The implementation of get_ap_change() may call this function recursively. To guarantee no
    // salsa query cycles are created, we first verify that there are no cycles.
    if db.final_contains_call_cycle(function_id)? {
        return Ok(SierraApChange::Unknown);
    }

    let known_ap_change = db.priv_function_with_body_sierra_data(function_id)?.known_ap_change;
    Ok(match known_ap_change {
        true => SierraApChange::Known { new_vars_only: false },
        false => SierraApChange::Unknown,
    })
}
