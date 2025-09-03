#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use salsa::Database;

use crate::db::SierraGenGroup;

/// Query implementation of [SierraGenGroup::get_ap_change].
#[salsa::tracked]
pub fn get_ap_change(
    db: &dyn Database,
    function_id: ConcreteFunctionWithBodyId<'_>,
) -> Maybe<SierraApChange> {
    // The implementation of get_ap_change() may call this function recursively. To guarantee no
    // salsa query cycles are created, we first verify that there are no cycles.
    if db.final_contains_call_cycle(function_id)? {
        return Ok(SierraApChange::Unknown);
    }

    Ok(db.priv_function_with_body_sierra_data(function_id)?.ap_change.clone())
}
