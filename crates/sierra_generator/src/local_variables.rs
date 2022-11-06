#[cfg(test)]
#[path = "local_variables_test.rs"]
mod test;

use lowering::lower::Lowered;
use lowering::VariableId;
use utils::ordered_hash_set::OrderedHashSet;

use crate::db::SierraGenGroup;

/// Given the lowering of a function, returns the set of variables which should be stored as local
/// variables.
#[allow(dead_code)]
fn find_local_variables(
    _db: &dyn SierraGenGroup,
    _lowered_function: &Lowered,
) -> Option<OrderedHashSet<VariableId>> {
    Some(OrderedHashSet::default())
}
