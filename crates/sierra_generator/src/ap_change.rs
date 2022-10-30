#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use defs::ids::FreeFunctionId;

use crate::db::SierraGenGroup;

/// Query implementation of [SierraGenGroup::contains_cycle].
pub fn contains_cycle(db: &dyn SierraGenGroup, function_id: FreeFunctionId) -> Option<bool> {
    let lowered_function = &*db.free_function_lowered(function_id)?;
    for (_, block) in &lowered_function.blocks {
        for statement in &block.statements {
            if let lowering::Statement::Call(statement_call) = statement {
                match db.lookup_intern_function(statement_call.function) {
                    semantic::FunctionLongId::Concrete(concrete) => {
                        match concrete.generic_function {
                            defs::ids::GenericFunctionId::Free(free_function_id) => {
                                if db.contains_cycle(free_function_id)? {
                                    return Some(true);
                                }
                            }
                            defs::ids::GenericFunctionId::Extern(_) => {}
                        }
                    }
                    semantic::FunctionLongId::Missing => {
                        return None;
                    }
                }
            }
        }
    }

    Some(false)
}

/// Cycle handling for [SierraGenGroup::contains_cycle].
pub fn contains_cycle_handle_cycle(
    _db: &dyn SierraGenGroup,
    _cycle: &[String],
    _function_id: &FreeFunctionId,
) -> Option<bool> {
    Some(true)
}
