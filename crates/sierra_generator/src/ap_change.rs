#[cfg(test)]
#[path = "ap_change_test.rs"]
mod test;

use defs::ids::FreeFunctionId;
use lowering::lower::Lowered;
use lowering::BlockId;

use crate::db::SierraGenGroup;

/// Query implementation of [SierraGenGroup::contains_cycle].
pub fn contains_cycle(db: &dyn SierraGenGroup, function_id: FreeFunctionId) -> Option<bool> {
    let lowered_function = &*db.free_function_lowered(function_id)?;
    contains_call_to_cyclic_function(db, lowered_function, lowered_function.root?)
}

/// Cycle handling for [SierraGenGroup::contains_cycle].
pub fn contains_cycle_handle_cycle(
    _db: &dyn SierraGenGroup,
    _cycle: &Vec<String>,
    _function_id: &FreeFunctionId,
) -> Option<bool> {
    Some(true)
}

/// Helper function for [contains_cycle]. Returns `true` if the block contains a call to a function
/// that contains a cycle.
fn contains_call_to_cyclic_function(
    db: &dyn SierraGenGroup,
    lowered_function: &Lowered,
    block_id: BlockId,
) -> Option<bool> {
    let block = &lowered_function.blocks[block_id];
    for statement in &block.statements {
        match statement {
            lowering::Statement::Call(statement_call) => {
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
            lowering::Statement::CallBlock(statement_call_block) => {
                if contains_call_to_cyclic_function(
                    db,
                    lowered_function,
                    statement_call_block.block,
                )? {
                    return Some(true);
                }
            }
            lowering::Statement::MatchExtern(statement_match_extern) => {
                for arm_block_id in &statement_match_extern.arms {
                    if contains_call_to_cyclic_function(db, lowered_function, *arm_block_id)? {
                        return Some(true);
                    }
                }
            }
            lowering::Statement::MatchEnum(statement_match_enum) => {
                for (_, arm_block_id) in &statement_match_enum.arms {
                    if contains_call_to_cyclic_function(db, lowered_function, *arm_block_id)? {
                        return Some(true);
                    }
                }
            }
            lowering::Statement::Literal(_)
            | lowering::Statement::EnumConstruct(_)
            | lowering::Statement::StructConstruct
            | lowering::Statement::StructDestruct
            | lowering::Statement::TupleConstruct(_)
            | lowering::Statement::TupleDestruct(_) => {}
        };
    }

    return Some(false);
}
