#[cfg(test)]
#[path = "reorder_statements_test.rs"]
mod test;

use std::cmp::Reverse;

use cairo_lang_semantic::corelib;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::Itertools;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::db::LoweringGroup;
use crate::ids::{FunctionId, FunctionLongId};
use crate::{
    BlockId, FlatLowered, MatchInfo, Statement, StatementCall, VarRemapping, VarUsage, VariableId,
};

/// Reorder the statements in the lowering in order to move variable definitions closer to their
/// usage. Statement with no side effects and unused outputs are removed.
///
/// The list of call statements that can be moved is currently hardcoded.
///
/// Removing unnecessary remapping before this optimization will result in better code.
pub fn reorder_statements(db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let semantic_db = db.upcast();
        let bool_not_func_id = db.intern_lowering_function(FunctionLongId::Semantic(
            corelib::get_core_function_id(semantic_db, "bool_not_impl".into(), vec![]),
        ));

        let ctx = ReorderStatementsContext {
            lowered: &*lowered,
            moveable_functions: [bool_not_func_id].into_iter().collect(),
            statement_to_move: vec![],
        };
        let mut analysis =
            BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: ctx };
        analysis.get_root_info();
        let ctx = analysis.analyzer;

        let mut changes_by_block =
            OrderedHashMap::<BlockId, Vec<(usize, Option<Statement>)>>::default();

        for (src, opt_dst) in ctx.statement_to_move.into_iter() {
            changes_by_block.entry(src.0).or_insert_with(Vec::new).push((src.1, None));

            if let Some(dst) = opt_dst {
                let statement = lowered.blocks[src.0].statements[src.1].clone();
                changes_by_block
                    .entry(dst.0)
                    .or_insert_with(Vec::new)
                    .push((dst.1, Some(statement)));
            }
        }

        for (block_id, block_changes) in changes_by_block.into_iter() {
            let statements = &mut lowered.blocks[block_id].statements;

            // Apply block changes in reverse order to prevent a change from invalidating the
            // indices of the other changes.
            for (index, opt_statement) in
                block_changes.into_iter().sorted_by_key(|(index, _)| Reverse(*index))
            {
                match opt_statement {
                    Some(stmt) => statements.insert(index, stmt),
                    None => {
                        statements.remove(index);
                    }
                }
            }
        }
    }
}

#[derive(Clone, Default)]
pub struct ReorderStatementsInfo {
    // A mapping from var_id to a candidate location that it can be moved to.
    // If the variable is used in multiple match arms we define the next use to be
    // the match.
    next_use: OrderedHashMap<VariableId, StatementLocation>,
}

pub struct ReorderStatementsContext<'a> {
    lowered: &'a FlatLowered,
    // A list of function that can be moved.
    moveable_functions: UnorderedHashSet<FunctionId>,
    statement_to_move: Vec<(StatementLocation, Option<StatementLocation>)>,
}
impl ReorderStatementsContext<'_> {
    fn call_can_be_moved(&mut self, stmt: &StatementCall) -> bool {
        self.moveable_functions.contains(&stmt.function)
    }
}
impl Analyzer<'_> for ReorderStatementsContext<'_> {
    type Info = ReorderStatementsInfo;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        let mut immovable = matches!(stmt, Statement::Call(stmt) if !self.call_can_be_moved(stmt));
        let mut optional_target_location = None;
        for var_to_move in stmt.outputs() {
            let Some((block_id, index)) = info.next_use.swap_remove(&var_to_move) else { continue };
            if let Some((target_block_id, target_index)) = &mut optional_target_location {
                *target_index = std::cmp::min(*target_index, index);
                // If the output is used in multiple places we can't move their creation point.
                immovable |= target_block_id != &block_id;
            } else {
                optional_target_location = Some((block_id, index));
            }
        }
        if immovable {
            for var_usage in stmt.inputs() {
                info.next_use.insert(var_usage.var_id, statement_location);
            }
            return;
        }

        if let Some(target_location) = optional_target_location {
            // If the statement is not removed add demand for its inputs.
            for var_usage in stmt.inputs() {
                match info.next_use.entry(var_usage.var_id) {
                    indexmap::map::Entry::Occupied(mut e) => {
                        // Since we don't know where `e.get()` and `target_location` converge
                        // we use `statement_location` as a conservative estimate.
                        &e.insert(statement_location)
                    }
                    indexmap::map::Entry::Vacant(e) => e.insert(target_location),
                };
            }
        }

        let is_simple_move = optional_target_location.is_some();
        // If a movable statement is unused, and all its inputs are droppable removing it is valid.
        let is_removal_valid =
            || stmt.inputs().iter().all(|v| self.lowered.variables[v.var_id].droppable.is_ok());
        if is_simple_move || is_removal_valid() {
            self.statement_to_move.push((statement_location, optional_target_location));
        }
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        for VarUsage { var_id, .. } in remapping.values() {
            info.next_use.insert(*var_id, statement_location);
        }
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let mut info = Self::Info::default();

        for arm_info in infos {
            for (var_id, location) in arm_info.next_use.iter() {
                match info.next_use.entry(*var_id) {
                    indexmap::map::Entry::Occupied(mut e) => {
                        // A variable that is used in multiple arms can be moved to
                        // before the match.
                        e.insert(statement_location);
                    }
                    indexmap::map::Entry::Vacant(e) => {
                        e.insert(*location);
                    }
                }
            }
        }

        for var_usage in match_info.inputs() {
            info.next_use.insert(var_usage.var_id, statement_location);
        }

        info
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut info = Self::Info::default();
        for var_usage in vars {
            info.next_use.insert(var_usage.var_id, statement_location);
        }
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VarUsage,
    ) -> Self::Info {
        unreachable!("Panics should have been stripped in a previous phase.");
    }
}
