#[cfg(test)]
#[path = "delay_var_def_test.rs"]
mod test;

use std::cmp::Reverse;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::{BlockId, FlatLowered, MatchInfo, Statement, VarRemapping, VariableId};

/// Moves var definitions closer to their usage point and removes unused var.
/// Currently only moves consts and empty structs.
/// Remove unnessary remapping before this optimization will result in better code.
pub fn delay_var_def(lowered: &mut FlatLowered) {
    if !lowered.blocks.is_empty() {
        let ctx = DelayDefsContext::default();
        let mut analysis =
            BackAnalysis { lowered: &*lowered, cache: Default::default(), analyzer: ctx };
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
            let statments = &mut lowered.blocks[block_id].statements;

            // Apply block changes in revese order to prevent a change from invalidating the
            // indices of the other changes.
            for (index, opt_statment) in
                block_changes.into_iter().sorted_by_key(|(index, _)| Reverse(*index))
            {
                match opt_statment {
                    Some(stmt) => statments.insert(index, stmt),
                    None => {
                        statments.remove(index);
                    }
                }
            }
        }
    }
}

#[derive(Clone, Default)]
pub struct DelayDefsInfo {
    // A mapping from var_id to a candidate location that it can be moved to.
    // If the variable is used in multiple match arms we define the next use to be
    // the match.
    next_use: OrderedHashMap<VariableId, StatementLocation>,
}

#[derive(Default)]
pub struct DelayDefsContext {
    statement_to_move: Vec<(StatementLocation, Option<StatementLocation>)>,
}
impl Analyzer<'_> for DelayDefsContext {
    type Info = DelayDefsInfo;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &Statement,
    ) {
        let var_to_move = match stmt {
            Statement::Literal(stmt) => stmt.output,
            Statement::StructConstruct(stmt) if stmt.inputs.is_empty() => stmt.output,
            _ => {
                for var_id in stmt.inputs() {
                    info.next_use.insert(var_id, statement_location);
                }
                return;
            }
        };

        self.statement_to_move.push((statement_location, info.next_use.swap_remove(&var_to_move)));
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        for var_id in remapping.values() {
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

        for var_id in match_info.inputs() {
            info.next_use.insert(var_id, statement_location);
        }

        info
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VariableId],
    ) -> Self::Info {
        let mut info = Self::Info::default();
        for var_id in vars {
            info.next_use.insert(*var_id, statement_location);
        }
        info
    }

    fn info_from_panic(
        &mut self,
        _statement_location: StatementLocation,
        _data: &VariableId,
    ) -> Self::Info {
        unreachable!("Panics should have been stripped in a previous phase.");
    }
}
