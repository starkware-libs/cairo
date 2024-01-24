#[cfg(test)]
#[path = "cancel_ops_test.rs"]
mod test;

use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{izip, zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::DemandReporter;
use crate::borrow_check::Demand;
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{BlockId, FlatLowered, MatchInfo, Statement, VarRemapping, VarUsage, VariableId};

pub type MatchOptimizerDemand = Demand<VariableId, StatementLocation, ()>;

impl DemandReporter<VariableId> for CancelOpsContext<'_> {
    type IntroducePosition = ();
    type UsePosition = StatementLocation;

    fn last_use(&mut self, position: StatementLocation, var: VariableId) {
        self.next_use.insert(var, position);
    }
}

/// Cancels out a StructConstruct followed by a StructDestructure.
/// The optimizaion can be applied when the StructConstruct output is duplicatable or
/// used only by the struct destructure.
pub fn cancel_ops(lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }
    let ctx = CancelOpsContext {
        lowered,
        next_use: Default::default(),
        renamed_vars: Default::default(),
        stmts_to_remove: vec![],
    };
    let mut analysis =
        BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: ctx };
    analysis.get_root_info();
    let mut ctx = analysis.analyzer;

    let mut rebuilder = CancelOpsRebuilder { renamed_vars: ctx.renamed_vars };

    // Remove no-longer needed statements.
    ctx.stmts_to_remove.sort_by_key(|(block_id, stmt_id)| (block_id.0, *stmt_id));
    for (block_id, stmt_id) in ctx.stmts_to_remove.into_iter().rev() {
        lowered.blocks[block_id].statements.remove(stmt_id);
    }

    // Rebuild the blocks with the new variable names.
    for block in lowered.blocks.iter_mut() {
        *block = rebuilder.rebuild_block(block);
    }
}

pub struct CancelOpsContext<'a> {
    lowered: &'a FlatLowered,

    // Maps a variable to the next use of the variable.
    // Note that a remmaping is cosidered as usage here.
    next_use: UnorderedHashMap<VariableId, StatementLocation>,

    // Maps a variable to the variable that it was renamed to.
    renamed_vars: UnorderedHashMap<VariableId, VariableId>,

    // Statements that can be be removed.
    stmts_to_remove: Vec<StatementLocation>,
}

#[derive(Clone)]
pub struct AnalysisInfo {
    demand: MatchOptimizerDemand,
}
impl<'a> CancelOpsContext<'a> {
    fn handle_stmt(&mut self, stmt: &'a Statement, _statement_location: StatementLocation) {
        let Statement::StructConstruct(stmt) = stmt else {
            return;
        };

        let Some(next_use) = self.next_use.get(&stmt.output) else {
            return;
        };
        let Some(Statement::StructDestructure(destructure_stmt)) =
            self.lowered.blocks[next_use.0].statements.get(next_use.1)
        else {
            return;
        };

        // Mark the statements for removal and set the renaming for it outputs.
        self.stmts_to_remove.push(*next_use);
        for (output, input) in izip!(destructure_stmt.outputs.iter(), stmt.inputs.iter()) {
            assert!(
                self.renamed_vars.insert(*output, input.var_id).is_none(),
                "Variable {:?} was already renamed",
                output
            );
        }
    }
}

impl<'a> Analyzer<'a> for CancelOpsContext<'a> {
    type Info = AnalysisInfo;

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement,
    ) {
        self.handle_stmt(stmt, statement_location);
        info.demand.variables_introduced(self, &stmt.outputs(), ());
        info.demand.variables_used(
            self,
            stmt.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, statement_location)),
        );
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        info.demand.apply_remapping(
            self,
            remapping.iter().map(|(dst, src)| (dst, (&src.var_id, statement_location))),
        );
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo,
        infos: &[Self::Info],
    ) -> Self::Info {
        let arm_demands = zip_eq(match_info.arms(), infos)
            .map(|(arm, info)| {
                let mut demand = info.demand.clone();
                demand.variables_introduced(self, &arm.var_ids, ());

                (demand, ())
            })
            .collect_vec();
        let mut demand = MatchOptimizerDemand::merge_demands(&arm_demands, self);

        demand.variables_used(
            self,
            match_info.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, statement_location)),
        );

        Self::Info { demand }
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut demand = MatchOptimizerDemand::default();
        demand.variables_used(
            self,
            vars.iter().map(|VarUsage { var_id, .. }| (var_id, statement_location)),
        );
        Self::Info { demand }
    }

    fn info_from_panic(
        &mut self,
        statement_location: StatementLocation,
        data: &VarUsage,
    ) -> Self::Info {
        let mut demand = MatchOptimizerDemand::default();
        demand.variables_used(self, std::iter::once((&data.var_id, statement_location)));
        Self::Info { demand }
    }
}

pub struct CancelOpsRebuilder {
    renamed_vars: UnorderedHashMap<VariableId, VariableId>,
}

impl Rebuilder for CancelOpsRebuilder {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        self.renamed_vars.get(&var).copied().unwrap_or(var)
    }

    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        block
    }
}
