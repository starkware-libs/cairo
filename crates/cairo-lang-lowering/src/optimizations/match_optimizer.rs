#[cfg(test)]
#[path = "match_optimizer_test.rs"]
mod test;

use cairo_lang_semantic::MatchArmSelector;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::{Itertools, zip_eq};

use super::var_renamer::VarRenamer;
use crate::borrow_check::Demand;
use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::borrow_check::demand::EmptyDemandReporter;
use crate::db::LoweringGroup;
use crate::utils::RebuilderEx;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchEnumInfo, MatchInfo, Statement,
    StatementEnumConstruct, StatementStructConstruct, VarRemapping, VarUsage, Variable, VariableId,
};

pub type MatchOptimizerDemand = Demand<VariableId, (), ()>;

impl MatchOptimizerDemand {
    fn update(&mut self, statement: &Statement) {
        self.variables_introduced(&mut EmptyDemandReporter {}, statement.outputs(), ());
        self.variables_used(
            &mut EmptyDemandReporter {},
            statement.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
    }
}

/// Optimizes Statement::EnumConstruct that is followed by a match to jump to the target of the
/// relevant match arm.
///
/// For example, given:
///
/// ```plain
/// blk0:
/// Statements:
/// (v1: core::option::Option::<core::integer::u32>) <- Option::Some(v0)
/// End:
/// Goto(blk1, {v1-> v2})
///
/// blk1:
/// Statements:
/// End:
/// Match(match_enum(v2) {
///   Option::Some(v3) => blk4,
///   Option::None(v4) => blk5,
/// })
/// ```
///
/// Change `blk0` to jump directly to `blk4`.
pub fn optimize_matches(_db: &dyn LoweringGroup, lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }
    let ctx = MatchOptimizerContext { fixes: vec![] };
    let mut analysis = BackAnalysis::new(lowered, ctx);
    analysis.get_root_info();
    let ctx = analysis.analyzer;

    let mut new_blocks = vec![];
    let mut next_block_id = BlockId(lowered.blocks.len());

    // Track variable renaming that results from applying the fixes below.
    // For each (variable_id, arm_idx) pair that is remapped (prior to the match),
    // we assign a new variable (to satisfy the SSA requirement).
    //
    // For example, consider the following blocks:
    //   blk0:
    //   Statements:
    //   (v0: test::Color) <- Color::Red(v5)
    //   End:
    //   Goto(blk1, {v1 -> v2, v0 -> v3})
    //
    //   blk1:
    //   Statements:
    //   End:
    //   Match(match_enum(v3) {
    //     Color::Red(v4) => blk2,
    //   })
    //
    // When the optimization is applied, block0 will jump directly to blk2. Since the definition of
    // v2 is at blk1, we must map v1 to a new variable.
    //
    // If there is another fix for the same match arm, the same variable will be used.
    let mut var_renaming = UnorderedHashMap::<(VariableId, usize), VariableId>::default();

    // Fixes were added in reverse order and need to be applied in that order.
    // This is because `additional_remapping` in later blocks may need to be renamed by fixes from
    // earlier blocks.
    for FixInfo {
        statement_location,
        match_block,
        arm_idx,
        target_block,
        remapping,
        reachable_blocks,
        additional_remapping,
        n_same_block_statement,
        remove_enum_construct,
        additional_stmts,
    } in ctx.fixes
    {
        // Choose new variables for each destination of the additional remappings (see comment
        // above).
        let mut new_remapping = remapping.clone();
        let mut renamed_vars = OrderedHashMap::<VariableId, VariableId>::default();
        for (var, dst) in additional_remapping.iter() {
            // Allocate a new variable, if it was not allocated before.
            let new_var = *var_renaming
                .entry((*var, arm_idx))
                .or_insert_with(|| lowered.variables.alloc(lowered.variables[*var].clone()));
            new_remapping.insert(new_var, *dst);
            renamed_vars.insert(*var, new_var);
        }

        let block = &mut lowered.blocks[statement_location.0];
        assert_eq!(
            block.statements.len() - 1,
            statement_location.1 + n_same_block_statement,
            "Unexpected number of statements in block."
        );

        if remove_enum_construct {
            block.statements.remove(statement_location.1);
        }

        process_additional_statements(
            &mut lowered.variables,
            &mut var_renaming,
            arm_idx,
            &additional_remapping,
            additional_stmts,
            &mut new_remapping,
            &mut renamed_vars,
            block,
        );

        block.end = FlatBlockEnd::Goto(target_block, new_remapping);
        if statement_location.0 == match_block {
            // The match was removed (by the assignment of `block.end` above), no need to fix it.
            // Sanity check: there should be no additional remapping in this case.
            assert!(additional_remapping.remapping.is_empty());
            continue;
        }

        let block = &mut lowered.blocks[match_block];
        let FlatBlockEnd::Match { info: MatchInfo::Enum(MatchEnumInfo { arms, location, .. }) } =
            &mut block.end
        else {
            unreachable!("match block should end with a match.");
        };

        let arm = arms.get_mut(arm_idx).unwrap();
        if target_block != arm.block_id {
            // The match arm was already fixed, no need to fix it again.
            continue;
        }

        // Fix match arm not to jump directly to a block that has an incoming gotos and add
        // remapping that matches the goto above.
        let arm_var = arm.var_ids.get_mut(0).unwrap();
        let orig_var = *arm_var;
        *arm_var = lowered.variables.alloc(lowered.variables[orig_var].clone());
        let mut new_block_remapping: VarRemapping = Default::default();
        new_block_remapping.insert(orig_var, VarUsage { var_id: *arm_var, location: *location });
        for (var, new_var) in renamed_vars.iter() {
            new_block_remapping.insert(*new_var, VarUsage { var_id: *var, location: *location });
        }

        new_blocks.push(FlatBlock {
            statements: vec![],
            end: FlatBlockEnd::Goto(arm.block_id, new_block_remapping),
        });
        arm.block_id = next_block_id;
        next_block_id = next_block_id.next_block_id();

        let mut var_renamer = VarRenamer { renamed_vars: renamed_vars.into_iter().collect() };
        // Apply the variable renaming to the reachable blocks.
        for block_id in reachable_blocks {
            let block = &mut lowered.blocks[block_id];
            *block = var_renamer.rebuild_block(block);
        }
    }

    for block in new_blocks {
        lowered.blocks.push(block);
    }
}

#[allow(clippy::too_many_arguments)]
fn process_additional_statements(
    variables: &mut Arena<Variable>,
    var_renaming: &mut UnorderedHashMap<(VariableId, usize), VariableId>,
    arm_idx: usize,
    additional_remapping: &VarRemapping,
    stmts: Vec<Statement>,
    new_remapping: &mut VarRemapping,
    renamed_vars: &mut OrderedHashMap<VariableId, VariableId>,
    block: &mut FlatBlock,
) {
    if stmts.is_empty() {
        return;
    }

    // Maps input in the original lowering to the inputs after the optimization.
    // Since the statement are copied from after `additional_remapping` to before it,
    // `inputs_remapping` is initialized with `additional_remapping`.
    let mut inputs_remapping = UnorderedHashMap::<VariableId, VariableId>::from_iter(
        additional_remapping.iter().map(|(k, v)| (*k, v.var_id)),
    );
    for mut stmt in stmts {
        let (output, inputs) = match &mut stmt {
            Statement::EnumConstruct(StatementEnumConstruct { output, input, .. }) => {
                (output, std::slice::from_mut(input))
            }
            Statement::StructConstruct(StatementStructConstruct { output, inputs }) => {
                (output, inputs.as_mut_slice())
            }
            _ => unreachable!("Only EnumConstruct and StructConstruct are supported."),
        };

        for input in inputs.iter_mut() {
            if let Some(orig_var) = inputs_remapping.get(&input.var_id) {
                input.var_id = *orig_var;
            }
        }

        // Allocate a new output, if it was not allocated before.
        let new_output = *var_renaming
            .entry((*output, arm_idx))
            .or_insert_with(|| variables.alloc(variables[*output].clone()));

        renamed_vars.insert(*output, new_output);

        let orig_output = *output;
        *output = variables.alloc(variables[*output].clone());

        inputs_remapping.insert(orig_output, *output);
        let location = variables[*output].location;
        new_remapping.insert(new_output, VarUsage { var_id: *output, location });

        block.statements.push(stmt);
    }
}

/// Returns a [FixInfo] object if the statement can be optimized out and None otherwise.
fn statement_can_be_optimized_out(
    stmt: &Statement,
    info: &mut AnalysisInfo<'_>,
    candidate: &mut OptimizationCandidate<'_>,
    statement_location: (BlockId, usize),
) -> Option<FixInfo> {
    let Statement::EnumConstruct(StatementEnumConstruct { variant, input, output }) = stmt else {
        return None;
    };
    if *output != candidate.match_variable {
        return None;
    }

    let (arm_idx, arm) = candidate
        .match_arms
        .iter()
        .find_position(
            |arm| matches!(&arm.arm_selector, MatchArmSelector::VariantId(v) if v == variant),
        )
        .expect("arm not found.");

    let [var_id] = arm.var_ids.as_slice() else {
        panic!("An arm of an EnumMatch should produce a single variable.");
    };

    // Prepare a remapping object for the input of the EnumConstruct, which will be used as `var_id`
    // in `arm.block_id`.
    let mut remapping = VarRemapping::default();
    remapping.insert(*var_id, *input);

    // Compute the demand based on the demand of the specific arm, rather than the current demand
    // (which contains the union of the demands from all the arms).
    // Apply the remapping of the input variable and the additional remappings if exist.
    let mut demand = std::mem::take(&mut candidate.arm_demands[arm_idx]);
    demand
        .apply_remapping(&mut EmptyDemandReporter {}, [(var_id, (&input.var_id, ()))].into_iter());

    let additional_stmts =  candidate.statement_rev.iter().rev().skip(candidate.n_same_block_statement).cloned().cloned().collect_vec();
    for stmt in &additional_stmts {
        // TODO(ilya): Support other statements.
        if !matches!(stmt, Statement::EnumConstruct(_) | Statement::StructConstruct(_)) {
            return None;
        }
        demand.update(stmt);
    }

    if let Some(additional_remappings) = &candidate.additional_remappings {
        demand.apply_remapping(
            &mut EmptyDemandReporter {},
            additional_remappings
                .iter()
                .map(|(dst, src_var_usage)| (dst, (&src_var_usage.var_id, ()))),
        );
    }

    for stmt in candidate.statement_rev.iter().rev().take(candidate.n_same_block_statement) {
        demand.update(stmt);
    }
    info.demand = demand;
    info.reachable_blocks = std::mem::take(&mut candidate.arm_reachable_blocks[arm_idx]);

    Some(FixInfo {
        statement_location,
        match_block: candidate.match_block,
        arm_idx,
        target_block: arm.block_id,
        remapping,
        reachable_blocks: info.reachable_blocks.clone(),
        additional_remapping: std::mem::take(&mut candidate.additional_remappings)
            .unwrap_or_default(),
        n_same_block_statement: candidate.statement_rev.len(),
        remove_enum_construct: !info.demand.vars.contains_key(output),
        additional_stmts,
    })
}

pub struct FixInfo {
    /// The location that needs to be fixed,
    statement_location: (BlockId, usize),
    /// The block with the match statement that we want to jump over.
    match_block: BlockId,
    /// The index of the arm that we want to jump to.
    arm_idx: usize,
    /// The target block to jump to.
    target_block: BlockId,
    /// The variable remapping that should be applied.
    remapping: VarRemapping,
    /// The blocks that can be reached from the relevant arm of the match.
    reachable_blocks: OrderedHashSet<BlockId>,
    /// Additional remappings that appeared in a `Goto` leading to the match.
    additional_remapping: VarRemapping,
    /// The number of statement in the in the same block as the enum construct.
    n_same_block_statement: usize,
    /// Indicated that the enum construct statement can be removed.
    remove_enum_construct: bool,
    /// Additional statement that appear before the match but not in the same block as the enum construct.
    additional_stmts: Vec<Statement>,
}

#[derive(Clone)]
struct OptimizationCandidate<'a> {
    /// The variable that is matched.
    match_variable: VariableId,

    /// The match arms of the extern match that we are optimizing.
    match_arms: &'a [MatchArm],

    /// The block that the match is in.
    match_block: BlockId,

    /// The demands at the arms.
    arm_demands: Vec<MatchOptimizerDemand>,

    /// Whether there is a future merge between the match arms.
    future_merge: bool,

    /// The blocks that can be reached from each of the arms.
    arm_reachable_blocks: Vec<OrderedHashSet<BlockId>>,

    /// Additional remappings that appeared in a `Goto` leading to the match.
    additional_remappings: Option<VarRemapping>,

    /// The statements before the match in reverse order.
    statement_rev: Vec<&'a Statement>,

    /// The number of statement in the in the same block as the enum construct.
    n_same_block_statement: usize,
}

pub struct MatchOptimizerContext {
    fixes: Vec<FixInfo>,
}

#[derive(Clone)]
pub struct AnalysisInfo<'a> {
    candidate: Option<OptimizationCandidate<'a>>,
    demand: MatchOptimizerDemand,
    /// Blocks that can be reached from the current block.
    reachable_blocks: OrderedHashSet<BlockId>,
}

impl<'a> Analyzer<'a> for MatchOptimizerContext {
    type Info = AnalysisInfo<'a>;

    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &FlatBlock) {
        info.reachable_blocks.insert(block_id);
    }

    fn visit_stmt(
        &mut self,
        info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement,
    ) {
        if let Some(mut candidate) = std::mem::take(&mut info.candidate) {
            if let Some(fix_info) =
                statement_can_be_optimized_out(stmt, info, &mut candidate, statement_location)
            {
                self.fixes.push(fix_info);
                return;
            }

            candidate.statement_rev.push(stmt);
            candidate.n_same_block_statement += 1;
            info.candidate = Some(candidate);
        }

        info.demand.update(stmt);
    }

    fn visit_goto(
        &mut self,
        info: &mut Self::Info,
        _statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        info.demand.apply_remapping(
            &mut EmptyDemandReporter {},
            remapping.iter().map(|(dst, src)| (dst, (&src.var_id, ()))),
        );

        let Some(ref mut candidate) = &mut info.candidate else {
            return;
        };
        candidate.n_same_block_statement = 0;

     
        let orig_match_variable = candidate.match_variable;

        // The term 'additional_remappings' refers to remappings for variables other than the match
        // variable.
        let goto_has_additional_remappings =
            if let Some(var_usage) = remapping.get(&candidate.match_variable) {
                candidate.match_variable = var_usage.var_id;
                remapping.len() > 1
            } else {
                // Note that remapping.is_empty() is false here.
                true
            };

        if goto_has_additional_remappings {
            // here, we have remappings for variables other than the match variable.

            if candidate.future_merge || candidate.additional_remappings.is_some() || !candidate.statement_rev.is_empty() {
                // TODO(ilya): Support multiple remappings with future merges.

                // Revoke the candidate.
                info.candidate = None;
            } else {
                // Store the goto's remapping, except for the match variable.
                candidate.additional_remappings = Some(VarRemapping {
                    remapping: remapping
                        .iter()
                        .filter_map(|(var, dst)| {
                            if *var != orig_match_variable { Some((*var, *dst)) } else { None }
                        })
                        .collect(),
                });
            }
        }
    }

    fn merge_match(
        &mut self,
        (block_id, _statement_idx): StatementLocation,
        match_info: &'a MatchInfo,
        infos: impl Iterator<Item = Self::Info>,
    ) -> Self::Info {
        let (arm_demands, arm_reachable_blocks): (Vec<_>, Vec<_>) =
            infos.map(|info| (info.demand, info.reachable_blocks)).unzip();

        let arm_demands_without_arm_var = zip_eq(match_info.arms(), &arm_demands)
            .map(|(arm, demand)| {
                let mut demand = demand.clone();
                // Remove the variable that is introduced by the match arm.
                demand.variables_introduced(&mut EmptyDemandReporter {}, &arm.var_ids, ());

                (demand, ())
            })
            .collect_vec();
        let mut demand = MatchOptimizerDemand::merge_demands(
            &arm_demands_without_arm_var,
            &mut EmptyDemandReporter {},
        );

        // Union the reachable blocks for all the infos.
        let mut reachable_blocks = OrderedHashSet::default();
        let mut max_possible_size = 0;
        for cur_reachable_blocks in &arm_reachable_blocks {
            reachable_blocks.extend(cur_reachable_blocks.iter().cloned());
            max_possible_size += cur_reachable_blocks.len();
        }
        // If the size of `reachable_blocks` is less than the sum of the sizes of the
        // `arm_reachable_blocks`, then there was a collision.
        let found_collision = reachable_blocks.len() < max_possible_size;

        let candidate = match match_info {
            // A match is a candidate for the optimization if it is a match on an Enum
            // and its input is unused after the match.
            MatchInfo::Enum(MatchEnumInfo { input, arms, .. })
                if !demand.vars.contains_key(&input.var_id) =>
            {
                Some(OptimizationCandidate {
                    match_variable: input.var_id,
                    match_arms: arms,
                    match_block: block_id,
                    arm_demands,
                    future_merge: found_collision,
                    arm_reachable_blocks,
                    additional_remappings: None,
                    statement_rev: vec![],
                    n_same_block_statement: 0,
                })
            }
            _ => None,
        };

        demand.variables_used(
            &mut EmptyDemandReporter {},
            match_info.inputs().iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );

        Self::Info { candidate, demand, reachable_blocks }
    }

    fn info_from_return(
        &mut self,
        _statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        let mut demand = MatchOptimizerDemand::default();
        demand.variables_used(
            &mut EmptyDemandReporter {},
            vars.iter().map(|VarUsage { var_id, .. }| (var_id, ())),
        );
        Self::Info { candidate: None, demand, reachable_blocks: Default::default() }
    }
}
