#[cfg(test)]
#[path = "cancel_ops_test.rs"]
mod test;

use cairo_lang_utils::ordered_hash_map::{self, OrderedHashMap};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{izip, zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchInfo, Statement, StatementStructConstruct,
    VarRemapping, VarUsage, VariableId,
};

/// Cancels out a (StructConstruct, StructDestructure) and (Snap, Desnap) pair.
///
///
/// The algorithm is as follows:
/// Run backwards analysis with demand to find all the use sites.
/// When we reach the first item in the pair, check which statement can be removed and
/// construct the relevant `renamed_vars` mapping.
///
/// See CancelOpsContext::handle_stmt for more detail on when it is safe
/// to remove a statement.
pub fn cancel_ops(lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }
    let ctx = CancelOpsContext {
        lowered,
        use_sites: Default::default(),
        var_remapper: Default::default(),
        stmts_to_remove: vec![],
        vars_to_split: Default::default(),
    };
    let mut analysis =
        BackAnalysis { lowered: &*lowered, block_info: Default::default(), analyzer: ctx };
    analysis.get_root_info();

    let CancelOpsContext { mut var_remapper, mut stmts_to_remove, vars_to_split, .. } =
        analysis.analyzer;

    // Remove no-longer needed statements.
    stmts_to_remove.sort_by_key(|(block_id, stmt_id)| (block_id.0, *stmt_id));
    for (block_id, stmt_id) in stmts_to_remove.into_iter().rev() {
        lowered.blocks[block_id].statements.remove(stmt_id);
    }

    let mut safe_to_use = OrderedHashSet::<VariableId>::default();

    for block in lowered.blocks.iter_mut() {
        let old_statments = std::mem::take(&mut block.statements);

        for stmt in old_statments.into_iter() {
            match stmt {
                Statement::StructDestructure(stmt)
                    if vars_to_split.contains_key(&stmt.input.var_id) => {}
                Statement::StructConstruct(stmt) if vars_to_split.contains_key(&stmt.output) => {}
                _ => {
                    for input in stmt.inputs() {
                        if !safe_to_use.insert(input.var_id) {
                            continue;
                        }

                        if let Some(vars) = vars_to_split.get(&input.var_id) {
                            let location = input.location;

                            block.statements.push(Statement::StructConstruct(
                                StatementStructConstruct {
                                    inputs: vars
                                        .iter()
                                        .map(|var_id| VarUsage { var_id: *var_id, location })
                                        .collect_vec(),
                                    output: input.var_id,
                                },
                            ));
                        }
                    }
                    block.statements.push(stmt);
                }
            }
        }

        if let FlatBlockEnd::Goto(_, remappings) = &mut block.end {
            let mut new_remappings = VarRemapping::default();
            for (dst, src) in remappings.iter() {
                match (vars_to_split.get(dst), vars_to_split.get(&src.var_id)) {
                    (None, None) => {
                        new_remappings.insert(*dst, *src);
                    }
                    (Some(dst_vars), Some(src_vars)) => {
                        for (dst, new_src) in zip_eq(dst_vars, src_vars) {
                            new_remappings.insert(
                                *dst,
                                VarUsage { var_id: *new_src, location: src.location },
                            );
                        }
                    }
                    _ => panic!(""),
                }
            }
            *remappings = new_remappings;
        }
    }

    // Rebuild the blocks with the new variable names.
    for block in lowered.blocks.iter_mut() {
        *block = var_remapper.rebuild_block(block);
    }
}

pub struct CancelOpsContext<'a> {
    lowered: &'a FlatLowered,

    /// Maps a variable to the use sites of that variable.
    /// Note that a remapping is cosidered as usage here.
    use_sites: UnorderedHashMap<VariableId, Vec<StatementLocation>>,

    /// Maps a variable to the variable that it was renamed to.
    var_remapper: CancelOpsRebuilder,

    /// Statements that can be be removed.
    stmts_to_remove: Vec<StatementLocation>,

    vars_to_split: OrderedHashMap<VariableId, Vec<VariableId>>,
}

/// Returns the use sites of a variable.
///
/// Takes 'use_sites' map rather than `CancelOpsContext` to avoid borrowing the entire context.
fn get_use_sites<'a>(
    use_sites: &'a UnorderedHashMap<VariableId, Vec<StatementLocation>>,
    var: &VariableId,
) -> &'a [StatementLocation] {
    match use_sites.get(var) {
        Some(use_sites) => &use_sites[..],
        None => &[],
    }
}

impl<'a> CancelOpsContext<'a> {
    fn rename_var(&mut self, from: VariableId, to: VariableId) {
        assert!(
            self.var_remapper.renamed_vars.insert(from, to).is_none(),
            "Variable {:?} was already renamed",
            from
        );

        // Move `from` used sites to `to` to allow the optimization to be applied to them.
        if let Some(from_use_sites) = self.use_sites.remove(&from) {
            self.use_sites.entry(to).or_default().extend(from_use_sites);
        }
    }

    fn add_use_site(&mut self, var: VariableId, use_site: StatementLocation) {
        self.use_sites.entry(var).or_default().push(use_site);
    }

    /// Handles a statement and returns true if it can be removed.
    fn handle_stmt(&mut self, stmt: &'a Statement, statement_location: StatementLocation) -> bool {
        match stmt {
            Statement::StructDestructure(stmt) => {
                let mut use_sites = OrderedHashSet::<&StatementLocation>::default();

                for output in stmt.outputs.iter() {
                    let output_use_sites = get_use_sites(&self.use_sites, output);
                    use_sites.extend(output_use_sites);
                }

                let mut can_remove_struct_destructure = true;

                let constructs = use_sites
                    .iter()
                    .filter_map(|location| {
                        match self.lowered.blocks[location.0].statements.get(location.1) {
                            Some(Statement::StructConstruct(construct_stmt))
                                if stmt.outputs.len() == construct_stmt.inputs.len()
                                    && self.lowered.variables[stmt.input.var_id].ty
                                        == self.lowered.variables[construct_stmt.output].ty
                                    && zip_eq(
                                        stmt.outputs.iter(),
                                        construct_stmt.inputs.iter(),
                                    )
                                    .all(|(output, input)| {
                                        output == &self.var_remapper.map_var_id(input.var_id)
                                    }) =>
                            {
                                self.stmts_to_remove.push(**location);
                                Some(construct_stmt)
                            }
                            _ => {
                                can_remove_struct_destructure = false;
                                None
                            }
                        }
                    })
                    .collect_vec();

                if !(can_remove_struct_destructure
                    || self.lowered.variables[stmt.input.var_id].duplicatable.is_ok())
                {
                    // We can't remove any of of the construct statements.
                    self.stmts_to_remove.truncate(self.stmts_to_remove.len() - constructs.len());
                    return false;
                }

                // Mark the statements for removal and set the renaming for it outputs.
                if can_remove_struct_destructure {
                    self.stmts_to_remove.push(statement_location);
                }

                for construct in constructs {
                    self.rename_var(construct.output, stmt.input.var_id)
                }
                can_remove_struct_destructure
            }
            Statement::StructConstruct(stmt) => {
                let use_sites = get_use_sites(&self.use_sites, &stmt.output).to_vec();

                let mut vars_to_split = std::mem::take(&mut self.vars_to_split);

                let mut use_sites_stack =
                    use_sites.iter().map(|location| (*location, stmt.output)).collect_vec();

                while let Some((location, var_id)) = use_sites_stack.pop() {
                    eprintln!("Checking use site {:?} of {:?}", location, var_id);
                    let block = &self.lowered.blocks[location.0];
                    match (block.statements.get(location.1), &block.end) {
                        (Some(Statement::StructDestructure(destructure_stmt)), _) => {
                            match vars_to_split.entry(var_id) {
                                ordered_hash_map::Entry::Occupied(entry) => {
                                    for (output, input) in
                                        izip!(destructure_stmt.outputs.iter(), entry.get().iter())
                                    {
                                        eprintln!(
                                            "{:?}: Renaming {:?} -> {:?}",
                                            location, output, input
                                        );
                                        if output != input {
                                            self.rename_var(*output, *input);
                                        }
                                    }
                                }
                                ordered_hash_map::Entry::Vacant(entry) => {
                                    eprintln!(
                                        "{:?}: Adding {:?} to vars_to_split",
                                        location, destructure_stmt.input.var_id
                                    );
                                    entry.insert(destructure_stmt.outputs.clone());
                                }
                            };

                            // TODO, handle mid remapping. a -> b -> c

                            vars_to_split.entry(stmt.output).or_insert_with(|| {
                                eprintln!(
                                    "{:?}: Adding {:?} to vars_to_split",
                                    location, destructure_stmt.input.var_id
                                );
                                stmt.inputs.iter().map(|var_usage| var_usage.var_id).collect_vec()
                            });
                        }
                        (None, FlatBlockEnd::Goto(_, remappings)) => {
                            for (dst, src) in remappings.iter() {
                                if src.var_id == var_id {
                                    eprintln!("Found remapping {:?} -> {:?}", src.var_id, dst);
                                    use_sites_stack.extend(
                                        get_use_sites(&self.use_sites, dst)
                                            .iter()
                                            .map(|location| (*location, *dst)),
                                    )
                                }
                            }
                        }
                        _ => {}
                    }
                }

                self.vars_to_split = vars_to_split;
                true
            }
            Statement::Snapshot(stmt) => {
                let use_sites = get_use_sites(&self.use_sites, &stmt.output_snapshot);

                let mut can_remove_snap = true;
                let desnaps = use_sites
                    .iter()
                    .filter_map(|location| {
                        if let Some(Statement::Desnap(desnap_stmt)) =
                            self.lowered.blocks[location.0].statements.get(location.1)
                        {
                            self.stmts_to_remove.push(*location);
                            Some(desnap_stmt)
                        } else {
                            can_remove_snap = false;
                            None
                        }
                    })
                    .collect_vec();

                let new_var = if can_remove_snap {
                    self.stmts_to_remove.push(statement_location);
                    self.rename_var(stmt.output_original, stmt.input.var_id);
                    stmt.input.var_id
                } else if desnaps.is_empty()
                    && self.lowered.variables[stmt.input.var_id].duplicatable.is_err()
                {
                    stmt.output_original
                } else {
                    stmt.input.var_id
                };

                for desnap in desnaps {
                    self.rename_var(desnap.output, new_var);
                }
                can_remove_snap
            }
            _ => false,
        }
    }
}

impl<'a> Analyzer<'a> for CancelOpsContext<'a> {
    type Info = ();

    fn visit_stmt(
        &mut self,
        _info: &mut Self::Info,
        statement_location: StatementLocation,
        stmt: &'a Statement,
    ) {
        if !self.handle_stmt(stmt, statement_location) {
            for input in stmt.inputs() {
                self.add_use_site(input.var_id, statement_location);
            }
        }
    }

    fn visit_goto(
        &mut self,
        _info: &mut Self::Info,
        statement_location: StatementLocation,
        _target_block_id: BlockId,
        remapping: &VarRemapping,
    ) {
        for src in remapping.values() {
            self.add_use_site(src.var_id, statement_location);
        }
    }

    fn merge_match(
        &mut self,
        statement_location: StatementLocation,
        match_info: &'a MatchInfo,
        _infos: &[Self::Info],
    ) -> Self::Info {
        for var in match_info.inputs() {
            self.add_use_site(var.var_id, statement_location);
        }
    }

    fn info_from_return(
        &mut self,
        statement_location: StatementLocation,
        vars: &[VarUsage],
    ) -> Self::Info {
        for var in vars {
            self.add_use_site(var.var_id, statement_location);
        }
    }
}

#[derive(Default)]
pub struct CancelOpsRebuilder {
    renamed_vars: UnorderedHashMap<VariableId, VariableId>,
}

impl Rebuilder for CancelOpsRebuilder {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        let Some(mut new_var_id) = self.renamed_vars.get(&var).cloned() else {
            return var;
        };
        while let Some(new_id) = self.renamed_vars.get(&new_var_id) {
            assert_ne!(new_var_id, *new_id);
            new_var_id = *new_id;
        }

        self.renamed_vars.insert(var, new_var_id);
        new_var_id
    }

    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        block
    }
}
