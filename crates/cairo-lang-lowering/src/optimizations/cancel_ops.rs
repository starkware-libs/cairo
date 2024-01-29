#[cfg(test)]
#[path = "cancel_ops_test.rs"]
mod test;

use std::{iter, vec};

use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::{self, UnorderedHashMap};
use id_arena::Arena;
use itertools::{chain, zip_eq, Itertools};

use crate::borrow_check::analysis::{Analyzer, BackAnalysis, StatementLocation};
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, MatchInfo, Statement, StatementStructDestructure,
    VarRemapping, VarUsage, Variable, VariableId,
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
        aliases: Default::default(),
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
    for (block_id, stmt_id) in stmts_to_remove.into_iter().rev().dedup() {
        lowered.blocks[block_id].statements.remove(stmt_id);
    }

    rebuild_blocks(lowered, &mut var_remapper, vars_to_split);
}

/// Rebuilds the blocks.
/// 'var_remapper' is used to remap the variables in the blocks.
/// 'vars_to_split' maps from var_id to the variables that it should be split into,
/// Note however that it does not contain the correct ids for the new variables,
/// The new ids are allocated on the fly in the `split` mapping.
fn rebuild_blocks(
    lowered: &mut FlatLowered,
    var_remapper: &mut CancelOpsRebuilder,
    mut vars_to_split: UnorderedHashMap<VariableId, Vec<VariableId>>,
) {
    let mut split = UnorderedHashMap::<VariableId, Vec<VariableId>>::default();

    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &mut lowered.blocks[block_id];
        let old_statments = std::mem::take(&mut block.statements);

        for stmt in old_statments.into_iter() {
            match stmt {
                Statement::StructDestructure(stmt) => {
                    if let Some(output_split) = split.get(&stmt.input.var_id) {
                        for (output, new_var) in zip_eq(stmt.outputs.iter(), output_split.to_vec())
                        {
                            if let Some(new_var_split) = split.get(&new_var) {
                                split.insert(*output, new_var_split.to_vec());
                            }
                            assert!(var_remapper.renamed_vars.insert(*output, new_var).is_none())
                        }
                    } else {
                        block.statements.push(Statement::StructDestructure(stmt));
                    }
                }
                Statement::StructConstruct(stmt) if vars_to_split.contains_key(&stmt.output) => {
                    split.insert(
                        stmt.output,
                        stmt.inputs.into_iter().map(|input| input.var_id).collect(),
                    );
                }
                _ => {
                    block.statements.push(stmt);
                }
            }
        }

        match &mut block.end {
            FlatBlockEnd::Goto(block_id, remappings) => {
                stack.push(*block_id);

                let mut old_remappings = std::mem::take(remappings);

                rebuild_remapping(
                    &mut lowered.variables,
                    &mut split,
                    &mut vars_to_split,
                    &mut block.statements,
                    std::mem::take(&mut old_remappings.remapping).into_iter(),
                    remappings,
                );
            }
            FlatBlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));
            }
            FlatBlockEnd::Return(_) | FlatBlockEnd::Panic(_) => {}
            FlatBlockEnd::NotSet => unreachable!(),
        }

        // Remap block variables.
        *block = var_remapper.rebuild_block(block);
    }
}

fn rebuild_remapping(
    variables: &mut Arena<Variable>,
    split: &mut UnorderedHashMap<VariableId, Vec<VariableId>>,
    vars_to_split: &mut UnorderedHashMap<VariableId, Vec<VariableId>>,
    statements: &mut Vec<Statement>,
    remappings: impl Iterator<Item = (VariableId, VarUsage)>,
    new_remappings: &mut VarRemapping,
) {
    for (dst, src) in remappings {
        // Check if we need create a split for 'dst'.
        if let Some(orig_dst_vars) = vars_to_split.get(&dst) {
            if let unordered_hash_map::Entry::Vacant(e) = split.entry(dst) {
                e.insert(
                    orig_dst_vars
                        .clone()
                        .into_iter()
                        .map(|orig_dst| {
                            let new_var = variables.alloc(variables[orig_dst].clone());
                            if let Some(split) = vars_to_split.get(&orig_dst) {
                                // Note that we can't use new_var=orig_dst in this case as
                                // split[orig_dst] might already be defined.
                                vars_to_split.insert(new_var, split.clone());
                            }
                            new_var
                        })
                        .collect_vec(),
                );
            }
        }

        eprintln!(
            "{:?}: {:?} -> {:?}: {:?}",
            src.var_id,
            split.get(&src.var_id),
            dst,
            split.get(&dst)
        );

        match (split.get(&dst), split.get(&src.var_id)) {
            (None, None) => {
                new_remappings.insert(dst, src);
            }
            (Some(dst_vars), Some(src_vars)) => {
                let src_vars = src_vars.clone();
                let dst_vars = dst_vars.clone();

                for (dst, inner_src) in zip_eq(dst_vars, src_vars) {
                    rebuild_remapping(
                        variables,
                        split,
                        vars_to_split,
                        statements,
                        iter::once((dst, VarUsage { var_id: inner_src, location: src.location })),
                        new_remappings,
                    );
                }
            }
            (Some(dst_vars), None) => {
                eprintln!("here: {:?}", dst_vars);
                let mut src_vars = vec![];

                for dst in dst_vars {
                    src_vars.push(variables.alloc(variables[*dst].clone()));
                }

                statements.push(Statement::StructDestructure(StatementStructDestructure {
                    input: src,
                    outputs: src_vars.clone(),
                }));

                let dst_vars = dst_vars.clone();
                split.insert(src.var_id, src_vars.clone());

                rebuild_remapping(
                    variables,
                    split,
                    vars_to_split,
                    statements,
                    zip_eq(
                        dst_vars,
                        src_vars
                            .into_iter()
                            .map(|var_id| VarUsage { var_id, location: src.location }),
                    )
                    .collect_vec()
                    .into_iter(),
                    new_remappings,
                );
            }
            (None, Some(_src_vars)) => {
                // let new_src = variables.alloc(variables[dst].clone());

                // statements.push(Statement::StructConstruct(StatementStructConstruct {
                //     inputs: src_vars
                //         .iter()
                //         .map(|var_id| VarUsage { var_id: *var_id, location: src.location })
                //         .collect_vec(),
                //     output: new_src,
                // }));
                // new_remappings.insert(*dst, VarUsage { var_id: new_src, location: src.location
                // });
                panic!("split {:?} to unsplittable {:?}", src, dst)
            }
        }
    }
}

pub struct CancelOpsContext<'a> {
    lowered: &'a FlatLowered,

    /// Maps a variable to the use sites of that variable.
    /// Note that a remapping is cosidered as usage here.
    use_sites: UnorderedHashMap<VariableId, Vec<StatementLocation>>,

    /// Maps a variable to the variable that it was renamed to.
    var_remapper: CancelOpsRebuilder,

    /// Keeps track of all the aliases created by the renaming.
    aliases: UnorderedHashMap<VariableId, Vec<VariableId>>,

    /// Statements that can be be removed.
    stmts_to_remove: Vec<StatementLocation>,

    /// Maps variables that should be split to a list of variables that it should be split into
    /// note the that 'values' are not the correct ids for the new variables, the new ids are
    /// going to be allocated in the forward pass in `rebuild_blocks`.
    vars_to_split: UnorderedHashMap<VariableId, Vec<VariableId>>,
}

/// Similar to `mapping.get(var).or_default()` but but works for types that don't implement Default.
fn get_entry_as_slice<'a, T>(
    mapping: &'a UnorderedHashMap<VariableId, Vec<T>>,
    var: &VariableId,
) -> &'a [T] {
    match mapping.get(var) {
        Some(entry) => &entry[..],
        None => &[],
    }
}

/// Returns the use sites of a variable.
///
/// Takes 'use_sites' map rather than `CancelOpsContext` to avoid borrowing the entire context.
fn filter_use_sites<'a, F, T>(
    use_sites: &'a UnorderedHashMap<VariableId, Vec<StatementLocation>>,
    var_aliases: &'a UnorderedHashMap<VariableId, Vec<VariableId>>,
    orig_var_id: &VariableId,
    mut f: F,
) -> Vec<T>
where
    F: FnMut(&StatementLocation) -> Option<T>,
{
    let mut res = vec![];

    let aliases = get_entry_as_slice(var_aliases, orig_var_id);

    for var in chain!(std::iter::once(orig_var_id), aliases) {
        let use_sites = get_entry_as_slice(use_sites, var);
        for use_site in use_sites {
            if let Some(filtered) = f(use_site) {
                res.push(filtered);
            }
        }
    }
    res
}

impl<'a> CancelOpsContext<'a> {
    fn rename_var(&mut self, from: VariableId, to: VariableId) {
        self.var_remapper.renamed_vars.insert(from, to);

        let mut aliases = Vec::from_iter(chain(
            std::iter::once(from),
            get_entry_as_slice(&self.aliases, &from).iter().copied(),
        ));
        // Optimize for the case where the alias list of `to` is empty.
        match self.aliases.entry(to) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                aliases.extend(entry.get().iter());
                *entry.get_mut() = aliases;
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(aliases);
            }
        }
    }

    fn add_use_site(&mut self, var: VariableId, use_site: StatementLocation) {
        self.use_sites.entry(var).or_default().push(use_site);
    }

    /// check if a variable should be split.
    fn check_split(
        &self,
        vars_to_split: &mut UnorderedHashMap<VariableId, Vec<VariableId>>,
        new_aliases: &mut Vec<Vec<VariableId>>,
        orig_var_id: &VariableId,
        split: &[VarUsage],
    ) -> bool {
        let aliases = get_entry_as_slice(&self.aliases, orig_var_id).to_vec();

        let mut can_split_variable = true;
        let mut should_split = false;
        for var_id in chain!(std::iter::once(orig_var_id), &aliases) {
            let use_sites = get_entry_as_slice(&self.use_sites, var_id);

            for location in use_sites {
                let block = &self.lowered.blocks[location.0];
                match (block.statements.get(location.1), &block.end) {
                    (Some(Statement::StructDestructure(destructure_stmt)), _) => {
                        for (output, alias_list) in
                            zip_eq(&destructure_stmt.outputs, new_aliases.iter_mut())
                        {
                            alias_list.push(*output);
                        }
                        should_split = true;
                    }
                    (None, FlatBlockEnd::Goto(_, remappings)) => {
                        for (dst, src) in remappings.iter() {
                            if &src.var_id == var_id {
                                should_split |=
                                    self.check_split(vars_to_split, new_aliases, dst, split);
                            }
                        }
                    }
                    _ => {
                        can_split_variable = false;
                    }
                }
            }
        }

        if can_split_variable && should_split {
            for var_id in chain!(std::iter::once(*orig_var_id), aliases) {
                vars_to_split
                    .insert(var_id, split.iter().map(|var_usage| var_usage.var_id).collect_vec());
            }
            true
        } else {
            false
        }
    }

    /// Handles a statement and returns true if it can be removed.
    fn handle_stmt(&mut self, stmt: &'a Statement, statement_location: StatementLocation) -> bool {
        match stmt {
            Statement::StructDestructure(stmt) => {
                let mut visited_use_sites = OrderedHashSet::<StatementLocation>::default();

                let mut can_remove_struct_destructure = true;

                let mut constructs = vec![];
                for output in stmt.outputs.iter() {
                    constructs.extend(filter_use_sites(
                        &self.use_sites,
                        &self.aliases,
                        output,
                        |location| match self.lowered.blocks[location.0].statements.get(location.1)
                        {
                            _ if !visited_use_sites.insert(*location) => {
                                // Filter previously seen use sites.
                                None
                            }
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
                                self.stmts_to_remove.push(*location);
                                Some(construct_stmt)
                            }
                            _ => {
                                can_remove_struct_destructure = false;
                                None
                            }
                        },
                    ));
                }

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
                let mut vars_to_split = std::mem::take(&mut self.vars_to_split);

                let mut new_use_sites = vec![vec![]; stmt.inputs.len()];

                let can_remove_struct_construct = self.check_split(
                    &mut vars_to_split,
                    &mut new_use_sites,
                    &stmt.output,
                    &stmt.inputs[..],
                );
                self.vars_to_split = vars_to_split;

                if can_remove_struct_construct {
                    self.vars_to_split.insert(
                        stmt.output,
                        stmt.inputs.iter().map(|input| input.var_id).collect_vec(),
                    );

                    for (input, new_alias) in zip_eq(stmt.inputs.iter(), new_use_sites) {
                        self.aliases.entry(input.var_id).or_default().extend(new_alias);
                    }
                }

                can_remove_struct_construct
            }
            Statement::Snapshot(stmt) => {
                let mut can_remove_snap = true;

                let desnaps = filter_use_sites(
                    &self.use_sites,
                    &self.aliases,
                    &stmt.output_snapshot,
                    |location| {
                        if let Some(Statement::Desnap(desnap_stmt)) =
                            self.lowered.blocks[location.0].statements.get(location.1)
                        {
                            self.stmts_to_remove.push(*location);
                            Some(desnap_stmt)
                        } else {
                            can_remove_snap = false;
                            None
                        }
                    },
                );

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
