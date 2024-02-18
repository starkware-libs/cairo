#[cfg(test)]
#[path = "split_structs_test.rs"]
mod test;

use std::vec;

use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use id_arena::Arena;
use itertools::{zip_eq, Itertools};

use crate::ids::LocationId;
use crate::utils::{Rebuilder, RebuilderEx};
use crate::{
    BlockId, FlatBlockEnd, FlatLowered, Statement, StatementStructConstruct,
    StatementStructDestructure, VarRemapping, VarUsage, Variable, VariableId,
};

/// Splits all the variables that were created by struct_construct and
/// reintroduces the struct_construct statement when needed.
/// Note that if a member is used after the struct then is means that that struct is duplicatable.

pub fn split_structs(lowered: &mut FlatLowered) {
    if lowered.blocks.is_empty() {
        return;
    }

    let split = get_var_split(lowered);
    rebuild_blocks(lowered, split);
}

/// Returns a mapping from variables that should be split to the variables resulting from the split.
fn get_var_split(lowered: &mut FlatLowered) -> UnorderedHashMap<VariableId, Vec<VariableId>> {
    let mut split = UnorderedHashMap::<VariableId, Vec<VariableId>>::default();

    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &lowered.blocks[block_id];

        for stmt in block.statements.iter() {
            if let Statement::StructConstruct(stmt) = stmt {
                split
                    .entry(stmt.output)
                    .or_insert_with(|| stmt.inputs.iter().map(|input| input.var_id).collect_vec());
            }
        }

        match &block.end {
            FlatBlockEnd::Goto(block_id, remappings) => {
                stack.push(*block_id);

                for (dst, src) in remappings.iter() {
                    split_remapping(&mut split, &mut lowered.variables, *dst, src.var_id);
                }
            }
            FlatBlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));
            }
            FlatBlockEnd::Return(_) => {}
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        }
    }

    split
}

/// Splits 'dst' according to the split of 'src'.
///
/// For example if if we have
///     split('dst') is None
///     split('src') = (v0, v1) and split(`v1`) = (v3, v4, v5).
/// The function will create new variables and set:
///     split('dst') = (v100, v101) and split(`v101`) = (v102, v103, v104).
fn split_remapping(
    split: &mut UnorderedHashMap<id_arena::Id<Variable>, Vec<id_arena::Id<Variable>>>,
    variables: &mut Arena<Variable>,
    dst: VariableId,
    src: VariableId,
) {
    let mut stack = vec![(dst, src)];

    while let Some((dst, src)) = stack.pop() {
        if split.contains_key(&dst) {
            continue;
        }
        if let Some(src_vars) = split.get(&src) {
            let mut dst_vars = vec![];
            for split_src in src_vars {
                let new_var = variables.alloc(variables[*split_src].clone());
                // queue inner remmapping for for possible splitting.
                stack.push((new_var, *split_src));
                dst_vars.push(new_var);
            }

            split.insert(dst, dst_vars);
        }
    }
}

/// Rebuilds the blocks, with the splitting.
fn rebuild_blocks(lowered: &mut FlatLowered, split: UnorderedHashMap<VariableId, Vec<VariableId>>) {
    let mut var_remapper = VarRename::default();

    // variables that were unsplit as they were needed.
    let mut unsplit = UnorderedHashSet::<VariableId>::default();

    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &mut lowered.blocks[block_id];
        let old_statements = std::mem::take(&mut block.statements);

        for stmt in old_statements.into_iter() {
            match stmt {
                Statement::StructDestructure(stmt) => {
                    if let Some(output_split) =
                        split.get(&var_remapper.map_var_id(stmt.input.var_id))
                    {
                        for (output, new_var) in zip_eq(stmt.outputs.iter(), output_split.to_vec())
                        {
                            assert!(var_remapper.renamed_vars.insert(*output, new_var).is_none())
                        }
                    } else {
                        block.statements.push(Statement::StructDestructure(stmt));
                    }
                }
                Statement::StructConstruct(stmt)
                    if split.contains_key(&var_remapper.map_var_id(stmt.output)) =>
                {
                    // Remove StructConstruct statement.
                }
                _ => {
                    for input in stmt.inputs() {
                        unsplit_var(
                            &mut var_remapper,
                            &mut unsplit,
                            &split,
                            &mut block.statements,
                            &input.var_id,
                            input.location,
                        );
                    }
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
                    &mut var_remapper,
                    &split,
                    &mut block.statements,
                    std::mem::take(&mut old_remappings.remapping).into_iter(),
                    remappings,
                );
            }
            FlatBlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));

                for input in info.inputs() {
                    unsplit_var(
                        &mut var_remapper,
                        &mut unsplit,
                        &split,
                        &mut block.statements,
                        &input.var_id,
                        input.location,
                    );
                }
            }
            FlatBlockEnd::Return(vars) => {
                for var in vars {
                    unsplit_var(
                        &mut var_remapper,
                        &mut unsplit,
                        &split,
                        &mut block.statements,
                        &var.var_id,
                        var.location,
                    );
                }
            }
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        }

        // Remap block variables.
        *block = var_remapper.rebuild_block(block);
    }
}

/// Given 'var_id' check if `var_remapper.map_var_id(*var_id)` was split and if
/// so constructs it recursively by adding struct_construct statements to 'statements'.
fn unsplit_var(
    var_remapper: &mut VarRename,
    unsplit: &mut UnorderedHashSet<VariableId>,
    split: &UnorderedHashMap<VariableId, Vec<VariableId>>,
    statements: &mut Vec<Statement>,
    var_id: &VariableId,
    location: LocationId,
) {
    let var_id = var_remapper.map_var_id(*var_id);
    if unsplit.contains(&var_id) {
        return;
    }

    let Some(split_vars) = split.get(&var_id) else {
        return;
    };

    for var in split_vars {
        unsplit_var(var_remapper, unsplit, split, statements, var, location);
    }

    statements.push(Statement::StructConstruct(StatementStructConstruct {
        inputs: split_vars
            .iter()
            .map(|var_id| VarUsage { var_id: *var_id, location })
            .collect_vec(),
        output: var_id,
    }));
}

/// Given an iterator over the original remmaping, rebuilds the remapping with the given
/// splitting of variables.
fn rebuild_remapping(
    variables: &mut Arena<Variable>,
    var_remapper: &mut VarRename,
    split: &UnorderedHashMap<VariableId, Vec<VariableId>>,
    statements: &mut Vec<Statement>,
    remappings: impl DoubleEndedIterator<Item = (VariableId, VarUsage)>,
    new_remappings: &mut VarRemapping,
) {
    let mut stack = remappings.rev().collect_vec();
    while let Some((orig_dst, orig_src)) = stack.pop() {
        let dst = var_remapper.map_var_id(orig_dst);
        let src = var_remapper.map_var_id(orig_src.var_id);
        match (split.get(&dst), split.get(&src)) {
            (None, None) => {
                new_remappings.insert(dst, VarUsage { var_id: src, location: orig_src.location });
            }
            (Some(dst_vars), Some(src_vars)) => {
                stack.extend(zip_eq(
                    dst_vars.iter().cloned().rev(),
                    src_vars
                        .iter()
                        .map(|var_id| VarUsage { var_id: *var_id, location: orig_src.location })
                        .rev(),
                ));
            }
            (Some(dst_vars), None) => {
                let mut src_vars = vec![];

                for dst in dst_vars {
                    src_vars.push(variables.alloc(variables[*dst].clone()));
                }

                statements.push(Statement::StructDestructure(StatementStructDestructure {
                    input: VarUsage { var_id: src, location: orig_src.location },
                    outputs: src_vars.clone(),
                }));

                stack.extend(zip_eq(
                    dst_vars.iter().cloned().rev(),
                    src_vars
                        .into_iter()
                        .map(|var_id| VarUsage { var_id, location: orig_src.location })
                        .rev(),
                ));
            }
            (None, Some(_src_vars)) => {
                unreachable!("a split variable should not be mapped to an unsplit one.")
            }
        }
    }
}

#[derive(Default)]
pub struct VarRename {
    renamed_vars: UnorderedHashMap<VariableId, VariableId>,
}

impl Rebuilder for VarRename {
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
