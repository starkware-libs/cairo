#[cfg(test)]
#[path = "split_structs_test.rs"]
mod test;

use std::{usize, vec};

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use id_arena::Arena;
use itertools::{zip_eq, Itertools};

use crate::borrow_check::analysis::StatementLocation;
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

// Information about a split variable.
struct SplitInfo {
    // The block_id where the variable was defined.
    block_id: BlockId,
    // The variables resulting from the split.
    vars: Vec<VariableId>,
}

// The location where a variable should be unsplit.
enum UnsplitLocation {
    EndOFlock(BlockId),
    Location(StatementLocation),
}

type SplitMapping = UnorderedHashMap<VariableId, SplitInfo>;
type UnsplitMapping = OrderedHashMap<VariableId, UnsplitLocation>;

/// Returns a mapping from variables that should be split to the variables resulting from the split.
fn get_var_split(lowered: &mut FlatLowered) -> SplitMapping {
    let mut split = UnorderedHashMap::<VariableId, SplitInfo>::default();

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
                assert!(
                    split
                        .insert(
                            stmt.output,
                            SplitInfo {
                                block_id,
                                vars: stmt.inputs.iter().map(|input| input.var_id).collect_vec(),
                            },
                        )
                        .is_none()
                );
            }
        }

        match &block.end {
            FlatBlockEnd::Goto(block_id, remappings) => {
                stack.push(*block_id);

                for (dst, src) in remappings.iter() {
                    split_remapping(
                        *block_id,
                        &mut split,
                        &mut lowered.variables,
                        *dst,
                        src.var_id,
                    );
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
    target_block_id: BlockId,
    split: &mut SplitMapping,
    variables: &mut Arena<Variable>,
    dst: VariableId,
    src: VariableId,
) {
    let mut stack = vec![(dst, src)];

    while let Some((dst, src)) = stack.pop() {
        if split.contains_key(&dst) {
            continue;
        }
        if let Some(SplitInfo { block_id: _, vars: src_vars }) = split.get(&src) {
            let mut dst_vars = vec![];
            for split_src in src_vars {
                let new_var = variables.alloc(variables[*split_src].clone());
                // queue inner remmapping for for possible splitting.
                stack.push((new_var, *split_src));
                dst_vars.push(new_var);
            }

            split.insert(dst, SplitInfo { block_id: target_block_id, vars: dst_vars });
        }
    }
}

/// Rebuilds the blocks, with the splitting.
fn rebuild_blocks(lowered: &mut FlatLowered, split: SplitMapping) {
    let mut var_remapper = VarRename::default();

    // variables that were unsplit as they were needed.
    let mut unsplit = UnsplitMapping::default();

    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;

        let block = &mut lowered.blocks[block_id];
        let old_statements = std::mem::take(&mut block.statements);
        let n_unsplit = unsplit.len();

        let mut stmt_idx = 0;

        for stmt in old_statements.into_iter() {
            match stmt {
                Statement::StructDestructure(stmt) => {
                    if let Some(output_split) =
                        split.get(&var_remapper.map_var_id(stmt.input.var_id))
                    {
                        for (output, new_var) in
                            zip_eq(stmt.outputs.iter(), output_split.vars.to_vec())
                        {
                            assert!(var_remapper.renamed_vars.insert(*output, new_var).is_none())
                        }
                    } else {
                        block.statements.push(Statement::StructDestructure(stmt));
                        stmt_idx += 1;
                    }
                }
                Statement::StructConstruct(stmt)
                    if split.contains_key(&var_remapper.map_var_id(stmt.output)) =>
                {
                    // Remove StructConstruct statement.
                }
                _ => {
                    for input in stmt.inputs() {
                        check_if_var_needs_unsplit(
                            &mut var_remapper,
                            &mut unsplit,
                            &split,
                            &input.var_id,
                            block_id,
                            &mut stmt_idx,
                        );
                    }

                    block.statements.push(stmt);
                    stmt_idx += 1;
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
                    check_if_var_needs_unsplit(
                        &mut var_remapper,
                        &mut unsplit,
                        &split,
                        &input.var_id,
                        block_id,
                        &mut stmt_idx,
                    );
                }
            }
            FlatBlockEnd::Return(vars) => {
                for var in vars {
                    check_if_var_needs_unsplit(
                        &mut var_remapper,
                        &mut unsplit,
                        &split,
                        &var.var_id,
                        block_id,
                        &mut stmt_idx,
                    );
                }
            }
            FlatBlockEnd::Panic(_) | FlatBlockEnd::NotSet => unreachable!(),
        }

        // Remap block variables.
        *block = var_remapper.rebuild_block(block);

        unsplit_vars(&mut var_remapper, unsplit.iter().skip(n_unsplit), &split, lowered);
    }
}

/// Given 'var_id' check if `var_remapper.map_var_id(*var_id)` was split and if
/// so constructs it recursively by adding struct_construct statements to 'statements'.
fn check_if_var_needs_unsplit(
    var_remapper: &mut VarRename,
    unsplit: &mut UnsplitMapping,
    split: &SplitMapping,
    var_id: &VariableId,
    block_id: BlockId,
    stmt_idx: &mut usize,
) {
    let var_id = var_remapper.map_var_id(*var_id);
    if unsplit.contains_key(&var_id) {
        return;
    }

    let Some(split_info) = split.get(&var_id) else {
        return;
    };

    for var in &split_info.vars {
        check_if_var_needs_unsplit(var_remapper, unsplit, split, var, block_id, stmt_idx);
    }

    // If the variable was defined in the same block then we can unsplit it before the first usage.
    // If not we need to unsplit it at the end of the of the original block as it might be used
    // by more then one of the childrent.
    let unsplit_location = if block_id == split_info.block_id {
        let res = UnsplitLocation::Location((block_id, *stmt_idx));
        *stmt_idx += 1;
        res
    } else {
        UnsplitLocation::EndOFlock(split_info.block_id)
    };

    unsplit.insert(var_id, unsplit_location);
}

/// Given 'var_id' check if `var_remapper.map_var_id(*var_id)` was split and if
/// so constructs it recursively by adding struct_construct statements to 'statements'.
fn unsplit_vars<'a>(
    var_remapper: &mut VarRename,
    unsplit: impl Iterator<Item = (&'a VariableId, &'a UnsplitLocation)>,
    split: &SplitMapping,
    lowered: &mut FlatLowered,
) {
    for (var_id, location) in unsplit {
        let split_vars =
            split.get(var_id).expect("Should be check in `check_if_var_needs_unsplit`.");

        let stmt = Statement::StructConstruct(StatementStructConstruct {
            inputs: split_vars
                .vars
                .iter()
                .map(|var_id| VarUsage {
                    var_id: var_remapper.map_var_id(*var_id),
                    location: lowered.variables[*var_id].location,
                })
                .collect_vec(),
            output: *var_id,
        });

        match location {
            UnsplitLocation::EndOFlock(block_id) => {
                lowered.blocks[*block_id].statements.push(stmt);
            }
            UnsplitLocation::Location(location) => {
                lowered.blocks[location.0].statements.insert(location.1, stmt);
            }
        };
    }
}

/// Given an iterator over the original remmaping, rebuilds the remapping with the given
/// splitting of variables.
fn rebuild_remapping(
    variables: &mut Arena<Variable>,
    var_remapper: &mut VarRename,
    split: &SplitMapping,
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
            (Some(dst_split), Some(src_split)) => {
                stack.extend(zip_eq(
                    dst_split.vars.iter().cloned().rev(),
                    src_split
                        .vars
                        .iter()
                        .map(|var_id| VarUsage { var_id: *var_id, location: orig_src.location })
                        .rev(),
                ));
            }
            (Some(dst_split), None) => {
                let mut src_vars = vec![];

                for dst in &dst_split.vars {
                    src_vars.push(variables.alloc(variables[*dst].clone()));
                }

                statements.push(Statement::StructDestructure(StatementStructDestructure {
                    input: VarUsage { var_id: src, location: orig_src.location },
                    outputs: src_vars.clone(),
                }));

                stack.extend(zip_eq(
                    dst_split.vars.iter().cloned().rev(),
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
