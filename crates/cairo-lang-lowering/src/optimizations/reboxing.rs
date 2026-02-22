#[cfg(test)]
#[path = "reboxing_test.rs"]
mod reboxing_test;

use std::rc::Rc;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::flag::FlagsGroup;
use cairo_lang_semantic::corelib::core_box_ty;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;
use salsa::Database;

use super::var_renamer::VarRenamer;
use crate::analysis::StatementLocation;
use crate::blocks::Blocks;
use crate::utils::RebuilderEx;
use crate::{
    BlockEnd, Lowered, Statement, StatementStructDestructure, VarUsage, Variable, VariableArena,
    VariableId,
};

/// Data for the MemberOfUnboxed variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MemberOfUnboxedData {
    /// The source reboxing value.
    pub source: Rc<ReboxingValue>,
    /// The member index.
    pub member: usize,
    /// The location where the deconstruct statement occurs.
    pub deconstruct_location: StatementLocation,
}

/// The possible values for the reboxing analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReboxingValue {
    /// No reboxing can be done. Relevant after a meet of two paths.
    Revoked,
    /// The variable is unboxed from a different variable.
    Unboxed(VariableId),
    /// The variable is a member of an unboxed variable.
    MemberOfUnboxed(MemberOfUnboxedData),
}

/// Represents a candidate for reboxing optimization.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReboxCandidate {
    /// The reboxing data.
    pub source: ReboxingValue,
    /// The reboxed variable (output of into_box).
    pub reboxed_var: VariableId,
    /// Location where into_box call occurs (block_id, stmt_idx).
    pub into_box_location: StatementLocation,
}

/// Finds reboxing candidates in the lowered function. Assumes a topological sort of blocks.
///
/// This analysis detects patterns where we:
/// 1. Unbox a struct
/// 2. (Optional) Destructure it
/// 3. Box one of the members back
///
/// Returns candidates that can be optimized with struct_boxed_deconstruct libfunc calls.
pub fn find_reboxing_candidates<'db>(lowered: &Lowered<'db>) -> OrderedHashSet<ReboxCandidate> {
    if lowered.blocks.is_empty() {
        return Default::default();
    }

    trace!("Running reboxing analysis...");

    // TODO(eytan-starkware): When applied, reboxing analysis should replace the existing
    // deconstruct with a boxed-deconstruct, and add unbox statements on members as needed.

    // TODO(eytan-starkware): Support "snapshot" equality tracking in the reboxing analysis.
    // Currently we track unboxed values and their members, but we don't properly handle
    // the case where snapshots are taken and we need to track that a snapshot of a member
    // is equivalent to a member of a snapshot.

    let mut current_state: OrderedHashMap<VariableId, ReboxingValue> = Default::default();
    let mut candidates: OrderedHashSet<ReboxCandidate> = Default::default();

    for (block_id, block) in lowered.blocks.iter() {
        for (stmt_idx, stmt) in block.statements.iter().enumerate() {
            match stmt {
                Statement::Unbox(unbox_stmt) => {
                    let res = ReboxingValue::Unboxed(unbox_stmt.input.var_id);
                    current_state.insert(unbox_stmt.output, res);
                }
                Statement::IntoBox(into_box_stmt) => {
                    let source = current_state
                        .get(&into_box_stmt.input.var_id)
                        .unwrap_or(&ReboxingValue::Revoked);
                    if !matches!(source, ReboxingValue::Revoked) {
                        candidates.insert(ReboxCandidate {
                            source: source.clone(),
                            reboxed_var: into_box_stmt.output,
                            into_box_location: (block_id, stmt_idx),
                        });
                    }
                }
                Statement::StructDestructure(destructure_stmt) => {
                    let info = &lowered.variables[destructure_stmt.input.var_id].info;
                    if info.copyable.is_err() || info.droppable.is_err() {
                        continue;
                    }
                    let input_state = current_state
                        .get(&destructure_stmt.input.var_id)
                        .cloned()
                        .unwrap_or(ReboxingValue::Revoked);
                    match input_state {
                        ReboxingValue::Revoked => {}
                        ReboxingValue::MemberOfUnboxed { .. } | ReboxingValue::Unboxed(_) => {
                            let input_state_rc = Rc::new(input_state);
                            for (member_idx, output_var) in
                                destructure_stmt.outputs.iter().enumerate()
                            {
                                let res = ReboxingValue::MemberOfUnboxed(MemberOfUnboxedData {
                                    source: Rc::clone(&input_state_rc),
                                    deconstruct_location: (block_id, stmt_idx),
                                    member: member_idx,
                                });

                                current_state.insert(*output_var, res);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Process block end to handle variable remapping
        if let BlockEnd::Goto(_, remapping) = &block.end {
            for (dst, src_usage) in remapping.iter() {
                let src_state =
                    current_state.get(&src_usage.var_id).cloned().unwrap_or(ReboxingValue::Revoked);
                update_reboxing_variable_join(&mut current_state, *dst, src_state);
            }
        }
    }

    trace!("Found {} reboxing candidate(s).", candidates.len());
    candidates
}

/// Update the reboxing state for a variable join. If the variable is already in the state with a
/// different value, it is revoked.
fn update_reboxing_variable_join(
    current_state: &mut OrderedHashMap<id_arena::Id<crate::VariableMarker>, ReboxingValue>,
    var: VariableId,
    res: ReboxingValue,
) {
    match current_state.entry(var) {
        Entry::Vacant(entry) => {
            entry.insert(res);
        }
        Entry::Occupied(mut entry) => {
            if entry.get() != &res {
                entry.insert(ReboxingValue::Revoked);
            }
        }
    }
}

/// Represents an operation to apply to the lowered function.
#[derive(Debug)]
enum ReboxingOperation<'db> {
    Add { location: StatementLocation, statement: Statement<'db> },
    Remove { location: StatementLocation },
}

impl<'db> ReboxingOperation<'db> {
    /// Applies this operation to the lowered function, adding or removing statements as specified.
    /// Note: This function modifies the lowered object, and applying multiple operations may
    /// require applying in reverse order to statement location.
    fn apply(self, lowered: &mut Lowered<'db>) {
        match self {
            ReboxingOperation::Add { location: (block_id, stmt_idx), statement } => {
                lowered.blocks[block_id].statements.insert(stmt_idx, statement);
            }
            ReboxingOperation::Remove { location: (block_id, stmt_idx) } => {
                lowered.blocks[block_id].statements.remove(stmt_idx);
            }
        }
    }

    /// Returns the statement location associated with this operation.
    fn location(&self) -> StatementLocation {
        match self {
            ReboxingOperation::Add { location, .. } => *location,
            ReboxingOperation::Remove { location } => *location,
        }
    }
}

/// Applies reboxing optimizations to the lowered function using the provided candidates.
pub fn apply_reboxing_candidates<'db>(
    db: &'db dyn Database,
    lowered: &mut Lowered<'db>,
    candidates: &OrderedHashSet<ReboxCandidate>,
) -> Maybe<()> {
    if candidates.is_empty() {
        trace!("No reboxing candidates to apply.");
        return Ok(());
    }

    trace!("Applying {} reboxing optimization(s).", candidates.len());

    let mut renamer = VarRenamer::default();
    let mut operations = Vec::new();
    let mut added_boxes = UnorderedHashMap::default();

    for candidate in candidates.iter() {
        let box_var_to_use = match &candidate.source {
            ReboxingValue::Revoked => unreachable!("Revoked reboxing candidate should not exist"),
            ReboxingValue::Unboxed(id) => *id,
            ReboxingValue::MemberOfUnboxed(data) => {
                // Create output variables for all members (all will be Box<MemberType>), or get
                // existing ones.
                create_deconstruct_statements(
                    db,
                    &mut lowered.variables,
                    &lowered.blocks,
                    &mut operations,
                    &mut added_boxes,
                    data,
                )
            }
        };
        renamer.renamed_vars.insert(candidate.reboxed_var, box_var_to_use);
        operations.push(ReboxingOperation::Remove { location: candidate.into_box_location });
    }

    // Sort operations by location in reverse order (highest block_id, highest stmt_idx first)
    // This ensures that an operation index won't change before it is updated.
    operations.sort_by_key(|op| {
        let (block_id, stmt_idx) = op.location();
        (std::cmp::Reverse(block_id.0), std::cmp::Reverse(stmt_idx))
    });

    operations.into_iter().for_each(|operation| operation.apply(lowered));

    for block in lowered.blocks.iter_mut() {
        *block = renamer.rebuild_block(block);
    }
    Ok(())
}

/// Recursively allocate boxed variables for a deconstruct statement and its original deconstruct
/// creation. Add the boxed_deconstruct operations (recursively) to the operations vector.
#[allow(clippy::too_many_arguments)]
fn create_deconstruct_statements<'db>(
    db: &'db dyn Database,
    variables: &mut VariableArena<'db>,
    blocks: &Blocks<'db>,
    operations: &mut Vec<ReboxingOperation<'db>>,
    added_boxes: &mut UnorderedHashMap<VariableId, Vec<VariableId>>,
    data: &MemberOfUnboxedData,
) -> VariableId {
    let deconstruct_statement = &blocks[data.deconstruct_location];
    let deconstruct_outputs = deconstruct_statement.outputs();
    let input_var = deconstruct_statement.inputs()[0];
    if let Some(boxed_vars) = added_boxes.get(&input_var.var_id) {
        return boxed_vars[data.member];
    }

    let source_box = match data.source.as_ref() {
        ReboxingValue::Revoked => {
            unreachable!("A Revoked reboxing state should be blocked before.")
        }
        ReboxingValue::Unboxed(id) => {
            // We are at top of the recursion and found our source var.
            *id
        }
        ReboxingValue::MemberOfUnboxed(data) => {
            // Recurse to find the source var.
            create_deconstruct_statements(db, variables, blocks, operations, added_boxes, data)
        }
    };
    let outputs = deconstruct_outputs
        .iter()
        .map(|out_var_id| {
            let out_var = &variables[*out_var_id];
            let box_ty = core_box_ty(db, out_var.ty);
            let out_location = out_var.location;
            variables.alloc(Variable::with_default_context(db, box_ty, out_location))
        })
        .collect_vec();

    operations.push(ReboxingOperation::Add {
        location: data.deconstruct_location,
        statement: Statement::StructDestructure(StatementStructDestructure {
            input: VarUsage { var_id: source_box, location: input_var.location },
            outputs: outputs.clone(),
        }),
    });
    added_boxes.insert(input_var.var_id, outputs);
    added_boxes[&input_var.var_id][data.member]
}

/// Applies the reboxing optimization to the lowered function.
///
/// This optimization detects patterns where we:
/// 1. Unbox a struct
/// 2. (Optional) Destructure it
/// 3. Box one of the members back
///
/// And replaces it with a direct struct_boxed_deconstruct libfunc call.
pub fn apply_reboxing<'db>(db: &'db dyn Database, lowered: &mut Lowered<'db>) -> Maybe<()> {
    if db.flag_future_sierra() {
        let candidates = find_reboxing_candidates(lowered);
        apply_reboxing_candidates(db, lowered, &candidates)
    } else {
        Ok(())
    }
}
