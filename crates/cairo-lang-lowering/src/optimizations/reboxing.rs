#[cfg(test)]
#[path = "reboxing_test.rs"]
mod reboxing_test;

use std::fmt::Display;
use std::rc::Rc;

use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::types::{TypesSemantic, peel_snapshots};
use cairo_lang_semantic::{ConcreteTypeId, GenericArgumentId, TypeId, TypeLongId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use salsa::Database;

use crate::{
    BlockId, Lowered, Statement, StatementStructDestructure, VarUsage, Variable, VariableArena,
    VariableId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReboxingValue {
    Nothing,
    Unboxed(VariableId),
    MemberOfUnboxed { source: Rc<ReboxingValue>, member: usize },
}

impl Display for ReboxingValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReboxingValue::Nothing => write!(f, "Nothing"),
            ReboxingValue::Unboxed(id) => write!(f, "Unboxed({})", id.index()),
            ReboxingValue::MemberOfUnboxed { source, member } => {
                write!(f, "MemberOfUnboxed({}, {})", source, member)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReboxCandidate {
    /// The reboxing data
    pub source: ReboxingValue,
    /// The reboxed variable (output of into_box)
    pub reboxed_var: VariableId,
    /// Location where into_box call occurs (block_id, stmt_idx)
    pub into_box_location: (BlockId, usize),
}

/// Finds reboxing candidates in the lowered function.
///
/// This analysis detects patterns where we:
/// 1. Unbox a struct
/// 2. (Optional) Destructure it
/// 3. Box one of the members back
///
/// Returns candidates that can be optimized with struct_boxed_deconstruct libfunc calls.
pub fn find_reboxing_candidates<'db>(
    db: &'db dyn Database,
    lowered: &Lowered<'db>,
) -> OrderedHashSet<ReboxCandidate> {
    if lowered.blocks.is_empty() {
        return OrderedHashSet::default();
    }

    trace!("Running reboxing analysis...");

    let core = ModuleHelper::core(db);
    let box_module = core.submodule("box");
    let unbox_id = box_module.extern_function_id("unbox");
    let into_box_id = box_module.extern_function_id("into_box");

    // TODO(eytan-starkware): Support "snapshot" equality tracking in the reboxing analysis.
    // Currently we track unboxed values and their members, but we don't properly handle
    // the case where snapshots are taken and we need to track that a snapshot of a member
    // is equivalent to a member of a snapshot.

    let mut current_state: OrderedHashMap<VariableId, ReboxingValue> = OrderedHashMap::default();
    let mut candidates: OrderedHashSet<ReboxCandidate> = OrderedHashSet::default();

    // Worklist algorithm is just iterating the blocks as we run after block reordering which gives
    // us a topological sort (TODO: add to doc)
    for (block_id, block) in lowered.blocks.iter() {
        // Process statements
        for (stmt_idx, stmt) in block.statements.iter().enumerate() {
            match stmt {
                Statement::Call(call_stmt) => {
                    if let Some((extern_id, _)) = call_stmt.function.get_extern(db) {
                        if extern_id == unbox_id {
                            current_state.insert(
                                call_stmt.outputs[0],
                                ReboxingValue::Unboxed(call_stmt.inputs[0].var_id),
                            );
                        } else if extern_id == into_box_id {
                            let source = current_state
                                .get(&call_stmt.inputs[0].var_id)
                                .unwrap_or(&ReboxingValue::Nothing);
                            if matches!(source, ReboxingValue::Nothing) {
                                continue;
                            }
                            candidates.insert(ReboxCandidate {
                                source: source.clone(),
                                reboxed_var: call_stmt.outputs[0],
                                into_box_location: (block_id, stmt_idx),
                            });
                        }
                    }
                }
                Statement::StructDestructure(destructure_stmt) => {
                    let input_state = current_state
                        .get(&destructure_stmt.input.var_id)
                        .cloned()
                        .unwrap_or(ReboxingValue::Nothing);
                    match input_state {
                        ReboxingValue::Nothing => {}
                        ReboxingValue::MemberOfUnboxed { .. } | ReboxingValue::Unboxed(_) => {
                            for (member_idx, output_var) in
                                destructure_stmt.outputs.iter().enumerate()
                            {
                                let res = ReboxingValue::MemberOfUnboxed {
                                    source: Rc::new(input_state.clone()),
                                    member: member_idx,
                                };
                                current_state.insert(*output_var, res);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    trace!("Found {} reboxing candidate(s).", candidates.len());
    candidates
}

/// Applies reboxing optimizations to the lowered function using the provided candidates.
pub fn apply_reboxing_candidates<'db>(
    db: &'db dyn Database,
    lowered: &mut Lowered<'db>,
    candidates: &OrderedHashSet<ReboxCandidate>,
) {
    if candidates.is_empty() {
        trace!("No reboxing candidates to apply.");
        return;
    }

    trace!("Applying {} reboxing optimization(s).", candidates.len());

    for candidate in candidates {
        apply_reboxing_candidate(db, lowered, candidate);
    }
}

/// Applies the reboxing optimization to the lowered function.
///
/// This optimization detects patterns where we:
/// 1. Unbox a struct
/// 2. (Optional) Destructure it
/// 3. Box one of the members back
///
/// And replaces it with a direct struct_boxed_deconstruct libfunc call.
pub fn apply_reboxing<'db>(db: &'db dyn Database, lowered: &mut Lowered<'db>) {
    let candidates = find_reboxing_candidates(db, lowered);
    apply_reboxing_candidates(db, lowered, &candidates);
}

/// Applies a single reboxing optimization for the given candidate.
fn apply_reboxing_candidate<'db>(
    db: &'db dyn Database,
    lowered: &mut Lowered<'db>,
    candidate: &ReboxCandidate,
) {
    trace!(
        "Applying optimization: candidate={}, reboxed={}",
        candidate.source,
        candidate.reboxed_var.index()
    );

    // Only support MemberOfUnboxed where source is Unboxed for now.
    if let ReboxingValue::MemberOfUnboxed { source, member } = &candidate.source {
        if let ReboxingValue::Unboxed(source_var) = **source {
            // Create the struct_boxed_deconstruct call
            if let Some(new_stmt) = create_struct_boxed_deconstruct_call(
                db,
                &mut lowered.variables,
                source_var,
                *member,
                candidate.reboxed_var,
                &lowered.blocks[candidate.into_box_location.0].statements
                    [candidate.into_box_location.1],
            ) {
                // swap to the new call
                let (into_box_block, into_box_stmt_idx) = candidate.into_box_location;
                lowered.blocks[into_box_block].statements[into_box_stmt_idx] = new_stmt;

                trace!("Successfully applied reboxing optimization.");
            }
        } else {
            // Nested MemberOfUnboxed not supported yet.
        }
    } else {
        // Unboxed reboxing not supported yet.
    }
}

/// Creates a struct_boxed_deconstruct call statement.
/// Returns None if the call cannot be created.
fn create_struct_boxed_deconstruct_call<'db>(
    db: &'db dyn Database,
    variables: &mut VariableArena<'db>,
    boxed_struct_var: VariableId,
    member_index: usize,
    output_var: VariableId,
    old_stmt: &Statement<'db>,
) -> Option<Statement<'db>> {
    // TODO(eytan-starkware): Accept a collection of vars to create a box of. A single call to
    // struct_boxed_deconstruct can be created for multiple vars.       When creating multivars
    // we need to put creation at a dominating point.

    let boxed_struct_ty = variables[boxed_struct_var].ty;
    trace!("Creating struct_boxed_deconstruct call for type {:?}", boxed_struct_ty);

    // Extract the struct type from Box<Struct>
    // The boxed type should be Box<T>, we need to get T
    let TypeLongId::Concrete(concrete_box) = boxed_struct_ty.long(db) else {
        unreachable!("Unbox should always be called on a box type (which is concrete).");
    };

    let generic_args = concrete_box.generic_args(db);
    // TODO: Why isnt this a box??????
    let GenericArgumentId::Type(inner_ty) = generic_args.first()? else {
        unreachable!("Box unbox call should always have a generic arg");
    };

    if db.copyable(TypeId::new(db, inner_ty.long(db))).is_err() {
        return None;
    }
    let (n_snapshots, struct_ty) = peel_snapshots(db, *inner_ty);

    // TODO(eytan-starkware): Support snapshots of structs in reboxing optimization.
    // Currently we give up if the struct is wrapped in snapshots.
    if n_snapshots > 0 {
        trace!("Skipping reboxing for snapshotted struct (n_snapshots={})", n_snapshots);
        return None;
    }

    trace!("Extracted struct or tuple type: {:?}", struct_ty);

    // Get the type info to determine number of members
    let (num_members, member_types): (usize, Vec<TypeId<'_>>) = match struct_ty {
        TypeLongId::Concrete(ConcreteTypeId::Struct(struct_id)) => {
            let members = db.concrete_struct_members(struct_id).ok()?;
            let num = members.len();
            let types = members.iter().map(|(_, member)| member.ty).collect();
            (num, types)
        }
        TypeLongId::Tuple(inner_types) => {
            let num = inner_types.len();
            (num, inner_types)
        }
        _ => {
            trace!("Unsupported type for reboxing: {:?}", struct_ty);
            return None;
        }
    };

    trace!("Type has {} members, accessing member {}", num_members, member_index);

    if member_index >= num_members {
        unreachable!("Member index out of bounds");
    }

    // Create output variables for all members (all will be Box<MemberType>)
    // We'll create new variables except for the one we're interested in
    let mut outputs = Vec::new();
    for (idx, member_ty) in member_types.into_iter().enumerate() {
        if idx == member_index {
            // Use the existing output variable
            outputs.push(output_var);
        } else {
            // Create a new variable for this member
            // The type should be Box<member_ty>
            let box_ty = cairo_lang_semantic::corelib::core_box_ty(db, member_ty);
            let out_location = variables[output_var].location;
            let var = variables.alloc(Variable::with_default_context(db, box_ty, out_location));
            outputs.push(var);
        }
    }

    // Create the call statement
    let old_input = old_stmt.inputs()[0];
    let stmt = Statement::StructDestructure(StatementStructDestructure {
        input: VarUsage { var_id: boxed_struct_var, location: old_input.location },
        outputs,
    });

    Some(stmt)
}
