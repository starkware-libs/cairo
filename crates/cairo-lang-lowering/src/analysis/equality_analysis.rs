//! Equality analysis for lowered IR.
//!
//! This module tracks semantic equivalence between variables as information flows through the
//! program. Two variables are equivalent if they hold the same value. Additionally, the analysis
//! tracks `Box`/unbox, snapshot/desnap, and struct/array construct relationships between
//! equivalence classes. Arrays reuse the struct hashcons infrastructure since both map
//! `(TypeId, Vec<VariableId>)` — array pop operations act as destructures.

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{ExternFunctionId, NamedLanguageElementId};
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::{ConcreteVariant, MatchArmSelector, TypeId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;

use crate::analysis::core::Edge;
use crate::analysis::{DataflowAnalyzer, Direction, ForwardDataflowAnalysis};
use crate::{
    BlockEnd, BlockId, Lowered, MatchArm, MatchExternInfo, MatchInfo, Statement, VariableId,
};

/// Tracks relationships between equivalence classes.
#[derive(Clone, Debug, Default)]
struct ClassInfo {
    /// If this class has a boxed version, the representative of that class.
    boxed_class: Option<VariableId>,
    /// If this class has an unboxed version, the representative of that class.
    unboxed_class: Option<VariableId>,
    /// If this class has a snapshot version, the representative of that class.
    snapshot_class: Option<VariableId>,
    /// If this class is a snapshot, the representative of the original class.
    original_class: Option<VariableId>,
}

impl ClassInfo {
    /// Returns all variables referenced by this ClassInfo's relationships.
    fn referenced_vars(&self) -> impl Iterator<Item = VariableId> + '_ {
        [self.boxed_class, self.original_class, self.snapshot_class, self.unboxed_class]
            .into_iter()
            .flatten()
    }

    /// Returns true if this ClassInfo has no relationships.
    fn is_empty(&self) -> bool {
        self.referenced_vars().next().is_none()
    }

    /// Merges another ClassInfo into this one.
    /// When both have the same relationship type, calls union_fn to merge the related classes.
    fn merge(
        self,
        other: Self,
        union_fn: &mut impl FnMut(VariableId, VariableId) -> VariableId,
    ) -> Self {
        let mut merge_field = |new: Option<VariableId>, old: Option<VariableId>| match (new, old) {
            (Some(new_val), Some(old_val)) if new_val != old_val => {
                Some(union_fn(new_val, old_val))
            }
            (new, old) => new.or(old),
        };
        Self {
            boxed_class: merge_field(self.boxed_class, other.boxed_class),
            unboxed_class: merge_field(self.unboxed_class, other.unboxed_class),
            snapshot_class: merge_field(self.snapshot_class, other.snapshot_class),
            original_class: merge_field(self.original_class, other.original_class),
        }
    }
}

/// State for the equality analysis, tracking variable equivalences.
///
/// This is the `Info` type for the dataflow analysis. Each block gets its own
/// `EqualityState` representing what we know at that point in the program.
///
/// Uses sparse HashMaps for efficiency - only variables that have been touched
/// by the analysis are stored.
#[derive(Clone, Debug, Default)]
pub struct EqualityState<'db> {
    /// Union-find parent map. If a variable is not in the map, it is its own representative.
    union_find: OrderedHashMap<VariableId, VariableId>,

    /// For each equivalence class representative, track relationships only if they exist.
    class_info: OrderedHashMap<VariableId, ClassInfo>,

    /// Hashcons for enum constructs: maps (variant, input_rep) -> output_rep.
    /// This allows us to detect when two enum constructs with the same variant
    /// and equivalent inputs should produce equivalent outputs.
    ///
    /// Keys use representatives at insertion time. In SSA form each variable is defined
    /// exactly once, so representatives cannot change within a block and keys stay valid
    /// without migration during `union`. At merge points the maps are rebuilt from scratch.
    enum_hashcons: OrderedHashMap<(ConcreteVariant<'db>, VariableId), VariableId>,

    /// Reverse hashcons for enum constructs: maps output_rep -> (variant, input_rep).
    /// This allows efficient lookup when matching on an enum to find the original input.
    enum_hashcons_rev: OrderedHashMap<VariableId, (ConcreteVariant<'db>, VariableId)>,

    /// Hashcons for struct/array constructs: maps (type, [field_reps...]) -> output_rep.
    /// This allows us to detect when two constructs with the same type
    /// and equivalent fields/elements should produce equivalent outputs.
    /// Arrays reuse this same infrastructure — `array_new`/`array_append` chains are recorded
    /// as constructs keyed by the array type, and `array_pop_front` acts as a destructure.
    struct_hashcons: OrderedHashMap<(TypeId<'db>, Vec<VariableId>), VariableId>,

    /// Reverse hashcons for struct/array constructs: maps output_rep -> (type,
    /// [field_reps/element_reps...]).
    /// This allows efficient lookup when destructuring a struct or popping from an array.
    struct_hashcons_rev: OrderedHashMap<VariableId, (TypeId<'db>, Vec<VariableId>)>,
}

impl<'db> EqualityState<'db> {
    /// Gets the parent of a variable, defaulting to itself (root) if not in the map.
    fn get_parent(&self, var: VariableId) -> VariableId {
        self.union_find.get(&var).copied().unwrap_or(var)
    }

    /// Gets the class info for a variable, returning a default if not present.
    fn get_class_info(&self, var: VariableId) -> ClassInfo {
        self.class_info.get(&var).cloned().unwrap_or_default()
    }

    /// Finds the representative of a variable's equivalence class.
    /// Uses path compression for efficiency.
    fn find(&mut self, var: VariableId) -> VariableId {
        let parent = self.get_parent(var);
        if parent != var {
            let root = self.find(parent);
            // Path compression: point directly to root.
            self.union_find.insert(var, root);
            root
        } else {
            var
        }
    }

    /// Finds the representative without modifying the structure.
    pub(crate) fn find_immut(&self, var: VariableId) -> VariableId {
        let parent = self.get_parent(var);
        if parent != var { self.find_immut(parent) } else { var }
    }

    /// Unions two variables into the same equivalence class.
    /// Returns the representative of the merged class.
    /// Always chooses the lower ID as the representative to maintain canonical form.
    fn union(&mut self, a: VariableId, b: VariableId) -> VariableId {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a == root_b {
            return root_a;
        }

        // Always choose the lower ID as the new root to maintain canonical form.
        // This ensures hashcons keys remain valid since lower IDs are defined earlier.
        let (new_root, old_root) =
            if root_a.index() < root_b.index() { (root_a, root_b) } else { (root_b, root_a) };

        // Ensure new_root is in the map (as its own parent).
        self.union_find.entry(new_root).or_insert(new_root);
        // Update old_root to point to new_root.
        self.union_find.insert(old_root, new_root);

        // Merge class info: since A == B, we have Box(A) == Box(B), @A == @B, etc.
        // Recursive unions inside merge() only affect related classes (which have strictly
        // one-step increment in information in forward analysis), so they never deposit class_info
        // back at new_root.
        let old_info = self.class_info.swap_remove(&old_root).unwrap_or_default();
        let new_info = self.class_info.swap_remove(&new_root).unwrap_or_default();
        let merged = new_info.merge(old_info, &mut |a, b| self.union(a, b));
        if !merged.is_empty() {
            let final_root = self.find(new_root);
            self.class_info.insert(final_root, merged);
        }

        self.find(new_root)
    }

    /// Looks up a related variable through a ClassInfo field accessor.
    fn get_related(
        &mut self,
        var: VariableId,
        field: fn(&mut ClassInfo) -> &mut Option<VariableId>,
    ) -> Option<VariableId> {
        let rep = self.find(var);
        let mut info = self.get_class_info(rep);
        let related = (*field(&mut info))?;
        Some(self.find(related))
    }

    /// Sets a bidirectional relationship between two variables' equivalence classes.
    /// If inputs already have a relationship of the same kind, unions with the existing class.
    fn set_relationship(
        &mut self,
        var_a: VariableId,
        var_b: VariableId,
        field_a_to_b: fn(&mut ClassInfo) -> &mut Option<VariableId>,
        field_b_to_a: fn(&mut ClassInfo) -> &mut Option<VariableId>,
    ) {
        // Union with existing relationships if present.
        if let Some(existing) = self.get_related(var_a, field_a_to_b) {
            self.union(var_b, existing);
        }
        if let Some(existing) = self.get_related(var_b, field_b_to_a) {
            self.union(var_a, existing);
        }

        // Re-find after potential unions — representatives may have changed.
        let rep_a = self.find(var_a);
        let rep_b = self.find(var_b);

        *field_a_to_b(self.class_info.entry(rep_a).or_default()) = Some(rep_b);
        *field_b_to_a(self.class_info.entry(rep_b).or_default()) = Some(rep_a);
    }

    /// Sets a box relationship: boxed_var = Box(unboxed_var).
    fn set_box_relationship(&mut self, unboxed_var: VariableId, boxed_var: VariableId) {
        self.set_relationship(
            unboxed_var,
            boxed_var,
            |ci| &mut ci.boxed_class,
            |ci| &mut ci.unboxed_class,
        );
    }

    /// Sets a snapshot relationship: snapshot_var = @original_var.
    fn set_snapshot_relationship(&mut self, original_var: VariableId, snapshot_var: VariableId) {
        self.set_relationship(
            original_var,
            snapshot_var,
            |ci| &mut ci.snapshot_class,
            |ci| &mut ci.original_class,
        );
    }

    /// Records an enum construct: output = Variant(input).
    /// If we've already seen the same variant with an equivalent input, unions the outputs.
    fn set_enum_construct(
        &mut self,
        variant: ConcreteVariant<'db>,
        input: VariableId,
        output: VariableId,
    ) {
        let input_rep = self.find(input);
        let output_rep = self.find(output);

        match self.enum_hashcons.entry((variant, input_rep)) {
            cairo_lang_utils::ordered_hash_map::Entry::Occupied(entry) => {
                let existing_output = *entry.get();
                self.union(existing_output, output);
                // Union may have changed the representative. Update the reverse map
                // so that transfer_edge lookups via find() hit the current representative.
                let new_output_rep = self.find(existing_output);
                self.enum_hashcons_rev.swap_remove(&existing_output);
                self.enum_hashcons_rev.insert(new_output_rep, (variant, input_rep));
            }
            cairo_lang_utils::ordered_hash_map::Entry::Vacant(entry) => {
                entry.insert(output_rep);
                self.enum_hashcons_rev.insert(output_rep, (variant, input_rep));
            }
        }
    }

    /// Records a struct construct: output = StructType(inputs...).
    /// If we've already seen the same type with equivalent inputs, unions the outputs.
    fn set_struct_construct(
        &mut self,
        ty: TypeId<'db>,
        input_reps: Vec<VariableId>,
        output: VariableId,
    ) {
        let key = (ty, input_reps);
        if let Some(&existing_output) = self.struct_hashcons.get(&key) {
            self.union(existing_output, output);
        } else {
            let output_rep = self.find(output);
            self.struct_hashcons.insert(key.clone(), output_rep);
            self.struct_hashcons_rev.insert(output_rep, key);
        }
    }
}

impl<'db> DebugWithDb<'db> for EqualityState<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let v = |id: VariableId| format!("v{}", self.find_immut(id).index());
        let mut lines = Vec::<String>::new();
        for (&rep, info) in self.class_info.iter() {
            if let Some(s) = info.snapshot_class {
                lines.push(format!("@{} = {}", v(rep), v(s)));
            }
            if let Some(b) = info.boxed_class {
                lines.push(format!("Box({}) = {}", v(rep), v(b)));
            }
        }
        for (&(variant, input), &output) in self.enum_hashcons.iter() {
            let name = variant.id.name(db).to_string(db);
            lines.push(format!("{name}({}) = {}", v(input), v(output)));
        }
        for ((ty, inputs), &output) in self.struct_hashcons.iter() {
            let type_name = ty.format(db);
            let fields = inputs.iter().map(|&id| v(id)).collect::<Vec<_>>().join(", ");
            lines.push(format!("{type_name}({fields}) = {}", v(output)));
        }
        for &var in self.union_find.keys() {
            let rep = self.find_immut(var);
            if var != rep {
                lines.push(format!("v{} = v{}", rep.index(), var.index()));
            }
        }
        lines.sort();
        if lines.is_empty() { write!(f, "(empty)") } else { write!(f, "{}", lines.join(", ")) }
    }
}

/// Variable equality analysis.
///
/// This analyzer tracks snapshot/desnap, box/unbox, and array construct relationships as data
/// flows through the program. At merge points (after match arms converge), we conservatively
/// intersect the equivalence classes, keeping only equalities that hold on all paths.
pub struct EqualityAnalysis<'a, 'db> {
    db: &'db dyn Database,
    lowered: &'a Lowered<'db>,
    /// The `array_new` extern function id.
    array_new: ExternFunctionId<'db>,
    /// The `array_append` extern function id.
    array_append: ExternFunctionId<'db>,
    /// The `array_pop_front` extern function id.
    array_pop_front: ExternFunctionId<'db>,
    /// The `array_pop_front_consume` extern function id.
    array_pop_front_consume: ExternFunctionId<'db>,
    /// The `array_snapshot_pop_front` extern function id.
    array_snapshot_pop_front: ExternFunctionId<'db>,
    /// The `array_snapshot_pop_back` extern function id.
    array_snapshot_pop_back: ExternFunctionId<'db>,
}

impl<'a, 'db> EqualityAnalysis<'a, 'db> {
    /// Creates a new equality analysis instance.
    pub fn new(db: &'db dyn Database, lowered: &'a Lowered<'db>) -> Self {
        let array_module = ModuleHelper::core(db).submodule("array");
        Self {
            db,
            lowered,
            array_new: array_module.extern_function_id("array_new"),
            array_append: array_module.extern_function_id("array_append"),
            array_pop_front: array_module.extern_function_id("array_pop_front"),
            array_pop_front_consume: array_module.extern_function_id("array_pop_front_consume"),
            array_snapshot_pop_front: array_module.extern_function_id("array_snapshot_pop_front"),
            array_snapshot_pop_back: array_module.extern_function_id("array_snapshot_pop_back"),
        }
    }

    /// Runs equality analysis on a lowered function.
    /// Returns the equality state at the exit of each block.
    pub fn analyze(
        db: &'db dyn Database,
        lowered: &'a Lowered<'db>,
    ) -> Vec<Option<EqualityState<'db>>> {
        ForwardDataflowAnalysis::new(lowered, EqualityAnalysis::new(db, lowered)).run()
    }

    /// Handles extern match arms for array operations.
    ///
    /// Array pop operations act as "destructures" on the struct-hashcons representation:
    /// - `array_pop_front` / `array_pop_front_consume`: On the Some arm, if the input array was
    ///   tracked as `[e0, e1, ..., eN]`, the popped element (boxed) is `Box(e0)` and the remaining
    ///   array is `[e1, ..., eN]`.
    /// - `array_snapshot_pop_front`: Same as above but through snapshot/box-of-snapshot wrappers.
    /// - `array_snapshot_pop_back`: Like pop_front but pops from the back: element is `Box(eN)`,
    ///   remaining is `[e0, ..., eN-1]`.
    fn transfer_extern_match_arm(
        &self,
        info: &mut EqualityState<'db>,
        extern_info: &MatchExternInfo<'db>,
        arm: &MatchArm<'db>,
    ) {
        let Some((id, _)) = extern_info.function.get_extern(self.db) else { return };
        // TODO(eytan-starkware): Add support for multipop.
        if id == self.array_pop_front || id == self.array_pop_front_consume {
            // Some arm: var_ids = [remaining_arr, boxed_elem]
            if arm.var_ids.len() == 2 {
                let input_arr = extern_info.inputs[0].var_id;
                let remaining_arr = arm.var_ids[0];
                let boxed_elem = arm.var_ids[1];

                let arr_rep = info.find(input_arr);
                if let Some((ty, elems)) = info.struct_hashcons_rev.get(&arr_rep).cloned()
                    && let Some((&first, rest)) = elems.split_first()
                {
                    // Popped element is boxed: boxed_elem = Box(first_element)
                    info.set_box_relationship(first, boxed_elem);
                    // Remaining array is the tail
                    let rest_reps: Vec<_> = rest.iter().map(|&v| info.find(v)).collect();
                    info.set_struct_construct(ty, rest_reps, remaining_arr);
                }
            } else {
                // None arm for array_pop_front: var_ids = [original_arr]. Union with input.
                // None arm for array_pop_front_consume: var_ids = [].
                let old_array_var = extern_info.inputs[0].var_id;
                let ty = self.lowered.variables[old_array_var].ty;
                // TODO(eytan-starkware): This introduces a backedge to our hashcons updates,
                //    so we might need to support updating the structures accordingly.
                //    For example, if this is empty after a pop, then we know previous array was a
                //    singleton.
                info.set_struct_construct(ty, vec![], old_array_var);
                if arm.var_ids.len() == 1 {
                    let empty_array_var = arm.var_ids[0];
                    info.union(old_array_var, empty_array_var);
                }
            }
        } else if id == self.array_snapshot_pop_front || id == self.array_snapshot_pop_back {
            // Some arm: var_ids = [remaining_snap_arr, boxed_snap_elem]
            if arm.var_ids.len() == 2 {
                let input_snap_arr = extern_info.inputs[0].var_id;
                let remaining_snap_arr = arm.var_ids[0];
                let boxed_snap_elem = arm.var_ids[1];

                // The input is @Array<T>. Look up the tracked elements.
                // Two paths: (1) the input snapshot was created via `snapshot(arr)` where `arr`
                // has a struct-hashcons entry under `Array<T>`, or (2) the input is itself a
                // remaining snapshot array from a prior snapshot pop, stored directly in the
                // struct hashcons under its snapshot type `@Array<T>`.
                let snap_rep = info.find(input_snap_arr);
                let elems_opt = info
                    .class_info
                    .get(&snap_rep)
                    .and_then(|ci| ci.original_class)
                    .and_then(|orig| {
                        let orig = info.find_immut(orig);
                        info.struct_hashcons_rev.get(&orig).cloned()
                    })
                    .or_else(|| info.struct_hashcons_rev.get(&snap_rep).cloned());

                if let Some((_orig_ty, elems)) = elems_opt {
                    let pop_front = id == self.array_snapshot_pop_front;
                    let (elem, rest) = if pop_front {
                        let Some((&first, tail)) = elems.split_first() else { return };
                        (first, tail.to_vec())
                    } else {
                        let Some((&last, init)) = elems.split_last() else { return };
                        (last, init.to_vec())
                    };

                    // The popped element is `Box<@T>`. The box wraps the *snapshot* of the
                    // original element. We can only record this relationship if a variable
                    // for `@elem` already exists (i.e., elem has a snapshot class).
                    // TODO(eytan-starkware): Support relationships even no variable to represent
                    // `@elem` yet.
                    let elem_rep = info.find(elem);
                    if let Some(snap_of_elem) =
                        info.class_info.get(&elem_rep).and_then(|ci| ci.snapshot_class)
                    {
                        info.set_box_relationship(snap_of_elem, boxed_snap_elem);
                    }

                    // Record the remaining snapshot array under its snapshot type
                    // (`@Array<T>`). This is a hack: `@Array<T>` is not a struct, but we
                    // reuse `struct_hashcons` to store element info for it. This also
                    // requires the two-path lookup above (path 2).
                    // TODO(eytan-starkware): Once placeholder vars are supported, store
                    //    this as a proper `Array<T>` linked via `original_class` instead,
                    //    since we may not have a var representing the non-snapshot array at the
                    // moment.
                    let snap_ty = self.lowered.variables[remaining_snap_arr].ty;
                    let rest_reps: Vec<_> = rest.iter().map(|&v| info.find(v)).collect();
                    info.set_struct_construct(snap_ty, rest_reps, remaining_snap_arr);
                }
            } else {
                // None arm: var_ids = [original_snap_arr]. Snapshot array was empty.
                let old_snap_arr = extern_info.inputs[0].var_id;
                let snap_ty = self.lowered.variables[old_snap_arr].ty;
                info.set_struct_construct(snap_ty, vec![], old_snap_arr);
                info.union(arm.var_ids[0], old_snap_arr);
            }
        }
    }
}

/// Returns an iterator over all variables with equality ir relationship information in the equality
/// states.
fn merge_referenced_vars<'db, 'a>(
    info1: &'a EqualityState<'db>,
    info2: &'a EqualityState<'db>,
) -> impl Iterator<Item = VariableId> + 'a {
    let union_find_vars = info1.union_find.keys().chain(info2.union_find.keys()).copied();

    let class_info_vars = info1
        .class_info
        .values()
        .chain(info2.class_info.values())
        .flat_map(ClassInfo::referenced_vars);

    let enum_vars = info1
        .enum_hashcons
        .iter()
        .chain(info2.enum_hashcons.iter())
        .flat_map(|(&(_, input), &output)| [input, output]);

    let struct_vars =
        info1.struct_hashcons.iter().chain(info2.struct_hashcons.iter()).flat_map(
            |((_, inputs), &output)| inputs.iter().copied().chain(std::iter::once(output)),
        );

    union_find_vars.chain(class_info_vars).chain(enum_vars).chain(struct_vars)
}

/// Preserves only class relationships (box/snapshot) that exist in both branches.
fn merge_class_relationships(
    info1: &EqualityState<'_>,
    info2: &EqualityState<'_>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState<'_>,
) {
    for (&var, class1) in info1.class_info.iter() {
        for &(var_rep2, intersection_var) in intersections.get(&var).unwrap_or(&vec![]) {
            let Some(class2) = info2.class_info.get(&var_rep2) else {
                continue;
            };

            if let Some(boxed_rep) = find_intersection_rep_opt(
                info1,
                info2,
                intersections,
                class1.boxed_class,
                class2.boxed_class,
            ) {
                result.set_box_relationship(intersection_var, boxed_rep);
            }

            if let Some(snap_rep) = find_intersection_rep_opt(
                info1,
                info2,
                intersections,
                class1.snapshot_class,
                class2.snapshot_class,
            ) {
                result.set_snapshot_relationship(intersection_var, snap_rep);
            }
        }
    }
}

/// Finds an intersection representative: given a rep in info1 and a rep in info2,
/// returns the intersection representative in the result if one exists.
fn find_intersection_rep(
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    rep1: VariableId,
    rep2: VariableId,
) -> Option<VariableId> {
    intersections.get(&rep1)?.iter().find_map(|(intersection_r2, intersection_rep)| {
        (*intersection_r2 == rep2).then_some(*intersection_rep)
    })
}

/// Like [`find_intersection_rep`], but accepts optional reps and resolves them through the
/// respective states. Returns `None` if either rep is `None` or no intersection exists.
fn find_intersection_rep_opt(
    info1: &EqualityState<'_>,
    info2: &EqualityState<'_>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    rep1: Option<VariableId>,
    rep2: Option<VariableId>,
) -> Option<VariableId> {
    find_intersection_rep(intersections, info1.find_immut(rep1?), info2.find_immut(rep2?))
}

/// Preserves enum hashcons entries that exist in both branches.
/// An entry survives if both input and output have intersection representatives, and info2 has the
/// same relation.
fn merge_enum_hashcons<'db>(
    info1: &EqualityState<'db>,
    info2: &EqualityState<'db>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState<'db>,
) {
    for (&(variant, input1), &output1) in info1.enum_hashcons.iter() {
        for &(input_rep2, input_intersection) in intersections.get(&input1).unwrap_or(&vec![]) {
            let output2 = info2.enum_hashcons.get(&(variant, input_rep2)).copied();
            let Some(output_intersection) =
                find_intersection_rep_opt(info1, info2, intersections, Some(output1), output2)
            else {
                continue;
            };

            result.set_enum_construct(variant, input_intersection, output_intersection);
        }
    }
}

/// Preserves struct hashcons entries that exist in both branches.
/// An entry survives if all inputs and output have intersection representatives, and info2 has the
/// same relation.
fn merge_struct_hashcons<'db>(
    info1: &EqualityState<'db>,
    info2: &EqualityState<'db>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState<'db>,
) {
    for ((ty, inputs1), &output1) in info1.struct_hashcons.iter() {
        let input_reps2: Vec<_> = inputs1.iter().map(|&v| info2.find_immut(v)).collect();

        let Some(&output2) = info2.struct_hashcons.get(&(*ty, input_reps2.clone())) else {
            continue;
        };

        let result_inputs: Option<Vec<_>> = inputs1
            .iter()
            .zip(&input_reps2)
            .map(|(&v, &rep2)| {
                find_intersection_rep(intersections, info1.find_immut(v), info2.find_immut(rep2))
            })
            .collect();
        let Some(result_inputs) = result_inputs else {
            continue;
        };

        let Some(output_intersection) = find_intersection_rep(
            intersections,
            info1.find_immut(output1),
            info2.find_immut(output2),
        ) else {
            continue;
        };

        result.set_struct_construct(*ty, result_inputs, output_intersection);
    }
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for EqualityAnalysis<'a, 'db> {
    type Info = EqualityState<'db>;

    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        EqualityState::default()
    }

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: super::StatementLocation,
        info1: Self::Info,
        info2: Self::Info,
    ) -> Self::Info {
        // Intersection-based merge: keep only equalities that hold in BOTH branches.
        let mut result = EqualityState::default();

        // Group variables by (rep1, rep2) - for variables present in either state.
        let mut groups: OrderedHashMap<(VariableId, VariableId), Vec<VariableId>> =
            OrderedHashMap::default();

        // Group by (rep1, rep2). Duplicates are fine - they'll just be added to the same group.
        for var in merge_referenced_vars(&info1, &info2) {
            let key = (info1.find_immut(var), info2.find_immut(var));
            groups.entry(key).or_default().push(var);
        }

        // Union all variables within each group
        for members in groups.values() {
            if members.len() > 1 {
                let first = members[0];
                for &var in &members[1..] {
                    result.union(first, var);
                }
            }
        }

        // An important point in this merge is to retain relationships.
        // Consider:
        //  info1 [equality class[1] = 1, 2, 4] and 6 is Box(1).
        //  info2 [equality class[2] = 3, 5, 4] and 6 is Box(3).
        // To detect we can keep 6 is Box(4), as it is true in all branches, we need intersection of
        // eclasses (a single eclass can split in the result into multiple eclasses).
        // Build secondary index: rep1 -> Vec<(rep2, intersection_rep)>.
        let mut intersections: OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>> =
            OrderedHashMap::default();
        for (&(rep1, rep2), vars) in groups.iter() {
            intersections.entry(rep1).or_default().push((rep2, result.find(vars[0])));
        }

        merge_class_relationships(&info1, &info2, &intersections, &mut result);

        merge_enum_hashcons(&info1, &info2, &intersections, &mut result);

        merge_struct_hashcons(&info1, &info2, &intersections, &mut result);

        result
    }

    fn transfer_stmt(
        &mut self,
        info: &mut Self::Info,
        _statement_location: super::StatementLocation,
        stmt: &'a Statement<'db>,
    ) {
        match stmt {
            Statement::Snapshot(snapshot_stmt) => {
                info.union(snapshot_stmt.original(), snapshot_stmt.input.var_id);
                info.set_snapshot_relationship(
                    snapshot_stmt.input.var_id,
                    snapshot_stmt.snapshot(),
                );
            }

            Statement::Desnap(desnap_stmt) => {
                info.set_snapshot_relationship(desnap_stmt.output, desnap_stmt.input.var_id);
            }

            Statement::IntoBox(into_box_stmt) => {
                info.set_box_relationship(into_box_stmt.input.var_id, into_box_stmt.output);
            }

            Statement::Unbox(unbox_stmt) => {
                info.set_box_relationship(unbox_stmt.output, unbox_stmt.input.var_id);
            }

            Statement::EnumConstruct(enum_stmt) => {
                // output = Variant(input): track via hashcons
                // If we've already seen this variant with an equivalent input, the outputs are
                // equal.
                info.set_enum_construct(
                    enum_stmt.variant,
                    enum_stmt.input.var_id,
                    enum_stmt.output,
                );
            }

            Statement::StructConstruct(struct_stmt) => {
                // output = StructType(inputs...): track via hashcons
                // If we've already seen the same struct type with equivalent inputs, the outputs
                // are equal.
                let ty = self.lowered.variables[struct_stmt.output].ty;
                let input_reps = struct_stmt.inputs.iter().map(|i| info.find(i.var_id)).collect();
                info.set_struct_construct(ty, input_reps, struct_stmt.output);
            }

            Statement::StructDestructure(struct_stmt) => {
                // (outputs...) = struct_destructure(input)
                // 1. If input was previously constructed, union outputs with original fields.
                let input_rep = info.find(struct_stmt.input.var_id);
                if let Some((_, field_reps)) = info.struct_hashcons_rev.get(&input_rep).cloned() {
                    for (&output, &field_rep) in struct_stmt.outputs.iter().zip(field_reps.iter()) {
                        info.union(output, field_rep);
                    }
                }
                // 2. Record: struct_construct(outputs) == input (for future constructs).
                let ty = self.lowered.variables[struct_stmt.input.var_id].ty;
                let output_reps = struct_stmt.outputs.iter().map(|&v| info.find(v)).collect();
                info.set_struct_construct(ty, output_reps, struct_stmt.input.var_id);
            }

            Statement::Call(call_stmt) => {
                let Some((id, _)) = call_stmt.function.get_extern(self.db) else { return };
                if id == self.array_new {
                    let ty = self.lowered.variables[call_stmt.outputs[0]].ty;
                    info.set_struct_construct(ty, vec![], call_stmt.outputs[0]);
                } else if id == self.array_append {
                    let arr_rep = info.find(call_stmt.inputs[0].var_id);
                    if let Some((ty, elems)) = info.struct_hashcons_rev.get(&arr_rep).cloned() {
                        let mut new_elems = elems;
                        new_elems.push(info.find(call_stmt.inputs[1].var_id));
                        info.set_struct_construct(ty, new_elems, call_stmt.outputs[0]);
                    }
                }
            }

            Statement::Const(_) => {}
        }
    }

    fn transfer_edge(&mut self, info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        let mut new_info = info.clone();
        match edge {
            Edge::Goto { remapping, .. } => {
                // Union remapped variables: dst and src should be in the same equivalence class
                for (dst, src_usage) in remapping.iter() {
                    new_info.union(*dst, src_usage.var_id);
                }
            }
            Edge::MatchArm { arm, match_info } => {
                // For enum matches, track that matched_var = Variant(arm_var).
                if let MatchInfo::Enum(enum_info) = match_info
                    && let MatchArmSelector::VariantId(variant) = arm.arm_selector
                    && let [arm_var] = arm.var_ids[..]
                {
                    let matched_var = enum_info.input.var_id;

                    // If we previously saw this enum constructed with the same variant,
                    // union with the original input. Skip if variants differ — this can
                    // happen after optimizations merge states from different branches.
                    let output_rep = new_info.find(matched_var);
                    if let Some((old_variant, input)) =
                        new_info.enum_hashcons_rev.get(&output_rep).copied()
                        && variant == old_variant
                    {
                        new_info.union(arm_var, input);
                    }

                    // Record the relationship: matched_var = Variant(arm_var)
                    new_info.set_enum_construct(variant, arm_var, matched_var);
                }

                // For extern matches on array operations, track pop/destructure relationships.
                if let MatchInfo::Extern(extern_info) = match_info {
                    self.transfer_extern_match_arm(&mut new_info, extern_info, arm);
                }
            }
            Edge::Return { .. } | Edge::Panic { .. } => {}
        }
        new_info
    }
}
