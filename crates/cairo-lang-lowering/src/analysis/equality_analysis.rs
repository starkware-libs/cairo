//! Equality analysis for lowered IR.
//!
//! This module tracks semantic equivalence between variables as information flows through the
//! program. Two variables are equivalent if they hold the same value. Additionally, the analysis
//! tracks `Box`/unbox, snapshot/desnap, and struct/array construct relationships between
//! equivalence classes via unified forward/reverse maps. Arrays reuse the struct construct
//! representation since both map `(TypeId, Vec<VariableId>)` — array pop operations act as
//! destructures.

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{ExternFunctionId, NamedLanguageElementId};
use cairo_lang_semantic::corelib::option_some_variant;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::{ConcreteVariant, GenericArgumentId, MatchArmSelector, TypeId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;

use crate::analysis::core::Edge;
use crate::analysis::{DataflowAnalyzer, Direction, ForwardDataflowAnalysis};
use crate::{
    BlockEnd, BlockId, Lowered, MatchArm, MatchExternInfo, MatchInfo, Statement, VariableId,
};

/// A relationship between equivalence classes, carrying its payload data.
/// Hashable so it can be used as a forward map key.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Relation<'db> {
    Box(VariableId),
    Snapshot(VariableId),
    EnumConstruct(ConcreteVariant<'db>, VariableId),
    StructConstruct(TypeId<'db>, Vec<VariableId>),
}

impl<'db> Relation<'db> {
    /// Returns an iterator over all variables referenced by this relation.
    fn referenced_vars(&self) -> impl Iterator<Item = VariableId> + '_ {
        let (single, fields): (Option<VariableId>, &[VariableId]) = match self {
            Relation::Box(v) | Relation::Snapshot(v) | Relation::EnumConstruct(_, v) => {
                (Some(*v), &[])
            }
            Relation::StructConstruct(_, vs) => (None, vs),
        };
        single.into_iter().chain(fields.iter().copied())
    }

    /// Returns a new Relation with all input variables resolved to their current representatives.
    fn with_fresh_reps(self, state: &mut EqualityState<'_>) -> Self {
        match self {
            Relation::Box(v) => Relation::Box(state.find(v)),
            Relation::Snapshot(v) => Relation::Snapshot(state.find(v)),
            Relation::EnumConstruct(variant, v) => Relation::EnumConstruct(variant, state.find(v)),
            Relation::StructConstruct(ty, fields) => {
                Relation::StructConstruct(ty, fields.into_iter().map(|v| state.find(v)).collect())
            }
        }
    }

    /// Merges two reverse relationships (self and other are proven equal).
    /// When both exist with the same kind, propagates equality through inputs.
    /// Always returns a valid relation (self if no merge needed).
    fn union_equal_relations(self, other: Option<Self>, uf: &mut EqualityState<'_>) -> Self {
        let Some(other_rel) = other else { return self };
        match (&self, &other_rel) {
            (Relation::Box(a), Relation::Box(b)) if a != b => Relation::Box(uf.union(*a, *b)),
            (Relation::Snapshot(a), Relation::Snapshot(b)) if a != b => {
                Relation::Snapshot(uf.union(*a, *b))
            }
            (Relation::EnumConstruct(v1, a), Relation::EnumConstruct(v2, b))
                if v1 == v2 && a != b =>
            {
                Relation::EnumConstruct(*v1, uf.union(*a, *b))
            }
            (Relation::StructConstruct(t1, a), Relation::StructConstruct(t2, b))
                if t1 == t2 && a.len() == b.len() =>
            {
                Relation::StructConstruct(
                    *t1,
                    a.iter().zip(b).map(|(x1, x2)| uf.union(*x1, *x2)).collect(),
                )
            }
            // Same values or different kinds: keep self.
            _ => self,
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

    /// Forward map: Relation(...) -> output representative.
    ///
    /// Keys use representatives at insertion time. In SSA form, representatives are generally
    /// stable within a block, so keys stay valid without migration during `union`. A union
    /// *can* change a representative to a lower ID, which may cause a subsequent identical
    /// construct to miss the earlier entry — this is a known imprecision (conservative, not
    /// unsound). At merge points the maps are rebuilt from scratch.
    forward: OrderedHashMap<Relation<'db>, VariableId>,

    /// Reverse map: output representative -> Relation.
    /// Records how each class was produced. A class has at most one reverse relationship.
    reverse: OrderedHashMap<VariableId, Relation<'db>>,
}

impl<'db> EqualityState<'db> {
    /// Gets the parent of a variable, defaulting to itself (root) if not in the map.
    fn get_parent(&self, var: VariableId) -> VariableId {
        self.union_find.get(&var).copied().unwrap_or(var)
    }

    /// Finds the representative of a variable's equivalence class.
    /// Uses path splitting for efficiency: each node is redirected to its grandparent.
    fn find(&mut self, mut var: VariableId) -> VariableId {
        let mut parent = self.get_parent(var);
        while parent != var {
            let grandparent = self.get_parent(parent);
            self.union_find.insert(var, grandparent);
            var = parent;
            parent = grandparent;
        }
        var
    }

    /// Finds the representative without modifying the structure.
    pub(crate) fn find_immut(&self, mut var: VariableId) -> VariableId {
        let mut parent = self.get_parent(var);
        while parent != var {
            var = parent;
            parent = self.get_parent(var);
        }
        var
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
        // This ensures forward map keys remain valid since lower IDs are defined earlier.
        let (new_root, old_root) =
            if root_a.index() < root_b.index() { (root_a, root_b) } else { (root_b, root_a) };

        // Ensure new_root is in the map (as its own parent).
        self.union_find.entry(new_root).or_insert(new_root);
        // Update old_root to point to new_root.
        self.union_find.insert(old_root, new_root);

        // Merge reverse entries for both roots.
        let old_reverse = self.reverse.swap_remove(&old_root);
        let new_reverse = self.reverse.swap_remove(&new_root);
        let merged_reverse = match (new_reverse, old_reverse) {
            (Some(new_rev), old) => Some(new_rev.union_equal_relations(old, self)),
            (None, old) => old,
        };

        // TODO(eytan-starkware): Forward Struct/Enum entries aren't re-keyed during union.
        //    Doing so would require congruence closure (updating all entries whose inputs
        //    changed). The consequence is missed hashcons hits — conservative, not unsound.
        //    We also don't invalidate forward entries that become inconsistent (e.g., two
        //    different enum variants mapping to the now-merged class). These stale entries
        //    are harmless as this code should be unreachable.
        // Merge forward Box/Snapshot entries for both roots.
        let constructors = [Relation::Box, Relation::Snapshot];
        for ctor in constructors {
            let old_fwd = self.forward.swap_remove(&ctor(old_root));
            let new_fwd = self.forward.swap_remove(&ctor(new_root));
            let merged = match (new_fwd, old_fwd) {
                (Some(t1), Some(t2)) => Some(self.union(t1, t2)),
                (Some(t), None) | (None, Some(t)) => Some(t),
                (None, None) => None,
            };
            if let Some(target) = merged {
                let final_root = self.find(new_root);
                let target_rep = self.find(target);
                self.forward.insert(ctor(final_root), target_rep);
            }
        }

        let final_root = self.find(new_root);
        if let Some(merged_reverse) = merged_reverse {
            self.reverse.insert(final_root, merged_reverse);
        }

        self.find(new_root)
    }

    /// Records a relation: forward maps `relation -> output`, reverse maps `output -> relation`.
    /// If the same relation already maps to an existing output, unions them.
    /// If the output already has a reverse, merges inputs via `union_equal_relations`.
    fn set_relation(&mut self, relation: Relation<'db>, output: VariableId) {
        // Refresh reps — callers may pass stale IDs, and this maximizes forward hits.
        let relation = relation.with_fresh_reps(self);

        // Forward dedup: if this exact relation already maps to an output, union them.
        if let Some(&existing_output) = self.forward.get(&relation) {
            self.union(existing_output, output);
        }

        // Reverse merge: if output already has a reverse, merge inputs and use the result.
        let output_rep = self.find(output);
        let existing = self.reverse.swap_remove(&output_rep);
        let relation = relation.union_equal_relations(existing, self);

        // Insert with current reps (may be slightly stale after unions above).
        let output_rep = self.find(output);
        self.forward.insert(relation.clone(), output_rep);
        self.reverse.insert(output_rep, relation);
    }

    /// Looks up the struct construct info for a representative (immutable).
    fn get_struct_construct_immut(
        &self,
        rep: VariableId,
    ) -> Option<(TypeId<'db>, Vec<VariableId>)> {
        match self.reverse.get(&rep)? {
            Relation::StructConstruct(ty, fields) => Some((*ty, fields.clone())),
            _ => None,
        }
    }

    /// Looks up the struct construct info for a variable (mutable, uses find for path compression).
    fn get_struct_construct(&mut self, var: VariableId) -> Option<(TypeId<'db>, Vec<VariableId>)> {
        let rep = self.find(var);
        self.get_struct_construct_immut(rep)
    }

    /// Looks up the enum construct info for a representative (immutable).
    fn get_enum_construct_immut(
        &self,
        rep: VariableId,
    ) -> Option<(ConcreteVariant<'db>, VariableId)> {
        match self.reverse.get(&rep)? {
            Relation::EnumConstruct(variant, input) => Some((*variant, *input)),
            _ => None,
        }
    }
}

impl<'db> DebugWithDb<'db> for EqualityState<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let v = |id: VariableId| format!("v{}", self.find_immut(id).index());
        let mut lines = Vec::<String>::new();
        for (relation, &output) in self.forward.iter() {
            match relation {
                Relation::Snapshot(source) => {
                    lines.push(format!("@{} = {}", v(*source), v(output)));
                }
                Relation::Box(source) => {
                    lines.push(format!("Box({}) = {}", v(*source), v(output)));
                }
                Relation::EnumConstruct(variant, input) => {
                    let name = variant.id.name(db).to_string(db);
                    lines.push(format!("{name}({}) = {}", v(*input), v(output)));
                }
                Relation::StructConstruct(ty, inputs) => {
                    let type_name = ty.format(db);
                    let fields = inputs.iter().map(|&id| v(id)).collect::<Vec<_>>().join(", ");
                    lines.push(format!("{type_name}({fields}) = {}", v(output)));
                }
            }
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
    /// Array pop operations act as "destructures" on the struct construct representation:
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
        let MatchArmSelector::VariantId(variant) = arm.arm_selector else { return };
        if id == self.array_pop_front
            || id == self.array_pop_front_consume
            || id == self.array_snapshot_pop_front
            || id == self.array_snapshot_pop_back
        {
            let [GenericArgumentId::Type(option_ty)] =
                variant.concrete_enum_id.long(self.db).generic_args[..]
            else {
                panic!("Expected Option<T> with a single type argument");
            };
            let some_variant = option_some_variant(self.db, option_ty);
            assert_eq!(
                variant.concrete_enum_id.enum_id(self.db),
                some_variant.concrete_enum_id.enum_id(self.db),
                "Expected match to be on an Option<T>"
            );
            self.transfer_array_pop_arm(info, extern_info, arm, id, variant == some_variant);
        }
    }

    /// Handles the actual array pop arm transfer after validating the Option variant.
    fn transfer_array_pop_arm(
        &self,
        info: &mut EqualityState<'db>,
        extern_info: &MatchExternInfo<'db>,
        arm: &MatchArm<'db>,
        id: ExternFunctionId<'db>,
        is_some: bool,
    ) {
        if id == self.array_pop_front || id == self.array_pop_front_consume {
            if is_some {
                // Some arm: var_ids = [remaining_arr, boxed_elem]
                let input_arr = extern_info.inputs[0].var_id;
                let remaining_arr = arm.var_ids[0];
                let boxed_elem = arm.var_ids[1];
                if let Some((ty, elems)) = info.get_struct_construct(input_arr)
                    && let Some((&first, rest)) = elems.split_first()
                {
                    info.set_relation(Relation::Box(first), boxed_elem);
                    let rest_reps: Vec<_> = rest.iter().map(|&v| info.find(v)).collect();
                    info.set_relation(Relation::StructConstruct(ty, rest_reps), remaining_arr);
                }
            } else {
                // None arm for array_pop_front: var_ids = [original_arr]. Union with input.
                // None arm for array_pop_front_consume: var_ids = [].
                let old_array_var = extern_info.inputs[0].var_id;
                let ty = self.lowered.variables[old_array_var].ty;
                // TODO(eytan-starkware): This introduces a backedge to our forward map updates,
                //    so we might need to support updating the structures accordingly.
                //    For example, if this is empty after a pop, then we know previous array
                //    was a singleton.
                info.set_relation(Relation::StructConstruct(ty, vec![]), old_array_var);
                if let [original_arr] = arm.var_ids[..] {
                    info.union(original_arr, old_array_var);
                }
            }
        } else if id == self.array_snapshot_pop_front || id == self.array_snapshot_pop_back {
            if is_some {
                // Some arm: var_ids = [remaining_snap_arr, boxed_snap_elem]
                let input_snap_arr = extern_info.inputs[0].var_id;
                let remaining_snap_arr = arm.var_ids[0];
                let boxed_snap_elem = arm.var_ids[1];

                // Look up tracked elements via snapshot reverse relationship or direct lookup.
                let snap_rep = info.find(input_snap_arr);
                let original_rep = match info.reverse.get(&snap_rep) {
                    Some(Relation::Snapshot(v)) => Some(*v),
                    _ => None,
                };
                let elems_opt = original_rep
                    .and_then(|orig| {
                        let orig = info.find_immut(orig);
                        info.get_struct_construct_immut(orig)
                    })
                    .or_else(|| info.get_struct_construct_immut(snap_rep));

                if let Some((_orig_ty, elems)) = elems_opt {
                    let pop_front = id == self.array_snapshot_pop_front;
                    let (elem, rest) = if pop_front {
                        let Some((&first, tail)) = elems.split_first() else { return };
                        (first, tail)
                    } else {
                        let Some((&last, init)) = elems.split_last() else { return };
                        (last, init)
                    };

                    // The popped element is `Box<@T>`. Record the box relationship against
                    // the snapshot class of `elem` if it exists.
                    // TODO(eytan-starkware): Support relationships even when no variable
                    // represents `@elem` yet.
                    let elem_rep = info.find(elem);
                    if let Some(&snap_of_elem) = info.forward.get(&Relation::Snapshot(elem_rep)) {
                        info.set_relation(Relation::Box(snap_of_elem), boxed_snap_elem);
                    }

                    // Record the remaining snapshot array under its snapshot type
                    // (`@Array<T>`). This is a hack: `@Array<T>` is not a struct, but we
                    // reuse struct construct tracking to store element info for it. This
                    // also requires the two-path lookup above (path 2).
                    // TODO(eytan-starkware): Once placeholder vars are supported, store
                    //    this as a proper `Array<T>` linked via the reverse map instead,
                    //    since we may not have a var representing the non-snapshot array
                    //    at the moment.
                    let snap_ty = self.lowered.variables[remaining_snap_arr].ty;
                    let rest_reps: Vec<_> = rest.iter().map(|&v| info.find(v)).collect();
                    info.set_relation(
                        Relation::StructConstruct(snap_ty, rest_reps),
                        remaining_snap_arr,
                    );
                }
            } else {
                // None arm: record empty snapshot array and union output with input.
                let old_snap_arr = extern_info.inputs[0].var_id;
                let snap_ty = self.lowered.variables[old_snap_arr].ty;
                info.set_relation(Relation::StructConstruct(snap_ty, vec![]), old_snap_arr);
                if let [original_snap_arr] = arm.var_ids[..] {
                    info.union(original_snap_arr, old_snap_arr);
                }
            }
        }
    }
}

/// Returns an iterator over all variables with equality or relationship information in the equality
/// states.
fn merge_referenced_vars<'db, 'a>(
    info1: &'a EqualityState<'db>,
    info2: &'a EqualityState<'db>,
) -> impl Iterator<Item = VariableId> + 'a {
    let union_find_vars = info1.union_find.keys().chain(info2.union_find.keys()).copied();

    let forward_vars =
        info1.forward.iter().chain(info2.forward.iter()).flat_map(|(relation, &output)| {
            relation.referenced_vars().chain(std::iter::once(output))
        });

    let reverse_vars = info1
        .reverse
        .iter()
        .chain(info2.reverse.iter())
        .flat_map(|(&rep, relation)| std::iter::once(rep).chain(relation.referenced_vars()));

    union_find_vars.chain(forward_vars).chain(reverse_vars)
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

/// Preserves relations that exist in both branches.
/// All relation types are looked up via the forward map, which retains all entries.
fn merge_relations<'db>(
    info1: &EqualityState<'db>,
    info2: &EqualityState<'db>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState<'db>,
) {
    // Iterate all forward entries from info1 and find matching entries in info2.
    // We use forward (not reverse) because reverse holds only the latest relation per output,
    // while forward retains all entries and is the authoritative source.
    for (relation, &output1) in info1.forward.iter() {
        match relation {
            Relation::Box(source1) | Relation::Snapshot(source1) => {
                for &(source2, intersection_var) in intersections.get(source1).unwrap_or(&vec![]) {
                    let relation2 = match relation {
                        Relation::Box(_) => Relation::Box(source2),
                        Relation::Snapshot(_) => Relation::Snapshot(source2),
                        _ => unreachable!(),
                    };
                    let Some(&output2) = info2.forward.get(&relation2) else { continue };
                    if let Some(output_intersection) = find_intersection_rep(
                        intersections,
                        info1.find_immut(output1),
                        info2.find_immut(output2),
                    ) {
                        let result_relation = match relation {
                            Relation::Box(_) => Relation::Box(result.find(intersection_var)),
                            Relation::Snapshot(_) => {
                                Relation::Snapshot(result.find(intersection_var))
                            }
                            _ => unreachable!(),
                        };
                        result.set_relation(result_relation, output_intersection);
                    }
                }
            }
            Relation::EnumConstruct(variant, input1) => {
                for &(input2, input_intersection) in
                    intersections.get(&info1.find_immut(*input1)).unwrap_or(&vec![])
                {
                    let relation2 = Relation::EnumConstruct(*variant, input2);
                    let Some(&output2) = info2.forward.get(&relation2) else { continue };
                    if let Some(output_intersection) = find_intersection_rep(
                        intersections,
                        info1.find_immut(output1),
                        info2.find_immut(output2),
                    ) {
                        result.set_relation(
                            Relation::EnumConstruct(*variant, input_intersection),
                            output_intersection,
                        );
                    }
                }
            }
            Relation::StructConstruct(ty, fields1) => {
                let fields2: Vec<_> = fields1.iter().map(|&v| info2.find_immut(v)).collect();
                let Some(&output2) =
                    info2.forward.get(&Relation::StructConstruct(*ty, fields2.clone()))
                else {
                    continue;
                };
                let result_fields: Option<Vec<_>> = fields1
                    .iter()
                    .zip(&fields2)
                    .map(|(&v1, &v2)| {
                        find_intersection_rep(intersections, info1.find_immut(v1), v2)
                    })
                    .collect();
                let Some(result_fields) = result_fields else { continue };
                if let Some(output_intersection) = find_intersection_rep(
                    intersections,
                    info1.find_immut(output1),
                    info2.find_immut(output2),
                ) {
                    result.set_relation(
                        Relation::StructConstruct(*ty, result_fields),
                        output_intersection,
                    );
                }
            }
        }
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

        merge_relations(&info1, &info2, &intersections, &mut result);

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
                info.set_relation(
                    Relation::Snapshot(snapshot_stmt.input.var_id),
                    snapshot_stmt.snapshot(),
                );
            }

            Statement::Desnap(desnap_stmt) => {
                info.set_relation(Relation::Snapshot(desnap_stmt.output), desnap_stmt.input.var_id);
            }

            Statement::IntoBox(into_box_stmt) => {
                info.set_relation(Relation::Box(into_box_stmt.input.var_id), into_box_stmt.output);
            }

            Statement::Unbox(unbox_stmt) => {
                info.set_relation(Relation::Box(unbox_stmt.output), unbox_stmt.input.var_id);
            }

            Statement::EnumConstruct(enum_stmt) => {
                // output = Variant(input): track via forward map
                // If we've already seen this variant with an equivalent input, the outputs are
                // equal.
                info.set_relation(
                    Relation::EnumConstruct(enum_stmt.variant, enum_stmt.input.var_id),
                    enum_stmt.output,
                );
            }

            Statement::StructConstruct(struct_stmt) => {
                // output = StructType(inputs...): track via forward map
                // If we've already seen the same struct type with equivalent inputs, the outputs
                // are equal.
                let ty = self.lowered.variables[struct_stmt.output].ty;
                let input_reps = struct_stmt.inputs.iter().map(|i| info.find(i.var_id)).collect();
                info.set_relation(Relation::StructConstruct(ty, input_reps), struct_stmt.output);
            }

            Statement::StructDestructure(struct_stmt) => {
                // (outputs...) = struct_destructure(input)
                // 1. If input was previously constructed, union outputs with original fields.
                if let Some((_, field_reps)) = info.get_struct_construct(struct_stmt.input.var_id) {
                    for (&output, &field_rep) in struct_stmt.outputs.iter().zip(field_reps.iter()) {
                        info.union(output, field_rep);
                    }
                }
                // 2. Record: struct_construct(outputs) == input (for future constructs).
                let ty = self.lowered.variables[struct_stmt.input.var_id].ty;
                let output_reps = struct_stmt.outputs.iter().map(|&v| info.find(v)).collect();
                info.set_relation(
                    Relation::StructConstruct(ty, output_reps),
                    struct_stmt.input.var_id,
                );
            }

            Statement::Call(call_stmt) => {
                let Some((id, _)) = call_stmt.function.get_extern(self.db) else { return };
                if id == self.array_new {
                    let ty = self.lowered.variables[call_stmt.outputs[0]].ty;
                    info.set_relation(Relation::StructConstruct(ty, vec![]), call_stmt.outputs[0]);
                } else if id == self.array_append
                    && let Some((ty, elems)) = info.get_struct_construct(call_stmt.inputs[0].var_id)
                {
                    // Only track append if the input array is already tracked. Arrays from
                    // function parameters or external calls are conservatively ignored.
                    let mut new_elems = elems;
                    new_elems.push(info.find(call_stmt.inputs[1].var_id));
                    info.set_relation(
                        Relation::StructConstruct(ty, new_elems),
                        call_stmt.outputs[0],
                    );
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
                        new_info.get_enum_construct_immut(output_rep)
                        && variant == old_variant
                    {
                        new_info.union(arm_var, input);
                    }

                    // Record the relationship: matched_var = Variant(arm_var)
                    new_info.set_relation(Relation::EnumConstruct(variant, arm_var), matched_var);
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
