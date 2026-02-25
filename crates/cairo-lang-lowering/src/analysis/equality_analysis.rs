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
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use salsa::Database;

use crate::analysis::core::Edge;
use crate::analysis::{DataflowAnalyzer, Direction, ForwardDataflowAnalysis};
use crate::{
    BlockEnd, BlockId, Lowered, MatchArm, MatchExternInfo, MatchInfo, Statement, VariableId,
};

/// The kind of relationship between equivalence classes.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum RelationKind {
    Box,
    Snapshot,
    EnumConstruct,
    StructConstruct,
}

/// A relationship between equivalence classes, carrying its payload data.
/// Hashable so it can be used as a hashcons key.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Relation<'db> {
    Box(VariableId),
    Snapshot(VariableId),
    EnumConstruct(ConcreteVariant<'db>, VariableId),
    StructConstruct(TypeId<'db>, Vec<VariableId>),
}

impl<'db> Relation<'db> {
    fn kind(&self) -> RelationKind {
        match self {
            Relation::Box(_) => RelationKind::Box,
            Relation::Snapshot(_) => RelationKind::Snapshot,
            Relation::EnumConstruct(_, _) => RelationKind::EnumConstruct,
            Relation::StructConstruct(_, _) => RelationKind::StructConstruct,
        }
    }

    /// Extracts the single variable for simple (1-to-1) relations.
    fn single_var(&self) -> Option<VariableId> {
        match self {
            Relation::Box(v) | Relation::Snapshot(v) | Relation::EnumConstruct(_, v) => Some(*v),
            Relation::StructConstruct(_, _) => None,
        }
    }

    /// Returns an iterator over all variables referenced by this relation.
    fn referenced_vars(&self) -> impl Iterator<Item = VariableId> + '_ {
        let fields: &[VariableId] = match self {
            Relation::StructConstruct(_, vs) => vs,
            _ => &[],
        };
        self.single_var().into_iter().chain(fields.iter().copied())
    }

    /// Creates a new relation of the same kind with the single variable replaced.
    /// Panics for StructConstruct (which has multiple variables).
    fn with_var(&self, var: VariableId) -> Self {
        match self {
            Relation::Box(_) => Relation::Box(var),
            Relation::Snapshot(_) => Relation::Snapshot(var),
            Relation::EnumConstruct(v, _) => Relation::EnumConstruct(*v, var),
            Relation::StructConstruct(_, _) => {
                unreachable!("with_var not supported for StructConstruct")
            }
        }
    }
}

/// Tracks relationships and construct provenance for an equivalence class representative.
#[derive(Clone, Debug, Default)]
struct ClassInfo<'db> {
    /// Forward relationships: this class → target class.
    /// Box: my boxed version is `target`. Snapshot: my snapshot version is `target`.
    /// Only used for Box and Snapshot (1-to-1 bidirectional relationships).
    relationship: OrderedHashMap<RelationKind, VariableId>,
    /// Reverse relationships: how this class was produced.
    /// Box: I was produced by boxing `source`. Snapshot: I am a snapshot of `source`.
    /// EnumConstruct: I was produced by Variant(`input`).
    /// StructConstruct: I was produced by Type(`fields`).
    reverse_relationship: OrderedHashMap<RelationKind, Relation<'db>>,
}

impl<'db> ClassInfo<'db> {
    /// Returns all variables referenced by this ClassInfo's relationships.
    fn referenced_vars(&self) -> impl Iterator<Item = VariableId> + '_ {
        self.relationship
            .values()
            .copied()
            .chain(self.reverse_relationship.values().flat_map(Relation::referenced_vars))
    }

    /// Returns true if this ClassInfo has no relationships or construct info.
    fn is_empty(&self) -> bool {
        self.relationship.is_empty() && self.reverse_relationship.is_empty()
    }

    /// Merges another ClassInfo into this one.
    /// When both have the same relationship type, calls union_fn to merge the related classes.
    fn merge(
        mut self,
        other: Self,
        union_fn: &mut impl FnMut(VariableId, VariableId) -> VariableId,
    ) -> Self {
        // Merge forward relationships.
        for (kind, other_var) in other.relationship {
            match self.relationship.entry(kind) {
                Entry::Occupied(mut e) => {
                    if *e.get() != other_var {
                        *e.get_mut() = union_fn(*e.get(), other_var);
                    }
                }
                Entry::Vacant(e) => {
                    e.insert(other_var);
                }
            }
        }
        // Merge reverse relationships.
        for (kind, other_rel) in other.reverse_relationship {
            match self.reverse_relationship.entry(kind) {
                Entry::Occupied(mut e) => {
                    // For simple relations (Box, Snapshot, EnumConstruct), union the vars.
                    if let (Some(self_var), Some(other_var)) =
                        (e.get().single_var(), other_rel.single_var())
                        && self_var != other_var
                    {
                        *e.get_mut() = e.get().with_var(union_fn(self_var, other_var));
                    }
                    // StructConstruct: keep whichever side we already have (no field-level merge).
                }
                Entry::Vacant(e) => {
                    e.insert(other_rel);
                }
            }
        }
        self
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

    /// For each equivalence class representative, track relationships and construct provenance.
    class_info: OrderedHashMap<VariableId, ClassInfo<'db>>,

    /// Unified hashcons: maps Relation(inputs) -> output representative.
    /// This allows us to detect when two constructs with equivalent inputs should produce
    /// equivalent outputs. Covers enum constructs, struct/array constructs.
    ///
    /// Keys use representatives at insertion time. In SSA form, representatives are generally
    /// stable within a block, so keys stay valid without migration during `union`. A union
    /// *can* change a representative to a lower ID, which may cause a subsequent identical
    /// construct to miss the earlier entry — this is a known imprecision (conservative, not
    /// unsound). At merge points the maps are rebuilt from scratch.
    hashcons: OrderedHashMap<Relation<'db>, VariableId>,
}

impl<'db> EqualityState<'db> {
    /// Gets the parent of a variable, defaulting to itself (root) if not in the map.
    fn get_parent(&self, var: VariableId) -> VariableId {
        self.union_find.get(&var).copied().unwrap_or(var)
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

    /// Looks up a forward relationship of the given kind on a variable's class.
    fn get_forward(&mut self, var: VariableId, kind: RelationKind) -> Option<VariableId> {
        let rep = self.find(var);
        let related = self.class_info.get(&rep)?.relationship.get(&kind).copied()?;
        Some(self.find(related))
    }

    /// Looks up a reverse relationship of the given kind on a variable's class.
    fn get_reverse(&mut self, var: VariableId, kind: RelationKind) -> Option<VariableId> {
        let rep = self.find(var);
        let related = self.class_info.get(&rep)?.reverse_relationship.get(&kind)?.single_var()?;
        Some(self.find(related))
    }

    /// Sets a bidirectional relationship (Box or Snapshot) between two variables' classes.
    /// If either side already has a relationship of the same kind, unions with the existing class.
    fn set_relationship(&mut self, source: VariableId, target: VariableId, kind: RelationKind) {
        // Union with existing relationships if present.
        if let Some(existing) = self.get_forward(source, kind) {
            self.union(target, existing);
        }
        if let Some(existing) = self.get_reverse(target, kind) {
            self.union(source, existing);
        }

        // Re-find after potential unions — representatives may have changed.
        let rep_source = self.find(source);
        let rep_target = self.find(target);

        self.class_info.entry(rep_source).or_default().relationship.insert(kind, rep_target);
        let reverse = match kind {
            RelationKind::Box => Relation::Box(rep_source),
            RelationKind::Snapshot => Relation::Snapshot(rep_source),
            _ => unreachable!("set_relationship only for Box/Snapshot"),
        };
        self.class_info.entry(rep_target).or_default().reverse_relationship.insert(kind, reverse);
    }

    /// Sets a box relationship: boxed_var = Box(unboxed_var).
    fn set_box_relationship(&mut self, unboxed_var: VariableId, boxed_var: VariableId) {
        self.set_relationship(unboxed_var, boxed_var, RelationKind::Box);
    }

    /// Sets a snapshot relationship: snapshot_var = @original_var.
    fn set_snapshot_relationship(&mut self, original_var: VariableId, snapshot_var: VariableId) {
        self.set_relationship(original_var, snapshot_var, RelationKind::Snapshot);
    }

    /// Records a construct (enum or struct) via the unified hashcons.
    /// If we've already seen the same construct with equivalent inputs, unions the outputs.
    fn set_construct(&mut self, relation: Relation<'db>, output: VariableId) {
        let output_rep = if let Some(&existing_output) = self.hashcons.get(&relation) {
            self.union(existing_output, output);
            self.find(existing_output)
        } else {
            let output_rep = self.find(output);
            self.hashcons.insert(relation.clone(), output_rep);
            output_rep
        };
        self.class_info
            .entry(output_rep)
            .or_default()
            .reverse_relationship
            .insert(relation.kind(), relation);
    }

    /// Records an enum construct: output = Variant(input).
    fn set_enum_construct(
        &mut self,
        variant: ConcreteVariant<'db>,
        input: VariableId,
        output: VariableId,
    ) {
        let input_rep = self.find(input);
        self.set_construct(Relation::EnumConstruct(variant, input_rep), output);
    }

    /// Looks up the struct construct info for a representative (immutable).
    fn get_struct_construct_immut(
        &self,
        rep: VariableId,
    ) -> Option<(TypeId<'db>, Vec<VariableId>)> {
        match self.class_info.get(&rep)?.reverse_relationship.get(&RelationKind::StructConstruct)? {
            Relation::StructConstruct(ty, fields) => Some((*ty, fields.clone())),
            _ => unreachable!(),
        }
    }

    /// Looks up the struct construct info for a variable (mutable, uses find for path compression).
    fn get_struct_construct(
        &mut self,
        var: VariableId,
    ) -> Option<(TypeId<'db>, Vec<VariableId>)> {
        let rep = self.find(var);
        self.get_struct_construct_immut(rep)
    }

    /// Looks up the enum construct info for a representative (immutable).
    fn get_enum_construct_immut(
        &self,
        rep: VariableId,
    ) -> Option<(ConcreteVariant<'db>, VariableId)> {
        match self.class_info.get(&rep)?.reverse_relationship.get(&RelationKind::EnumConstruct)? {
            Relation::EnumConstruct(variant, input) => Some((*variant, *input)),
            _ => unreachable!(),
        }
    }

    /// Records a struct construct: output = StructType(inputs...).
    fn set_struct_construct(
        &mut self,
        ty: TypeId<'db>,
        input_reps: Vec<VariableId>,
        output: VariableId,
    ) {
        self.set_construct(Relation::StructConstruct(ty, input_reps), output);
    }
}

impl<'db> DebugWithDb<'db> for EqualityState<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let v = |id: VariableId| format!("v{}", self.find_immut(id).index());
        let mut lines = Vec::<String>::new();
        for (&rep, info) in self.class_info.iter() {
            if let Some(&s) = info.relationship.get(&RelationKind::Snapshot) {
                lines.push(format!("@{} = {}", v(rep), v(s)));
            }
            if let Some(&b) = info.relationship.get(&RelationKind::Box) {
                lines.push(format!("Box({}) = {}", v(rep), v(b)));
            }
        }
        for (relation, &output) in self.hashcons.iter() {
            match relation {
                Relation::EnumConstruct(variant, input) => {
                    let name = variant.id.name(db).to_string(db);
                    lines.push(format!("{name}({}) = {}", v(*input), v(output)));
                }
                Relation::StructConstruct(ty, inputs) => {
                    let type_name = ty.format(db);
                    let fields = inputs.iter().map(|&id| v(id)).collect::<Vec<_>>().join(", ");
                    lines.push(format!("{type_name}({fields}) = {}", v(output)));
                }
                // Box/Snapshot never appear in hashcons — they use class_info relationships.
                _ => {}
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

        if id == self.array_pop_front || id == self.array_pop_front_consume {
            match arm.var_ids[..] {
                // Some arm: [remaining_arr, boxed_elem].
                [remaining_arr, boxed_elem] => {
                    let input_arr = extern_info.inputs[0].var_id;
                    if let Some((ty, elems)) = info.get_struct_construct(input_arr)
                        && let Some((&first, rest)) = elems.split_first()
                    {
                        info.set_box_relationship(first, boxed_elem);
                        let rest_reps: Vec<_> = rest.iter().map(|&v| info.find(v)).collect();
                        info.set_struct_construct(ty, rest_reps, remaining_arr);
                    }
                }
                // None arm: union output with input.
                [original_arr] => {
                    info.union(original_arr, extern_info.inputs[0].var_id);
                }
                _ => {}
            }
        } else if id == self.array_snapshot_pop_front || id == self.array_snapshot_pop_back {
            match arm.var_ids[..] {
                // Some arm: [remaining_snap_arr, boxed_snap_elem].
                [remaining_snap_arr, boxed_snap_elem] => {
                    let input_snap_arr = extern_info.inputs[0].var_id;

                    // Look up tracked elements via snapshot reverse relationship or direct lookup.
                    let snap_rep = info.find(input_snap_arr);
                    let original_rep = info.class_info.get(&snap_rep).and_then(|ci| {
                        ci.reverse_relationship
                            .get(&RelationKind::Snapshot)
                            .and_then(|r| r.single_var())
                    });
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
                            (first, tail.to_vec())
                        } else {
                            let Some((&last, init)) = elems.split_last() else { return };
                            (last, init.to_vec())
                        };

                        // The popped element is `Box<@T>`. Record the box relationship against
                        // the snapshot class of `elem` if it exists.
                        let elem_rep = info.find(elem);
                        if let Some(&snap_of_elem) = info
                            .class_info
                            .get(&elem_rep)
                            .and_then(|ci| ci.relationship.get(&RelationKind::Snapshot))
                        {
                            info.set_box_relationship(snap_of_elem, boxed_snap_elem);
                        }

                        let snap_ty = self.lowered.variables[remaining_snap_arr].ty;
                        let rest_reps: Vec<_> = rest.iter().map(|&v| info.find(v)).collect();
                        info.set_struct_construct(snap_ty, rest_reps, remaining_snap_arr);
                    }
                }
                // None arm: union output with input.
                [original_snap_arr] => {
                    info.union(original_snap_arr, extern_info.inputs[0].var_id);
                }
                _ => {}
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

    let class_info_vars = info1
        .class_info
        .values()
        .chain(info2.class_info.values())
        .flat_map(ClassInfo::referenced_vars);

    let hashcons_vars =
        info1.hashcons.iter().chain(info2.hashcons.iter()).flat_map(|(relation, &output)| {
            relation.referenced_vars().chain(std::iter::once(output))
        });

    union_find_vars.chain(class_info_vars).chain(hashcons_vars)
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

            for &kind in &[RelationKind::Box, RelationKind::Snapshot] {
                if let Some(target_rep) = find_intersection_rep_opt(
                    info1,
                    info2,
                    intersections,
                    class1.relationship.get(&kind).copied(),
                    class2.relationship.get(&kind).copied(),
                ) {
                    result.set_relationship(intersection_var, target_rep, kind);
                }
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

/// Preserves construct entries (enum, struct) that exist in both branches.
/// Uses output-based lookup via `class_info` reverse_relationships.
fn merge_constructs<'db>(
    info1: &EqualityState<'db>,
    info2: &EqualityState<'db>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState<'db>,
) {
    for (&rep1, class1) in info1.class_info.iter() {
        for &(rep2, intersection_output) in intersections.get(&rep1).unwrap_or(&vec![]) {
            let Some(class2) = info2.class_info.get(&rep2) else { continue };

            // EnumConstruct: both must have same variant and intersecting input.
            if let (
                Some(Relation::EnumConstruct(variant1, input1)),
                Some(Relation::EnumConstruct(variant2, input2)),
            ) = (
                class1.reverse_relationship.get(&RelationKind::EnumConstruct),
                class2.reverse_relationship.get(&RelationKind::EnumConstruct),
            ) && variant1 == variant2
                && let Some(input_intersection) = find_intersection_rep(
                    intersections,
                    info1.find_immut(*input1),
                    info2.find_immut(*input2),
                )
            {
                result.set_enum_construct(*variant1, input_intersection, intersection_output);
            }

            // StructConstruct: all fields must have intersection reps.
            if let (
                Some(Relation::StructConstruct(ty1, fields1)),
                Some(Relation::StructConstruct(ty2, fields2)),
            ) = (
                class1.reverse_relationship.get(&RelationKind::StructConstruct),
                class2.reverse_relationship.get(&RelationKind::StructConstruct),
            ) && ty1 == ty2
                && fields1.len() == fields2.len()
            {
                let result_fields: Option<Vec<_>> = fields1
                    .iter()
                    .zip(fields2.iter())
                    .map(|(&v1, &v2)| {
                        find_intersection_rep(
                            intersections,
                            info1.find_immut(v1),
                            info2.find_immut(v2),
                        )
                    })
                    .collect();
                if let Some(result_fields) = result_fields {
                    result.set_struct_construct(*ty1, result_fields, intersection_output);
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

        merge_class_relationships(&info1, &info2, &intersections, &mut result);

        merge_constructs(&info1, &info2, &intersections, &mut result);

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
                if let Some((_, field_reps)) = info.get_struct_construct(struct_stmt.input.var_id) {
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
                } else if id == self.array_append
                    && let Some((ty, elems)) =
                        info.get_struct_construct(call_stmt.inputs[0].var_id)
                {
                    // Only track append if the input array is already tracked. Arrays from
                    // function parameters or external calls are conservatively ignored.
                    let mut new_elems = elems;
                    new_elems.push(info.find(call_stmt.inputs[1].var_id));
                    info.set_struct_construct(ty, new_elems, call_stmt.outputs[0]);
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
