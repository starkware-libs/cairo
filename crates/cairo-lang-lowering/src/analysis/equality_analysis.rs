//! Equality analysis for lowered IR.
//!
//! This module tracks semantic equivalence between variables as information flows through the
//! program. Two variables are equivalent if they hold the same value (ignoring type wrappers like
//! `Box` and `@snapshot`).
//!
//! The analysis uses a union-find data structure to efficiently track equivalence classes, with
//! additional tracking for box/unbox and snapshot/desnap relationships between classes.

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::analysis::core::Edge;
use crate::analysis::{DataflowAnalyzer, Direction, ForwardDataflowAnalysis};
use crate::{BlockEnd, BlockId, Lowered, Statement, VariableId};

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
    /// Returns true if this ClassInfo has any relationships.
    fn is_empty(&self) -> bool {
        self.boxed_class.is_none()
            && self.unboxed_class.is_none()
            && self.snapshot_class.is_none()
            && self.original_class.is_none()
    }

    /// Merges a relationship field from another ClassInfo.
    /// If both have a value and they differ, calls the union function to merge them.
    fn merge_field(
        new: Option<VariableId>,
        old: Option<VariableId>,
        union_fn: &mut impl FnMut(VariableId, VariableId) -> VariableId,
    ) -> Option<VariableId> {
        match (new, old) {
            (None, old) => old,
            (Some(new_val), Some(old_val)) if new_val != old_val => {
                Some(union_fn(new_val, old_val))
            }
            (new, _) => new,
        }
    }

    /// Merges another ClassInfo into this one.
    /// When both have the same relationship type, calls union_fn to merge the related classes.
    fn merge(
        self,
        other: Self,
        union_fn: &mut impl FnMut(VariableId, VariableId) -> VariableId,
    ) -> Self {
        Self {
            boxed_class: Self::merge_field(self.boxed_class, other.boxed_class, union_fn),
            unboxed_class: Self::merge_field(self.unboxed_class, other.unboxed_class, union_fn),
            snapshot_class: Self::merge_field(self.snapshot_class, other.snapshot_class, union_fn),
            original_class: Self::merge_field(self.original_class, other.original_class, union_fn),
        }
    }

    /// Fills in missing fields from another ClassInfo (without calling union).
    fn fill_from(&mut self, other: Self) {
        if self.boxed_class.is_none() {
            self.boxed_class = other.boxed_class;
        }
        if self.unboxed_class.is_none() {
            self.unboxed_class = other.unboxed_class;
        }
        if self.snapshot_class.is_none() {
            self.snapshot_class = other.snapshot_class;
        }
        if self.original_class.is_none() {
            self.original_class = other.original_class;
        }
    }
}

/// Union-find node storing parent pointer and rank together.
#[derive(Clone, Copy, Debug)]
struct UnionFindNode {
    /// Parent variable. If equal to self, this is a root.
    parent: VariableId,
    /// Rank for union-by-rank optimization.
    rank: usize,
}

/// State for the equality analysis, tracking variable equivalences.
///
/// This is the `Info` type for the dataflow analysis. Each block gets its own
/// `EqualityState` representing what we know at that point in the program.
///
/// Uses sparse HashMaps for efficiency - only variables that have been touched
/// by the analysis are stored.
#[derive(Clone, Debug, Default)]
pub struct EqualityState {
    /// Union-find structure mapping VariableId -> (parent, rank).
    /// If a variable is not in the map, it is its own representative with rank 1.
    union_find: OrderedHashMap<VariableId, UnionFindNode>,

    /// For each equivalence class representative, track relationships.
    /// Only stored for representatives that have relationships.
    class_info: OrderedHashMap<VariableId, ClassInfo>,
}

impl EqualityState {
    /// Creates a new empty equality state.
    pub fn new(_lowered: &Lowered<'_>) -> Self {
        Self::default()
    }

    /// Gets the union-find node for a variable, returning a default self-referential node if not
    /// present.
    fn get_node(&self, var: VariableId) -> UnionFindNode {
        self.union_find.get(&var).copied().unwrap_or(UnionFindNode { parent: var, rank: 1 })
    }

    /// Gets the class info for a variable, returning a default if not present.
    fn get_class_info(&self, var: VariableId) -> ClassInfo {
        self.class_info.get(&var).cloned().unwrap_or_default()
    }

    /// Finds the representative of a variable's equivalence class.
    /// Uses path compression for efficiency.
    fn find(&mut self, var: VariableId) -> VariableId {
        let node = self.get_node(var);
        if node.parent != var {
            let root = self.find(node.parent);
            // Path compression: update parent to point directly to root
            self.union_find.insert(var, UnionFindNode { parent: root, rank: 0 });
            root
        } else {
            var
        }
    }

    /// Finds the representative without modifying the structure.
    fn find_immut(&self, var: VariableId) -> VariableId {
        let node = self.get_node(var);
        if node.parent != var { self.find_immut(node.parent) } else { var }
    }

    /// Unions two variables into the same equivalence class.
    /// Returns the representative of the merged class.
    fn union(&mut self, a: VariableId, b: VariableId) -> VariableId {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a == root_b {
            return root_a;
        }

        // Union by rank - read ranks first without modifying insertion order
        let node_a = self.get_node(root_a);
        let node_b = self.get_node(root_b);

        let (new_root, old_root, new_rank) = if node_a.rank < node_b.rank {
            (root_b, root_a, node_b.rank)
        } else if node_a.rank > node_b.rank {
            (root_a, root_b, node_a.rank)
        } else {
            (root_a, root_b, node_a.rank + node_b.rank)
        };

        // Insert new_root first to maintain ordering (lower IDs first when possible)
        self.union_find
            .entry(new_root)
            .or_insert(UnionFindNode { parent: new_root, rank: 1 })
            .rank = new_rank;
        // Update old_root to point to new_root
        self.union_find.insert(old_root, UnionFindNode { parent: new_root, rank: 0 });

        // Merge class info: since A == B, we have Box(A) == Box(B), @A == @B, etc.
        // The recursive unions in merge() might change who the representative is,
        // so we need to re-find after merging and insert at the actual root.
        let old_info = self.class_info.swap_remove(&old_root).unwrap_or_default();
        let new_info = self.class_info.swap_remove(&new_root).unwrap_or_default();
        let merged_info = new_info.merge(old_info, &mut |a, b| self.union(a, b));

        if !merged_info.is_empty() {
            // Re-find the representative after recursive unions (it might have changed)
            let final_root = self.find(new_root);
            self.class_info.entry(final_root).or_default().fill_from(merged_info);
        }

        self.find(new_root)
    }

    /// Sets a box relationship: boxed_var = Box(unboxed_var).
    /// If either side already has a relationship, unions with the existing class.
    fn set_box_relationship(&mut self, unboxed_var: VariableId, boxed_var: VariableId) {
        // Union with existing relationships if present
        if let Some(existing_boxed) = self.get_boxed(unboxed_var) {
            self.union(boxed_var, existing_boxed);
        }
        if let Some(existing_unboxed) = self.get_unboxed(boxed_var) {
            self.union(unboxed_var, existing_unboxed);
        }

        // Set bidirectional relationship (re-find after potential unions)
        let unboxed_rep = self.find(unboxed_var);
        let boxed_rep = self.find(boxed_var);

        let mut unboxed_info = self.get_class_info(unboxed_rep);
        unboxed_info.boxed_class = Some(boxed_rep);
        self.class_info.insert(unboxed_rep, unboxed_info);

        let mut boxed_info = self.get_class_info(boxed_rep);
        boxed_info.unboxed_class = Some(unboxed_rep);
        self.class_info.insert(boxed_rep, boxed_info);
    }

    /// Sets a snapshot relationship: snapshot_var = @original_var.
    /// If either side already has a relationship, unions with the existing class.
    fn set_snapshot_relationship(&mut self, original_var: VariableId, snapshot_var: VariableId) {
        // Union with existing relationships if present
        if let Some(existing_snapshot) = self.get_snapshot(original_var) {
            self.union(snapshot_var, existing_snapshot);
        }
        if let Some(existing_original) = self.get_original(snapshot_var) {
            self.union(original_var, existing_original);
        }

        // Set bidirectional relationship (re-find after potential unions)
        let original_rep = self.find(original_var);
        let snapshot_rep = self.find(snapshot_var);

        let mut original_info = self.get_class_info(original_rep);
        original_info.snapshot_class = Some(snapshot_rep);
        self.class_info.insert(original_rep, original_info);

        let mut snapshot_info = self.get_class_info(snapshot_rep);
        snapshot_info.original_class = Some(original_rep);
        self.class_info.insert(snapshot_rep, snapshot_info);
    }

    /// Gets the boxed version of a variable's class, if one exists.
    fn get_boxed(&mut self, var: VariableId) -> Option<VariableId> {
        let rep = self.find(var);
        let boxed = self.get_class_info(rep).boxed_class?;
        Some(self.find(boxed))
    }

    /// Gets the unboxed version of a variable's class, if one exists.
    fn get_unboxed(&mut self, var: VariableId) -> Option<VariableId> {
        let rep = self.find(var);
        let unboxed = self.get_class_info(rep).unboxed_class?;
        Some(self.find(unboxed))
    }

    /// Gets the snapshot version of a variable's class, if one exists.
    fn get_snapshot(&mut self, var: VariableId) -> Option<VariableId> {
        let rep = self.find(var);
        let snapshot = self.get_class_info(rep).snapshot_class?;
        Some(self.find(snapshot))
    }

    /// Gets the original (unsnapshot) version of a variable's class, if one exists.
    fn get_original(&mut self, var: VariableId) -> Option<VariableId> {
        let rep = self.find(var);
        let original = self.get_class_info(rep).original_class?;
        Some(self.find(original))
    }

    // Public query methods (immutable)

    /// Checks if two variables are in the same equivalence class.
    pub fn are_equal(&self, a: VariableId, b: VariableId) -> bool {
        self.find_immut(a) == self.find_immut(b)
    }

    /// Gets the boxed version of a variable's class, if one exists.
    /// Returns the current representative of the boxed class.
    pub fn get_boxed_var(&self, var: VariableId) -> Option<VariableId> {
        let rep = self.find_immut(var);
        self.class_info.get(&rep)?.boxed_class.map(|v| self.find_immut(v))
    }

    /// Gets the unboxed version of a variable's class, if one exists.
    /// Returns the current representative of the unboxed class.
    pub fn get_unboxed_var(&self, var: VariableId) -> Option<VariableId> {
        let rep = self.find_immut(var);
        self.class_info.get(&rep)?.unboxed_class.map(|v| self.find_immut(v))
    }

    /// Gets the snapshot version of a variable's class, if one exists.
    /// Returns the current representative of the snapshot class.
    pub fn get_snapshot_var(&self, var: VariableId) -> Option<VariableId> {
        let rep = self.find_immut(var);
        self.class_info.get(&rep)?.snapshot_class.map(|v| self.find_immut(v))
    }

    /// Gets the original (desnapped) version of a variable's class, if one exists.
    /// Returns the current representative of the original class.
    pub fn get_original_var(&self, var: VariableId) -> Option<VariableId> {
        let rep = self.find_immut(var);
        self.class_info.get(&rep)?.original_class.map(|v| self.find_immut(v))
    }

    /// Gets the representative of a variable's equivalence class.
    pub fn get_representative(&self, var: VariableId) -> VariableId {
        self.find_immut(var)
    }
}

/// Forward dataflow analyzer for tracking variable equality.
///
/// This analyzer tracks snapshot/desnap and box/unbox relationships as data flows
/// through the program. At merge points (after match arms converge), we conservatively
/// return a fresh state, losing all equality information from the branches.
pub struct EqualityAnalysis<'db, 'a> {
    lowered: &'a Lowered<'db>,
}

impl<'db, 'a> EqualityAnalysis<'db, 'a> {
    /// Creates a new equality analyzer.
    pub fn new(lowered: &'a Lowered<'db>) -> Self {
        Self { lowered }
    }

    /// Runs equality analysis on a lowered function.
    ///
    /// Returns the equality state at the exit of each block.
    pub fn analyze(lowered: &'a Lowered<'db>) -> Vec<Option<EqualityState>> {
        let analyzer = EqualityAnalysis::new(lowered);
        let mut fwd = ForwardDataflowAnalysis::new(lowered, analyzer);
        fwd.run()
    }
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for EqualityAnalysis<'db, 'a> {
    type Info = EqualityState;

    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        EqualityState::new(self.lowered)
    }

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: super::StatementLocation,
        info1: Self::Info,
        info2: Self::Info,
    ) -> Self::Info {
        // Intersection-based merge: keep only equalities that hold in BOTH branches.
        // This is sound for a must-analysis: we only retain info true on all paths.
        let mut result = EqualityState::new(self.lowered);

        // For each variable in info1's union_find map, check if it has the same representative
        // in both states. If so, union them in the result.
        for (&var, _) in info1.union_find.iter() {
            let rep1 = info1.find_immut(var);
            let rep2 = info2.find_immut(var);

            // If this variable has the same representative in both branches, preserve the equality.
            if rep1 == rep2 {
                result.union(var, rep1);
            }
        }

        // Also check variables only in info2's union_find map
        for (&var, _) in info2.union_find.iter() {
            if info1.union_find.contains_key(&var) {
                continue; // Already processed above
            }
            let rep1 = info1.find_immut(var);
            let rep2 = info2.find_immut(var);

            if rep1 == rep2 {
                result.union(var, rep1);
            }
        }

        // Preserve class relationships that exist in both branches with the same targets.
        for (&var, class1) in info1.class_info.iter() {
            let rep1 = info1.find_immut(var);
            let rep2 = info2.find_immut(var);

            // Only process representatives that are the same in both branches
            if rep1 != rep2 || rep1 != var {
                continue;
            }

            let class2 = match info2.class_info.get(&var) {
                Some(c) => c,
                None => continue,
            };

            // Preserve boxed relationship if both branches agree
            if let (Some(boxed1), Some(boxed2)) = (class1.boxed_class, class2.boxed_class) {
                let boxed_rep1 = info1.find_immut(boxed1);
                let boxed_rep2 = info2.find_immut(boxed2);
                if boxed_rep1 == boxed_rep2 {
                    result.set_box_relationship(var, boxed_rep1);
                }
            }

            // Preserve snapshot relationship if both branches agree
            if let (Some(snap1), Some(snap2)) = (class1.snapshot_class, class2.snapshot_class) {
                let snap_rep1 = info1.find_immut(snap1);
                let snap_rep2 = info2.find_immut(snap2);
                if snap_rep1 == snap_rep2 {
                    result.set_snapshot_relationship(var, snap_rep1);
                }
            }
        }

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
                // The original output is equal to the input
                info.union(snapshot_stmt.original(), snapshot_stmt.input.var_id);
                // Create snapshot relationship
                info.set_snapshot_relationship(
                    snapshot_stmt.input.var_id,
                    snapshot_stmt.snapshot(),
                );
            }

            Statement::Desnap(desnap_stmt) => {
                // output = desnap(input): input is a snapshot of output
                // This establishes the relationship and will union with any existing original
                info.set_snapshot_relationship(desnap_stmt.output, desnap_stmt.input.var_id);
            }

            Statement::IntoBox(into_box_stmt) => {
                // output = box(input): output is a box of input
                info.set_box_relationship(into_box_stmt.input.var_id, into_box_stmt.output);
            }

            Statement::Unbox(unbox_stmt) => {
                // output = unbox(input): input is a box of output
                info.set_box_relationship(unbox_stmt.output, unbox_stmt.input.var_id);
            }

            // Struct/enum handling deferred to future PR (hashconsing)
            Statement::StructConstruct(_)
            | Statement::StructDestructure(_)
            | Statement::EnumConstruct(_) => {}

            // These statements don't establish equality relationships
            Statement::Const(_) | Statement::Call(_) => {}
        }
    }

    fn transfer_edge(&mut self, info: &Self::Info, edge: &Edge<'db, 'a>) -> Self::Info {
        let mut new_info = info.clone();
        if let Edge::Goto { remapping, .. } = edge {
            // Union remapped variables: dst and src should be in the same equivalence class
            for (dst, src_usage) in remapping.iter() {
                new_info.union(*dst, src_usage.var_id);
            }
        }
        new_info
    }
}

// Note: Unit tests for EqualityState require creating VariableIds which needs an Arena.
// The equality analysis is tested through integration tests when used by other optimizations.
// See crates/cairo-lang-lowering/src/test.rs for integration test patterns.
