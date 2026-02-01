//! Equality analysis for lowered IR.
//!
//! This module tracks semantic equivalence between variables as information flows through the
//! program. Two variables are equivalent if they hold the same value. Additionally, the analysis
//! tracks `Box`/unbox and snapshot/desnap relationships between equivalence classes.

use std::fmt;

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
#[derive(Clone, Default)]
pub struct EqualityState {
    /// Union-find parent map. If a variable is not in the map, it is its own representative.
    union_find: OrderedHashMap<VariableId, VariableId>,

    /// For each equivalence class representative, track relationships only if they exist.
    class_info: OrderedHashMap<VariableId, ClassInfo>,
}

impl EqualityState {
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
    fn find_immut(&self, var: VariableId) -> VariableId {
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
}

impl fmt::Debug for EqualityState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
/// This analyzer tracks snapshot/desnap and box/unbox relationships as data flows
/// through the program. At merge points (after match arms converge), we conservatively
/// intersect the equivalence classes, keeping only equalities that hold on all paths.
pub struct EqualityAnalysis;

impl EqualityAnalysis {
    /// Runs equality analysis on a lowered function.
    /// Returns the equality state at the exit of each block.
    pub fn analyze<'a, 'db>(lowered: &'a Lowered<'db>) -> Vec<Option<EqualityState>> {
        ForwardDataflowAnalysis::new(lowered, EqualityAnalysis).run()
    }
}

/// Returns an iterator over all variables with equality ir relationship information in the equality
/// states.
fn merge_referenced_vars<'a>(
    info1: &'a EqualityState,
    info2: &'a EqualityState,
) -> impl Iterator<Item = VariableId> + 'a {
    let union_find_vars = info1.union_find.keys().chain(info2.union_find.keys()).copied();

    let class_info_vars = info1
        .class_info
        .values()
        .chain(info2.class_info.values())
        .flat_map(ClassInfo::referenced_vars);

    union_find_vars.chain(class_info_vars)
}

/// Preserves only class relationships (box/snapshot) that exist in both branches.
fn merge_class_relationships(
    info1: &EqualityState,
    info2: &EqualityState,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState,
) {
    /// Finds an intersection representative: given a rep in info1 and a rep in info2,
    /// returns the intersection representative in the result if one exists.
    fn find_intersection_rep(
        equality_state: &mut EqualityState,
        info1: &EqualityState,
        info2: &EqualityState,
        intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
        rep1: Option<VariableId>,
        rep2: Option<VariableId>,
    ) -> Option<VariableId> {
        let (rep1, rep2) = (info1.find_immut(rep1?), info2.find_immut(rep2?));
        intersections.get(&rep1)?.iter().find_map(|(intersection_r2, intersection_rep)| {
            (*intersection_r2 == rep2).then(|| equality_state.find(*intersection_rep))
        })
    }

    for (&var, class1) in info1.class_info.iter() {
        for &(var_rep2, intersection_var) in intersections.get(&var).unwrap_or(&vec![]) {
            let Some(class2) = info2.class_info.get(&var_rep2) else {
                continue;
            };

            if let Some(boxed_rep) = find_intersection_rep(
                result,
                info1,
                info2,
                intersections,
                class1.boxed_class,
                class2.boxed_class,
            ) {
                result.set_box_relationship(intersection_var, boxed_rep);
            }

            if let Some(snap_rep) = find_intersection_rep(
                result,
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

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for EqualityAnalysis {
    type Info = EqualityState;

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

            // TODO(eytan-starkware): Struct/enum handling.
            Statement::StructConstruct(_)
            | Statement::StructDestructure(_)
            | Statement::EnumConstruct(_) => {}

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
