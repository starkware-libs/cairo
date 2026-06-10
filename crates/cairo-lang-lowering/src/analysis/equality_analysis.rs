//! Equality analysis for lowered IR.
//!
//! This module tracks semantic equivalence between variables as information flows through the
//! program. Two variables are equivalent if they hold the same value. Additionally, the analysis
//! tracks `Box`/unbox, snapshot/desnap, struct construct, and array construct relationships between
//! equivalence classes via unified forward/reverse maps. Structs track every field; arrays track
//! a bounded prefix/suffix of elements around a size-typed run of forgotten ones.

use std::num::NonZeroUsize;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{ExternFunctionId, NamedLanguageElementId};
use cairo_lang_semantic::corelib::option_some_variant;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::{ConcreteVariant, GenericArgumentId, MatchArmSelector, TypeId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::zip_eq;
use salsa::Database;

use crate::analysis::core::Edge;
use crate::analysis::{DataflowAnalyzer, Direction, ForwardDataflowAnalysis};
use crate::{
    BlockEnd, BlockId, Lowered, MatchArm, MatchExternInfo, MatchInfo, Statement, VariableId,
};

/// A globally unique token for an unknown value or array run tracked by the analysis. Allocated by
/// [`fresh_placeholder`]; equality means "the very same unknown", so distinct unknowns never
/// compare equal.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Placeholder(usize);

/// A struct field variable: either a real variable or a unique placeholder for an unknown field.
/// Placeholders are created during merge when a field has no intersection representative.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum FieldVar {
    Var(VariableId),
    /// A globally unique placeholder representing an unknown field.
    Placeholder(Placeholder),
}

impl FieldVar {
    /// Returns the real variable if this is a `Var`, or `None` if it's a `Placeholder`.
    fn as_var(self) -> Option<VariableId> {
        match self {
            FieldVar::Var(v) => Some(v),
            FieldVar::Placeholder(_) => None,
        }
    }

    /// Resolves the variable inside a `Var` to its representative, leaves `Placeholder` unchanged.
    fn find_rep(self, info: &mut EqualityState<'_>) -> Self {
        match self {
            FieldVar::Var(v) => FieldVar::Var(info.find(v)),
            p @ FieldVar::Placeholder(_) => p,
        }
    }
}

/// Maximum number of individual elements tracked at each end of an array.
///
/// Tracking every element is quadratic in the number of appends, while element forwarding only
/// pays off near an array's ends — so only a bounded prefix and suffix are tracked, with a
/// single size-typed run of forgotten elements in between (see [`ArrayItems`]).
const MAX_ARRAY_END_ITEMS: usize = 5;

/// A run of consecutive forgotten array elements between [`ArrayItems::prefix`] and
/// [`ArrayItems::suffix`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct ArrayRun {
    /// The run's identity: two runs describe the same element sequence only if their
    /// placeholders match, so any change to a run's contents allocates a fresh placeholder.
    placeholder: Placeholder,
    /// The number of forgotten elements, or `None` when unknown (possibly zero).
    len: Option<NonZeroUsize>,
}

/// The tracked contents of an array: a bounded prefix and suffix of individual elements around
/// at most one run of forgotten ones.
///
/// Canonical form: without a run, the contents are fully known and fill the prefix first, so
/// the suffix is non-empty only when the prefix is full.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
struct ArrayItems {
    /// The leading elements, at most [`MAX_ARRAY_END_ITEMS`].
    prefix: Vec<FieldVar>,
    /// The forgotten elements between the prefix and the suffix.
    run: Option<ArrayRun>,
    /// The trailing elements, at most [`MAX_ARRAY_END_ITEMS`].
    suffix: Vec<FieldVar>,
}

impl ArrayItems {
    /// Whether this tracks an array known to be empty.
    fn is_empty(&self) -> bool {
        self.prefix.is_empty() && self.run.is_none() && self.suffix.is_empty()
    }

    /// Returns an iterator over the real variables of the tracked elements.
    fn referenced_vars(&self) -> impl Iterator<Item = VariableId> + '_ {
        self.prefix.iter().chain(&self.suffix).filter_map(|f| f.as_var())
    }

    /// Resolves the tracked elements' variables to their representatives.
    fn find_rep(self, info: &mut EqualityState<'_>) -> Self {
        Self {
            prefix: self.prefix.into_iter().map(|f| f.find_rep(info)).collect(),
            run: self.run,
            suffix: self.suffix.into_iter().map(|f| f.find_rep(info)).collect(),
        }
    }

    /// Appends `elem`, keeping the contents canonical: once both ends are full, the oldest
    /// suffix element is forgotten into the run.
    fn append(mut self, elem: FieldVar, next_placeholder: &mut usize) -> Self {
        if self.run.is_none() && self.prefix.len() < MAX_ARRAY_END_ITEMS {
            assert!(self.suffix.is_empty(), "suffix should be empty: cannot append");
            self.prefix.push(elem);
            return self;
        }
        if self.suffix.len() == MAX_ARRAY_END_ITEMS {
            let len = match self.run {
                None => Some(NonZeroUsize::new(1).unwrap()),
                Some(run) => run.len.map(|len| len.saturating_add(1)),
            };
            self.run = Some(ArrayRun { placeholder: fresh_placeholder(next_placeholder), len });
            self.suffix.remove(0);
        }
        assert!(self.suffix.len() < MAX_ARRAY_END_ITEMS, "suffix is full: cannot append");
        self.suffix.push(elem);
        self
    }

    /// Restores the canonical form after a removal: without a run, elements fill the prefix
    /// first. Only reorders — the end budgets and the run invariant must already hold.
    fn canonicalized(mut self) -> Self {
        assert!(
            self.prefix.len() <= MAX_ARRAY_END_ITEMS && self.suffix.len() <= MAX_ARRAY_END_ITEMS,
            "an end exceeds its budget"
        );
        // Reorder only without a run: elements must not be shifted across a live one.
        if self.run.is_none() {
            while self.prefix.len() < MAX_ARRAY_END_ITEMS && !self.suffix.is_empty() {
                let moved = self.suffix.remove(0);
                self.prefix.push(moved);
            }
        }
        self
    }

    /// Removes one element from the front (`from_front`) or back.
    fn pop(&self, from_front: bool, next_placeholder: &mut usize) -> PopResult {
        // Pops the outermost element of the given end of a clone.
        let pop_element = |from_prefix: bool| {
            let mut remaining = self.clone();
            let source = if from_prefix { &mut remaining.prefix } else { &mut remaining.suffix };
            assert!(!source.is_empty(), "pop from empty end");
            let element = if from_front { source.remove(0) } else { source.pop().unwrap() };
            PopResult::Element { element, remaining: remaining.canonicalized() }
        };
        let (start, end) =
            if from_front { (&self.prefix, &self.suffix) } else { (&self.suffix, &self.prefix) };
        match self.run {
            // The popped end holds an individually tracked element.
            _ if !start.is_empty() => pop_element(from_front),
            // The boundary is a run of unknown size: the array may even be empty, so nothing
            // is known about the result.
            Some(ArrayRun { len: None, .. }) => PopResult::Unknown,
            // The boundary is a known-size run: shrink it, dropping it once exhausted.
            Some(ArrayRun { len: Some(len), .. }) => PopResult::ForgottenElement {
                remaining: Self {
                    run: NonZeroUsize::new(len.get() - 1).map(|len| ArrayRun {
                        placeholder: fresh_placeholder(next_placeholder),
                        len: Some(len),
                    }),
                    ..self.clone()
                }
                .canonicalized(),
            },
            // No run: the contents are fully known and may all sit at the opposite end.
            None if end.is_empty() => PopResult::KnownEmpty,
            None => pop_element(!from_front),
        }
    }
}

/// The result of popping one element off a tracked array (see [`ArrayItems::pop`]).
enum PopResult {
    /// The popped element is individually tracked.
    Element { element: FieldVar, remaining: ArrayItems },
    /// The popped element exists but its value was forgotten (it came off a known-size run).
    ForgottenElement { remaining: ArrayItems },
    /// The array is known to be empty, so there is no element to pop.
    KnownEmpty,
    /// The array's size is unknown — it may even be empty — so nothing is known about the
    /// result.
    Unknown,
}

/// Allocates a globally unique placeholder, used for unknown struct fields and array runs.
fn fresh_placeholder(next_placeholder: &mut usize) -> Placeholder {
    *next_placeholder += 1;
    Placeholder(*next_placeholder - 1)
}

/// A relationship between equivalence classes, carrying its payload data.
/// Hashable so it can be used as a forward map key.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Relation<'db> {
    Box(VariableId),
    Snapshot(VariableId),
    EnumConstruct(ConcreteVariant<'db>, VariableId),
    StructConstruct(TypeId<'db>, Vec<FieldVar>),
    Array(TypeId<'db>, ArrayItems),
}

impl<'db> Relation<'db> {
    /// Returns an iterator over all real variables referenced by this relation.
    fn referenced_vars(&self) -> Box<dyn Iterator<Item = VariableId> + '_> {
        match self {
            Relation::Box(v) | Relation::Snapshot(v) | Relation::EnumConstruct(_, v) => {
                Box::new(std::iter::once(*v))
            }
            Relation::StructConstruct(_, fields) => {
                Box::new(fields.iter().filter_map(|f| f.as_var()))
            }
            Relation::Array(_, items) => Box::new(items.referenced_vars()),
        }
    }

    /// Returns a new Relation with all input variables resolved to their current representatives.
    fn with_fresh_reps(self, state: &mut EqualityState<'_>) -> Self {
        match self {
            Relation::Box(v) => Relation::Box(state.find(v)),
            Relation::Snapshot(v) => Relation::Snapshot(state.find(v)),
            Relation::EnumConstruct(variant, v) => Relation::EnumConstruct(variant, state.find(v)),
            Relation::StructConstruct(ty, fields) => Relation::StructConstruct(
                ty,
                fields.into_iter().map(|f| f.find_rep(state)).collect(),
            ),
            Relation::Array(ty, items) => Relation::Array(ty, items.find_rep(state)),
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

    /// Unions the two variables into the same equivalence class.
    /// `old_var`'s root is kept as the representative; `union_var`'s root joins it.
    /// `union_var` must be a root, unless already in `old_var`'s class (no-op).
    /// Returns the representative of the merged class.
    fn add_equality(&mut self, old_var: VariableId, union_var: VariableId) -> VariableId {
        let old_var = self.find(old_var);
        let new_var = self.find(union_var);
        if new_var == old_var {
            return old_var;
        }
        assert!(union_var == new_var, "Expected variables to always be 'new' or equal to old");
        self.union_find.insert(new_var, old_var);
        old_var
    }

    /// Records a relation: forward maps `relation -> output`, reverse maps `output -> relation`.
    /// If the same relation already maps to an existing output, unions them.
    fn set_relation(&mut self, relation: Relation<'db>, output: VariableId) {
        // Refresh reps — callers may pass stale IDs, and this maximizes forward hits.
        let relation = relation.with_fresh_reps(self);

        // Forward dedup: if this exact relation already maps to an output, union them.
        let existing_output = if let Some(&existing_output) = self.forward.get(&relation) {
            self.add_equality(existing_output, output)
        } else {
            output
        };

        // Insert with current reps (may be slightly stale after the union above).
        self.forward.insert(relation.clone(), existing_output);
        self.reverse.insert(existing_output, relation);
    }

    /// Looks up the struct construct info for a representative (immutable).
    fn get_struct_construct_immut(&self, rep: VariableId) -> Option<(TypeId<'db>, Vec<FieldVar>)> {
        match self.reverse.get(&rep)? {
            Relation::StructConstruct(ty, fields) => Some((*ty, fields.clone())),
            _ => None,
        }
    }

    /// Looks up the struct construct info for a variable (mutable, uses find for path compression).
    fn get_struct_construct(&mut self, var: VariableId) -> Option<(TypeId<'db>, Vec<FieldVar>)> {
        let rep = self.find(var);
        self.get_struct_construct_immut(rep)
    }

    /// Looks up the array construct info for a representative (immutable).
    fn get_array_construct_immut(&self, rep: VariableId) -> Option<(TypeId<'db>, ArrayItems)> {
        match self.reverse.get(&rep)? {
            Relation::Array(ty, items) => Some((*ty, items.clone())),
            _ => None,
        }
    }

    /// Looks up the array construct info for a variable (mutable, uses find for path compression).
    fn get_array_construct(&mut self, var: VariableId) -> Option<(TypeId<'db>, ArrayItems)> {
        let rep = self.find(var);
        self.get_array_construct_immut(rep)
    }

    /// Looks up the enum construct info for a variable (mutable, uses find for path compression).
    pub(crate) fn get_enum_construct(
        &self,
        var: VariableId,
    ) -> Option<(ConcreteVariant<'db>, VariableId)> {
        let rep = self.find_immut(var);
        self.get_enum_construct_immut(rep)
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
                    let fields = inputs
                        .iter()
                        .map(|f| match f {
                            FieldVar::Var(id) => v(*id),
                            FieldVar::Placeholder(_) => "?".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    lines.push(format!("{type_name}({fields}) = {}", v(output)));
                }
                // Arrays render as `Type[..]` (vs `Type(..)` for structs); runs of forgotten
                // elements render as `?(len)` / `?(*)`.
                Relation::Array(ty, items) => {
                    let type_name = ty.format(db);
                    let elem = |f: &FieldVar| match f {
                        FieldVar::Var(id) => v(*id),
                        FieldVar::Placeholder(_) => "?".to_string(),
                    };
                    let run = items.run.iter().map(|run| match run.len {
                        Some(len) => format!("?({len})"),
                        None => "?(*)".to_string(),
                    });
                    let rendered = items
                        .prefix
                        .iter()
                        .map(elem)
                        .chain(run)
                        .chain(items.suffix.iter().map(elem))
                        .collect::<Vec<_>>()
                        .join(", ");
                    lines.push(format!("{type_name}[{rendered}] = {}", v(output)));
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
/// This analyzer tracks snapshot/desnap, box/unbox, struct construct, and array construct
/// relationships as data
/// flows through the program. At merge points (after match arms converge), we union-find variables
/// that share the same equivalence classes in **both** branches (`(rep1, rep2)` meet), rebuild
/// `forward` / `reverse`, and intersect relations **without** mapping variables across differing
/// branch-local equivalence roots (structure fields fall back to placeholders).
pub struct EqualityAnalysis<'a, 'db> {
    db: &'db dyn Database,
    lowered: &'a Lowered<'db>,
    /// Counter for allocating globally unique ids: `FieldVar::Placeholder`s for unknown fields
    /// after merge, and [`ArrayRun`] ids.
    next_placeholder: usize,
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
            next_placeholder: 0,
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
    /// Array pop operations act as "destructures" on the array construct representation:
    /// - `array_pop_front` / `array_pop_front_consume`: On the Some arm, if the input array was
    ///   tracked as `[e0, e1, ..., eN]`, the popped element (boxed) is `Box(e0)` and the remaining
    ///   array is `[e1, ..., eN]`.
    /// - `array_snapshot_pop_front`: Same as above but through snapshot/box-of-snapshot wrappers.
    /// - `array_snapshot_pop_back`: Like pop_front but pops from the back: element is `Box(eN)`,
    ///   remaining is `[e0, ..., eN-1]`.
    ///
    /// The `None` arms are intentionally ignored: recording empty-array relations or unions there
    /// would backedge-merge older SSA equivalence classes based on branch discrimination.
    fn transfer_extern_match_arm(
        &mut self,
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
    ///
    /// Covers both owned pops (`array_pop_front`/`array_pop_front_consume`) and snapshot pops
    /// (`array_snapshot_pop_front`/`array_snapshot_pop_back`). Snapshot pops differ in that the
    /// tracked contents are preferably found through the input's snapshot relation, and the
    /// popped value is a `Box<@T>`, so the boxed class is the popped element's snapshot.
    fn transfer_array_pop_arm(
        &mut self,
        info: &mut EqualityState<'db>,
        extern_info: &MatchExternInfo<'db>,
        arm: &MatchArm<'db>,
        id: ExternFunctionId<'db>,
        is_some: bool,
    ) {
        let is_snapshot = id == self.array_snapshot_pop_front || id == self.array_snapshot_pop_back;
        let is_owned = id == self.array_pop_front || id == self.array_pop_front_consume;
        if !is_some || !(is_snapshot || is_owned) {
            return;
        }
        // Some arm: var_ids = [remaining_arr, boxed_elem]
        let input_arr = extern_info.inputs[0].var_id;
        let remaining_arr = arm.var_ids[0];
        let boxed_elem = arm.var_ids[1];

        // Look up the tracked contents: through the snapshotted original array when the input
        // is a snapshot of one, or directly (e.g. the remainder of a previous snapshot pop).
        let input_rep = info.find(input_arr);
        let original_rep = match info.reverse.get(&input_rep) {
            Some(Relation::Snapshot(v)) if is_snapshot => Some(info.find_immut(*v)),
            _ => None,
        };
        let Some((_, items)) = original_rep
            .and_then(|orig| info.get_array_construct_immut(orig))
            .or_else(|| info.get_array_construct_immut(input_rep))
        else {
            return;
        };

        let from_front = id != self.array_snapshot_pop_back;
        let remaining = match items.pop(from_front, &mut self.next_placeholder) {
            PopResult::Element { element, remaining } => {
                // TODO(eytan-starkware): Add support for placeholders in boxes and reverse box
                // relation to unify placeholder.
                if let FieldVar::Var(elem_var) = element {
                    // A snapshot pop yields `Box<@elem>`, so box the class of `@elem` — when a
                    // variable represents it.
                    let box_target = if is_snapshot {
                        let elem_rep = info.find(elem_var);
                        info.forward.get(&Relation::Snapshot(elem_rep)).copied()
                    } else {
                        Some(elem_var)
                    };
                    if let Some(target) = box_target {
                        info.set_relation(Relation::Box(target), boxed_elem);
                    }
                }
                remaining
            }
            PopResult::ForgottenElement { remaining } => remaining,
            // This arm is unreachable at runtime (popping a known-empty array fails), so any
            // state is sound; keep tracking the remainder as empty.
            PopResult::KnownEmpty => ArrayItems::default(),
            PopResult::Unknown => return,
        };
        let ty = self.lowered.variables[remaining_arr].ty;
        info.set_relation(Relation::Array(ty, remaining), remaining_arr);
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

/// Meets two field variables at a merge: kept when both cite the same representative in
/// `result` (setting `any_data`), otherwise replaced by a fresh placeholder.
fn merge_field(
    f1: FieldVar,
    f2: FieldVar,
    result: &mut EqualityState<'_>,
    next_placeholder: &mut usize,
    any_data: &mut bool,
) -> FieldVar {
    match (f1, f2) {
        (FieldVar::Var(v), FieldVar::Var(w)) if result.find(v) == result.find(w) => {
            *any_data = true;
            FieldVar::Var(result.find(v))
        }
        (_, _) => FieldVar::Placeholder(fresh_placeholder(next_placeholder)),
    }
}

/// Meets two arrays' tracked contents at a merge: requires matching shapes (prefix/suffix
/// lengths and run presence). Elements meet like struct fields; a run keeps its placeholder
/// when shared, and otherwise degrades to a fresh run whose length is kept only when the two
/// agree. Returns the met contents and whether any real data survived, or `None` when the
/// shapes are incompatible.
fn merge_array_items(
    items1: &ArrayItems,
    items2: &ArrayItems,
    result: &mut EqualityState<'_>,
    next_placeholder: &mut usize,
) -> Option<(ArrayItems, bool)> {
    if items1.prefix.len() != items2.prefix.len() || items1.suffix.len() != items2.suffix.len() {
        return None;
    }
    let mut any_data = false;
    let run = match (items1.run, items2.run) {
        (None, None) => None,
        (Some(r1), Some(r2)) => Some(if r1 == r2 {
            any_data = true;
            r1
        } else {
            ArrayRun {
                placeholder: fresh_placeholder(next_placeholder),
                len: if r1.len == r2.len { r1.len } else { None },
            }
        }),
        (None, Some(_)) | (Some(_), None) => return None,
    };
    let mut merge_ends = |end1: &[FieldVar], end2: &[FieldVar], any_data: &mut bool| {
        zip_eq(end1, end2)
            .map(|(f1, f2)| merge_field(*f1, *f2, result, next_placeholder, any_data))
            .collect()
    };
    let prefix = merge_ends(&items1.prefix, &items2.prefix, &mut any_data);
    let suffix = merge_ends(&items1.suffix, &items2.suffix, &mut any_data);
    Some((ArrayItems { prefix, run, suffix }, any_data))
}

/// Keeps relational edges that survive the join **without** mapping variables across mismatched
/// branch equivalence roots: struct fields become placeholders and array runs lose their
/// identity unless both sides cite the same SSA `VariableId` / run id. Box/Snapshot/Enum reuse
/// an edge only when merged outputs coincide in `result`.
fn merge_relations<'db>(
    info1: &EqualityState<'db>,
    info2: &EqualityState<'db>,
    intersections: &OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>>,
    result: &mut EqualityState<'db>,
    next_placeholder: &mut usize,
) {
    // Index info2's aggregate (struct/array) entries by output representative for lookup in the
    // merge loop. Forward entries are completely authoritative, so we use them to build the index.
    // Structs and arrays are indexed separately so they never cross-match during the intersection.
    let mut info2_structs_by_output: OrderedHashMap<VariableId, (TypeId<'db>, Vec<FieldVar>)> =
        OrderedHashMap::default();
    let mut info2_arrays_by_output: OrderedHashMap<VariableId, (TypeId<'db>, ArrayItems)> =
        OrderedHashMap::default();
    for (relation, &output2) in info2.forward.iter() {
        match relation {
            Relation::StructConstruct(ty, fields) => {
                info2_structs_by_output.insert(info2.find_immut(output2), (*ty, fields.clone()));
            }
            Relation::Array(ty, items) => {
                info2_arrays_by_output.insert(info2.find_immut(output2), (*ty, items.clone()));
            }
            _ => {}
        }
    }

    // Iterate all forward entries from info1 and find matching entries in info2.
    for (relation, &output1) in info1.forward.iter() {
        match relation {
            Relation::Box(source1) | Relation::Snapshot(source1) => {
                for &(source2, intersection_var) in intersections.get(source1).into_iter().flatten()
                {
                    let relation2 = match relation {
                        Relation::Box(_) => Relation::Box(source2),
                        Relation::Snapshot(_) => Relation::Snapshot(source2),
                        _ => unreachable!(),
                    };
                    let Some(&output2) = info2.forward.get(&relation2) else { continue };
                    // Require the same unified output (join meet); avoid mapping via rep pair
                    // lookup.
                    // TODO(eytan-starkware): We want to check that the intersection is not empty
                    // and use that for the output rep, not that the leaders are there.
                    let output_rep = result.find(output1);
                    if output_rep != result.find_immut(output2) {
                        continue;
                    }
                    let result_relation = match relation {
                        Relation::Box(_) => Relation::Box(result.find(intersection_var)),
                        Relation::Snapshot(_) => Relation::Snapshot(result.find(intersection_var)),
                        _ => unreachable!(),
                    };
                    result.set_relation(result_relation, output_rep);
                }
            }
            Relation::EnumConstruct(variant, input1) => {
                for &(input2, input_intersection) in
                    intersections.get(&info1.find_immut(*input1)).into_iter().flatten()
                {
                    let relation2 = Relation::EnumConstruct(*variant, input2);
                    let Some(&output2) = info2.forward.get(&relation2) else { continue };
                    let output_rep = result.find(output1);
                    if output_rep != result.find_immut(output2) {
                        continue;
                    }
                    result.set_relation(
                        Relation::EnumConstruct(*variant, input_intersection),
                        output_rep,
                    );
                }
            }
            Relation::StructConstruct(ty, fields1) => {
                let output1_rep = info1.find_immut(output1);
                for &(output2_rep, output_intersection) in
                    intersections.get(&output1_rep).into_iter().flatten()
                {
                    let Some((ty2, fields2)) = info2_structs_by_output.get(&output2_rep) else {
                        continue;
                    };
                    if ty2 != ty || fields2.len() != fields1.len() {
                        continue;
                    }
                    let mut any_data = false;
                    let result_fields: Vec<FieldVar> = fields1
                        .iter()
                        .zip(fields2)
                        .map(|(f1, f2)| {
                            merge_field(*f1, *f2, result, next_placeholder, &mut any_data)
                        })
                        .collect();
                    if any_data || result_fields.is_empty() {
                        result.set_relation(
                            Relation::StructConstruct(*ty, result_fields),
                            output_intersection,
                        );
                    }
                }
            }
            Relation::Array(ty, items1) => {
                let output1_rep = info1.find_immut(output1);
                for &(output2_rep, output_intersection) in
                    intersections.get(&output1_rep).into_iter().flatten()
                {
                    let Some((ty2, items2)) = info2_arrays_by_output.get(&output2_rep) else {
                        continue;
                    };
                    if ty2 != ty {
                        continue;
                    }
                    let Some((result_items, any_data)) =
                        merge_array_items(items1, items2, result, next_placeholder)
                    else {
                        continue;
                    };
                    if any_data || result_items.is_empty() {
                        result
                            .set_relation(Relation::Array(*ty, result_items), output_intersection);
                    }
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
        // Meet of union-finds, then intersect relations (`merge_relations`) without bridging
        // mismatched equiv-class reps across branches (struct fields → placeholders unless same SSA
        // id).
        let mut result = EqualityState::default();

        // Group variables by (rep1, rep2) - for variables present in either state.
        let mut groups: OrderedHashMap<(VariableId, VariableId), Vec<VariableId>> =
            OrderedHashMap::default();

        // Group by (rep1, rep2). Duplicates are fine - they'll just be added to the same group.
        for var in merge_referenced_vars(&info1, &info2) {
            let key = (info1.find_immut(var), info2.find_immut(var));
            groups.entry(key).or_default().push(var);
        }

        // Union all variables within each group. Members are fresh in `result`
        // (which was just constructed), so any choice of `keep` is flat-preserving;
        // the lowest-ID is chosen so the rep is stable/deterministic.
        for members in groups.values() {
            let Some(&keep) = members.iter().min_by_key(|v| v.index()) else { continue };
            for &var in members {
                if var != keep {
                    result.add_equality(keep, var);
                }
            }
        }

        // Secondary index: rep₁ -> `(rep₂, representative in result)` for relation intersection.
        let mut intersections: OrderedHashMap<VariableId, Vec<(VariableId, VariableId)>> =
            OrderedHashMap::default();
        for (&(rep1, rep2), vars) in groups.iter() {
            intersections.entry(rep1).or_default().push((rep2, result.find(vars[0])));
        }

        merge_relations(&info1, &info2, &intersections, &mut result, &mut self.next_placeholder);

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
                info.add_equality(snapshot_stmt.input.var_id, snapshot_stmt.original());
                info.set_relation(
                    Relation::Snapshot(snapshot_stmt.input.var_id),
                    snapshot_stmt.snapshot(),
                );
            }

            Statement::Desnap(desnap_stmt) => {
                let input_rep = info.find(desnap_stmt.input.var_id);
                if let Some(Relation::Snapshot(old_inner)) = info.reverse.get(&input_rep) {
                    info.add_equality(*old_inner, desnap_stmt.output);
                } else {
                    info.set_relation(
                        Relation::Snapshot(desnap_stmt.output),
                        desnap_stmt.input.var_id,
                    );
                }
            }

            Statement::IntoBox(into_box_stmt) => {
                info.set_relation(Relation::Box(into_box_stmt.input.var_id), into_box_stmt.output);
            }

            Statement::Unbox(unbox_stmt) => {
                let input_rep = info.find(unbox_stmt.input.var_id);
                if let Some(Relation::Box(old_inner)) = info.reverse.get(&input_rep) {
                    info.add_equality(*old_inner, unbox_stmt.output);
                } else {
                    info.set_relation(Relation::Box(unbox_stmt.output), unbox_stmt.input.var_id);
                }
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
                let fields =
                    struct_stmt.inputs.iter().map(|i| FieldVar::Var(info.find(i.var_id))).collect();
                info.set_relation(Relation::StructConstruct(ty, fields), struct_stmt.output);
            }

            Statement::StructDestructure(struct_stmt) => {
                // (outputs...) = struct_destructure(input)
                let struct_var = info.find(struct_stmt.input.var_id);
                // 1. If input was previously constructed, union outputs with original fields. Skip
                //    placeholder fields (unknown after merge).
                if let Some((_, field_reps)) = info.get_struct_construct(struct_var) {
                    for (&output, field) in struct_stmt.outputs.iter().zip(field_reps.iter()) {
                        if let FieldVar::Var(field_rep) = field {
                            info.add_equality(*field_rep, output);
                        }
                    }
                }
                // 2. Record: struct_construct(outputs) == input (for future constructs).
                let ty = self.lowered.variables[struct_var].ty;
                let fields =
                    struct_stmt.outputs.iter().map(|&v| FieldVar::Var(info.find(v))).collect();
                info.set_relation(Relation::StructConstruct(ty, fields), struct_var);
            }

            Statement::Call(call_stmt) => {
                let Some((id, _)) = call_stmt.function.get_extern(self.db) else { return };
                if id == self.array_new {
                    let ty = self.lowered.variables[call_stmt.outputs[0]].ty;
                    info.set_relation(
                        Relation::Array(ty, ArrayItems::default()),
                        call_stmt.outputs[0],
                    );
                } else if id == self.array_append
                    && let Some((ty, items)) = info.get_array_construct(call_stmt.inputs[0].var_id)
                {
                    // Only track append if the input array is already tracked. Arrays from
                    // function parameters or external calls are conservatively ignored.
                    let elem = FieldVar::Var(info.find(call_stmt.inputs[1].var_id));
                    let new_items = items.append(elem, &mut self.next_placeholder);
                    info.set_relation(Relation::Array(ty, new_items), call_stmt.outputs[0]);
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
                    new_info.add_equality(src_usage.var_id, *dst);
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
                    let matched_rep = new_info.find(matched_var);
                    if let Some((old_variant, input)) =
                        new_info.get_enum_construct_immut(matched_rep)
                        && variant == old_variant
                    {
                        new_info.add_equality(input, arm_var);
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
