use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::types::{peel_snapshots, wrap_in_snapshots};
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{self as semantic, ConcreteTypeId, MemberAccessKind, TypeLongId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, extract_matches, try_extract_matches};
use itertools::{Itertools, chain};

use super::block_builder::BlockStructRecomposer;
use super::context::LoweringContext;
use crate::VariableId;
use crate::ids::LocationId;

/// Information about members captured by the closure and their types.
#[derive(Clone, Debug)]
pub struct ClosureInfo<'db> {
    // TODO(TomerStarkware): unite copiable members and snapshots into a single map.
    /// The members captured by the closure (not as snapshot).
    pub members: OrderedHashMap<MemberPath<'db>, semantic::TypeId<'db>>,
    /// The types of the captured snapshot variables.
    pub snapshots: OrderedHashMap<MemberPath<'db>, semantic::TypeId<'db>>,
}

/// The result of `SemanticLoweringMapping::assemble_value`.
pub enum AssembleValueError<'db> {
    /// The variable was moved before.
    Moved(MovedVar<'db>),
    /// The variable is missing from `SemanticLoweringMapping::scattered`.
    Missing,
}

#[derive(Clone, Default, Debug)]
pub struct SemanticLoweringMapping<'db> {
    /// Maps member paths ([MemberPath]) to lowered variable ids or scattered variable ids.
    scattered: OrderedHashMap<MemberPath<'db>, Value<'db>>,
}
impl<'db> SemanticLoweringMapping<'db> {
    /// Returns the topmost mapped member path containing the given member path, or None no such
    /// member path exists in the mapping.
    pub fn topmost_mapped_containing_member_path(
        &self,
        mut member_path: MemberPath<'db>,
    ) -> Option<MemberPath<'db>> {
        let mut res = None;
        loop {
            if self.scattered.contains_key(&member_path) {
                res = Some(member_path.clone());
            }
            let MemberPath::Member { parent, .. } = member_path else {
                return res;
            };
            member_path = *parent;
        }
    }

    pub fn destructure_closure(
        &mut self,
        ctx: &mut BlockStructRecomposer<'_, '_, 'db>,
        closure_var: VariableId,
        closure_info: &ClosureInfo<'db>,
    ) -> Vec<VariableId> {
        ctx.deconstruct_by_types(
            closure_var,
            chain!(closure_info.members.values(), closure_info.snapshots.values()).cloned(),
        )
    }

    pub fn get(
        &mut self,
        mut ctx: BlockStructRecomposer<'_, '_, 'db>,
        path: &MemberPath<'db>,
    ) -> Result<VariableId, AssembleValueError<'db>> {
        let value = self.break_into_value(&mut ctx, path).ok_or(AssembleValueError::Missing)?;
        Self::assemble_value(&mut ctx, value).map_err(AssembleValueError::Moved)
    }

    pub fn introduce(&mut self, path: MemberPath<'db>, var: VariableId) {
        self.scattered.insert(path, Value::Var(var));
    }

    pub fn update(
        &mut self,
        ctx: &mut BlockStructRecomposer<'_, '_, 'db>,
        path: &MemberPath<'db>,
        var: VariableId,
    ) -> Option<()> {
        // TODO(TomerStarkware): check if path is captured by a closure and invalidate the closure.
        // Right now this can only happen if we take a snapshot of the variable (as the
        // snapshot function returns a new var).
        // we need to ensure the borrow checker invalidates the closure when mutable capture
        // is supported.

        let value = self.break_into_value(ctx, path)?;
        *value = Value::Var(var);
        Some(())
    }

    /// Marks the variable at the given path as moved.
    ///
    /// This function should be called for non-copyable variables.
    pub fn mark_as_used(
        &mut self,
        mut ctx: BlockStructRecomposer<'_, '_, 'db>,
        path: &MemberPath<'db>,
        moved: MovedVar<'db>,
    ) {
        *self.break_into_value(&mut ctx, path).unwrap() = Value::MovedVar(moved);
    }

    /// Assembles a [VariableId] from the given [Value] by recursively reconstructing it if it is
    /// currently deconstructed.
    ///
    /// Returns a [MovedVar] if the variable, or any of its members, were moved before.
    fn assemble_value(
        ctx: &mut BlockStructRecomposer<'_, '_, 'db>,
        value: &mut Value<'db>,
    ) -> Result<VariableId, MovedVar<'db>> {
        match value {
            Value::Var(var) => Ok(*var),
            Value::Scattered(scattered) => {
                let ty = scattered.ty;
                let members_res = scattered
                    .members
                    .iter_mut()
                    .map(|(_, value)| Self::assemble_value(ctx, value))
                    .collect::<Result<_, _>>();

                match members_res {
                    Ok(members) => {
                        let var = ctx.reconstruct(ty, members);
                        *value = Value::Var(var);
                        Ok(var)
                    }
                    Err(MovedVar { ty: _, inference_error, last_use_location }) => {
                        // Don't use the type of the moved member. Replace it with the type of the
                        // aggregate.
                        Err(MovedVar { ty, inference_error, last_use_location })
                    }
                }
            }
            Value::MovedVar(moved) => Err(moved.clone()),
        }
    }

    fn break_into_value(
        &mut self,
        ctx: &mut BlockStructRecomposer<'_, '_, 'db>,
        path: &MemberPath<'db>,
    ) -> Option<&mut Value<'db>> {
        if self.scattered.contains_key(path) {
            return self.scattered.get_mut(path);
        }

        let MemberPath::Member { parent, kind } = path else {
            return None;
        };

        // The type of the parent aggregate, taken from the member's [MemberAccessKind] (the
        // authoritative semantic type).
        let ty = aggregate_ty(ctx.ctx.db, kind);
        let parent_value = self.break_into_value(ctx, parent)?;
        match parent_value {
            Value::Var(var) => {
                let members = ctx.deconstruct(ty, *var);
                let members = OrderedHashMap::from_iter(
                    members.into_iter().map(|(kind, var)| (kind, Value::Var(var))),
                );
                let scattered = Scattered { ty, members };
                *parent_value = Value::Scattered(Box::new(scattered));
            }
            Value::MovedVar(MovedVar { inference_error, last_use_location, .. }) => {
                let members = OrderedHashMap::from_iter(
                    aggregate_members(ctx.ctx.db, ty).into_iter().map(|(kind, member_ty)| {
                        (
                            kind,
                            Value::MovedVar(MovedVar {
                                ty: member_ty,
                                inference_error: inference_error.clone(),
                                last_use_location: *last_use_location,
                            }),
                        )
                    }),
                );
                let scattered = Scattered { ty, members };
                *parent_value = Value::Scattered(Box::new(scattered));
            }
            Value::Scattered(..) => {}
        };
        extract_matches!(parent_value, Value::Scattered).members.get_mut(kind)
    }
}

impl<'db> cairo_lang_debug::debug::DebugWithDb<'db> for SemanticLoweringMapping<'db> {
    type Db = ExprFormatter<'db>;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &ExprFormatter<'db>) -> std::fmt::Result {
        for (member_path, value) in self.scattered.iter() {
            writeln!(f, "{:?}: {value}", member_path.debug(db))?;
        }
        Ok(())
    }
}

/// Merges [SemanticLoweringMapping] from multiple blocks to a single [SemanticLoweringMapping].
///
/// The mapping from semantic variables to lowered variables in the new block follows these rules:
///
/// * Variables mapped to the same lowered variable across all input blocks are kept as-is.
/// * Local variables that appear in only a subset of the blocks are removed.
/// * Variables with different mappings across blocks are remapped to a new lowered variable, by
///   invoking the `remapped_callback` function.
pub fn merge_semantics<'db, 'a>(
    mappings: impl Iterator<Item = &'a SemanticLoweringMapping<'db>>,
    remapped_callback: &mut impl FnMut(&MemberPath<'db>) -> VariableId,
) -> SemanticLoweringMapping<'db>
where
    'db: 'a,
{
    // A map from [MemberPath] to its [Value] in the `mappings` where it appears.
    // If the number of [Value]s is not the length of `mappings`, it is later dropped.
    let mut path_to_values: OrderedHashMap<MemberPath<'_>, Vec<Value<'_>>> = Default::default();

    let mut n_mappings = 0;
    for map in mappings {
        for (path, var) in map.scattered.iter() {
            path_to_values.entry(path.clone()).or_default().push(var.clone());
        }
        n_mappings += 1;
    }

    let mut scattered: OrderedHashMap<MemberPath<'_>, Value<'_>> = Default::default();
    for (path, values) in path_to_values {
        // The variable is missing in one or more of the maps.
        // It cannot be used in the merged block.
        if values.len() != n_mappings {
            continue;
        }

        let merged_value = compute_remapped_variables(
            &values.iter().collect_vec(),
            false,
            &path,
            remapped_callback,
        );
        scattered.insert(path, merged_value);
    }

    SemanticLoweringMapping { scattered }
}

/// Given a list of [Value]s that correspond to the same semantic [MemberPath] in different blocks,
/// compute the [Value] in the merge block.
///
/// If all values are the same, no remapping is needed.
/// If some of the values are [Value::Var] and some are [Value::Scattered], then all the values
/// inside the [Value::Scattered] values need to be remapped.
/// If all of them are [Value::Scattered], then it is possible that some of the members require
/// remapping and some don't.
///
/// Pass `require_remapping=true` to indicate that during the recursion we encountered a
/// [Value::Var], and thus we need to remap all the [Value::Scattered] values.
/// In particular, once we have `require_remapping=true`, all the recursive calls in the subtree
/// will have `require_remapping=true`.
///
/// For example, suppose `values` consists of two trees:
/// * `A = Scattered(Scattered(v0, v1), v2)` and
/// * `B = Scattered(Scattered(v0, v3), v4)`.
///
/// Then, the result will be:
/// * `Scattered(Scattered(v0, new_var), new_var)`,
///
/// since `v0` is the same in both trees, but the other nodes are not.
///
/// If in addition to `A` and `B`, we have another tree
/// * `C = Scattered(v5, v6)`,
///
/// then `v5` will need to be deconstructed, so `C` can be thought of as
/// * `C = Scattered(Scattered(?, ?), v6)`.
///
/// Now, the node of `v0` also requires remapping, so the result will be:
/// * `Scattered(Scattered(new_var, new_var), new_var)`.
///
/// In the recursion, when we encounter `v5`, we change `require_remapping` to `true` and drop `C`
/// from the list of values (keeping only the scattered values).
/// This signals that inside this subtree, all values need to be remapped (because of the children
/// of `v5`, which are marked by `?` above).
fn compute_remapped_variables<'db>(
    values: &[&Value<'db>],
    require_remapping: bool,
    parent_path: &MemberPath<'db>,
    remapped_callback: &mut impl FnMut(&MemberPath<'db>) -> VariableId,
) -> Value<'db> {
    if let Some(x) = values.iter().find(|value| matches!(value, Value::MovedVar { .. })) {
        // If any of the values being merged is a [MovedVar], the result will be a [MovedVar].
        // Return an arbitrary one of them.
        return (*x).clone();
    }

    if !require_remapping {
        // If all values are the same, no remapping is needed.
        let first_var = values[0];
        if values.iter().all(|x| *x == first_var) {
            return first_var.clone();
        }
    }

    // Collect all the `Value::Scattered` values.
    let only_scattered: Vec<&Box<Scattered<'_>>> =
        values.iter().filter_map(|value| try_extract_matches!(value, Value::Scattered)).collect();

    if only_scattered.is_empty() {
        let remapped_var = remapped_callback(parent_path);
        return Value::Var(remapped_var);
    }

    // If we encountered a [Value::Var], we need to remap all the [Value::Scattered] values.
    let require_remapping = require_remapping || only_scattered.len() < values.len();

    let ty = only_scattered[0].ty;
    let members = only_scattered[0]
        .members
        .keys()
        .map(|kind| {
            let member_path =
                MemberPath::Member { parent: parent_path.clone().into(), kind: kind.clone() };
            // Call `compute_remapped_variables` recursively on the scattered values.
            // If there is a [Value::Var], `require_remapping` will be set to `true` to account
            // for it.
            let member_values =
                only_scattered.iter().map(|scattered| &scattered.members[kind]).collect_vec();

            (
                kind.clone(),
                compute_remapped_variables(
                    &member_values,
                    require_remapping,
                    &member_path,
                    remapped_callback,
                ),
            )
        })
        .collect();

    Value::Scattered(Box::new(Scattered { ty, members }))
}

/// Returns an iterator to all the [MemberPath]s that appear in both mappings and have different
/// values.
pub fn find_changed_members<'db, 'a>(
    semantics0: &'a SemanticLoweringMapping<'db>,
    semantics1: &'a SemanticLoweringMapping<'db>,
) -> impl Iterator<Item = MemberPath<'db>> + 'a {
    semantics0.scattered.iter().filter_map(|(path, value0)| {
        if let Some(value1) = semantics1.scattered.get(path)
            && value0 != value1
        {
            return Some(path.clone());
        }
        None
    })
}

/// Represents a non-copyable variable that was moved, and can no longer be used.
#[derive(Clone, Debug, DebugWithDb, Eq, PartialEq)]
#[debug_db(ExprFormatter<'db>)]
pub struct MovedVar<'db> {
    /// The type of the variable.
    pub ty: semantic::TypeId<'db>,
    /// The reason it is not copyable.
    pub inference_error: InferenceError<'db>,
    /// The location of the last use of the moved variable. This is used to report an error.
    pub last_use_location: LocationId<'db>,
}

/// An intermediate value for a member path.
#[derive(Clone, Debug, DebugWithDb, Eq, PartialEq)]
#[debug_db(ExprFormatter<'db>)]
enum Value<'db> {
    /// The value of member path is stored in a lowered variable.
    Var(VariableId),
    /// The value of the member path is not stored. If needed, it should be reconstructed from the
    /// member values.
    Scattered(Box<Scattered<'db>>),
    /// Represents a non-copyable variable that was moved, and can no longer be used.
    MovedVar(MovedVar<'db>),
}

impl<'db> std::fmt::Display for Value<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Var(var) => write!(f, "v{}", var.index()),
            Value::Scattered(scattered) => {
                write!(
                    f,
                    "Scattered({})",
                    scattered.members.values().map(|value| value.to_string()).join(", ")
                )
            }
            Value::MovedVar(..) => write!(f, "MovedVar"),
        }
    }
}

/// A value for a non-stored member path. Recursively holds the [Value] for the members.
#[derive(Clone, Debug, DebugWithDb, Eq, PartialEq)]
#[debug_db(ExprFormatter<'db>)]
struct Scattered<'db> {
    /// The type of the scattered aggregate (a struct or a tuple), used to reconstruct it.
    ty: semantic::TypeId<'db>,
    members: OrderedHashMap<MemberAccessKind<'db>, Value<'db>>,
}

/// Returns the type of the aggregate (a struct or a tuple) that a [MemberAccessKind] accesses a
/// member of.
fn aggregate_ty<'db>(
    db: &'db dyn salsa::Database,
    kind: &MemberAccessKind<'db>,
) -> semantic::TypeId<'db> {
    match kind {
        MemberAccessKind::Struct { concrete_struct_id, .. } => {
            TypeLongId::Concrete(ConcreteTypeId::Struct(*concrete_struct_id)).intern(db)
        }
        MemberAccessKind::Index { tuple_ty, .. } => *tuple_ty,
    }
}

/// Returns the ordered members of an aggregate type (struct or tuple), as pairs of
/// [MemberAccessKind] and the member type.
pub(crate) fn aggregate_members<'db>(
    db: &'db dyn salsa::Database,
    ty: semantic::TypeId<'db>,
) -> Vec<(MemberAccessKind<'db>, semantic::TypeId<'db>)> {
    match ty.long(db) {
        TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)) => db
            .concrete_struct_members(*concrete_struct_id)
            .unwrap()
            .iter()
            .map(|(_, member)| {
                (
                    MemberAccessKind::Struct {
                        concrete_struct_id: *concrete_struct_id,
                        member_id: member.id,
                    },
                    member.ty,
                )
            })
            .collect(),
        TypeLongId::Tuple(tys) => tys
            .iter()
            .enumerate()
            .map(|(index, member_ty)| (MemberAccessKind::Index { tuple_ty: ty, index }, *member_ty))
            .collect(),
        _ => unreachable!("Tried to scatter a non-aggregate type."),
    }
}

/// Returns the snapshot-wrapped lowered types of the members of the aggregate `aggregate_ty`,
/// along with the index of the member selected by `kind`. Returns `None` if `aggregate_ty` does
/// not match `kind` (e.g. a tuple index on a non-tuple type).
pub(crate) fn member_access_components<'db>(
    ctx: &LoweringContext<'db, '_>,
    aggregate_ty: semantic::TypeId<'db>,
    kind: &MemberAccessKind<'db>,
) -> Option<(Vec<semantic::TypeId<'db>>, usize)> {
    let (n_snapshots, long_ty) = peel_snapshots(ctx.db, aggregate_ty);
    match kind {
        MemberAccessKind::Struct { concrete_struct_id, member_id } => {
            let members = ctx.db.concrete_struct_members(*concrete_struct_id).ok()?;
            let member_idx = members.iter().position(|(_, member)| member.id == *member_id)?;
            let member_tys = members
                .iter()
                .map(|(_, member)| wrap_in_snapshots(ctx.db, member.ty, n_snapshots))
                .collect();
            Some((member_tys, member_idx))
        }
        MemberAccessKind::Index { index, .. } => {
            let TypeLongId::Tuple(tys) = long_ty else {
                return None;
            };
            let member_tys =
                tys.iter().map(|ty| wrap_in_snapshots(ctx.db, *ty, n_snapshots)).collect();
            Some((member_tys, *index))
        }
    }
}
