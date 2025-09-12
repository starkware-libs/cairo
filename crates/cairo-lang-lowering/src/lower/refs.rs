use cairo_lang_defs::ids::MemberId;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::expr::inference::InferenceError;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{self as semantic, ConcreteTypeId, TypeLongId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, extract_matches, try_extract_matches};
use itertools::{Itertools, chain};

use super::block_builder::BlockStructRecomposer;
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
                let members_res = scattered
                    .members
                    .iter_mut()
                    .map(|(_, value)| Self::assemble_value(ctx, value))
                    .collect::<Result<_, _>>();

                match members_res {
                    Ok(members) => {
                        let var = ctx.reconstruct(scattered.concrete_struct_id, members);
                        *value = Value::Var(var);
                        Ok(var)
                    }
                    Err(MovedVar { ty: _, inference_error, last_use_location }) => {
                        // Don't use the type of the moved member. Replace it with the type of the
                        // variable.
                        let y = TypeLongId::<'db>::Concrete(ConcreteTypeId::Struct(
                            scattered.concrete_struct_id,
                        ));
                        let x = y.intern(ctx.ctx.db);
                        Err(MovedVar { ty: x, inference_error, last_use_location })
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

        let MemberPath::Member { parent, member_id, concrete_struct_id, .. } = path else {
            return None;
        };

        let parent_value = self.break_into_value(ctx, parent)?;
        match parent_value {
            Value::Var(var) => {
                let members = ctx.deconstruct(*concrete_struct_id, *var);
                let members = OrderedHashMap::from_iter(
                    members.into_iter().map(|(member_id, var)| (member_id, Value::Var(var))),
                );
                let scattered = Scattered { concrete_struct_id: *concrete_struct_id, members };
                *parent_value = Value::Scattered(Box::new(scattered));
            }
            Value::MovedVar(MovedVar { ty: _, inference_error, last_use_location }) => {
                let member_map = ctx.ctx.db.concrete_struct_members(*concrete_struct_id).unwrap();
                let members = OrderedHashMap::from_iter(member_map.values().map(|member| {
                    (
                        member.id,
                        Value::MovedVar(MovedVar {
                            ty: member.ty,
                            inference_error: inference_error.clone(),
                            last_use_location: *last_use_location,
                        }),
                    )
                }));
                let scattered = Scattered { concrete_struct_id: *concrete_struct_id, members };
                *parent_value = Value::Scattered(Box::new(scattered));
            }
            Value::Scattered(..) => {}
        };
        extract_matches!(parent_value, Value::Scattered).members.get_mut(member_id)
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

    let concrete_struct_id = only_scattered[0].concrete_struct_id;
    let members = only_scattered[0]
        .members
        .keys()
        .map(|member_id| {
            let member_path = MemberPath::Member {
                parent: parent_path.clone().into(),
                member_id: *member_id,
                concrete_struct_id,
            };
            // Call `compute_remapped_variables` recursively on the scattered values.
            // If there is a [Value::Var], `require_remapping` will be set to `true` to account
            // for it.
            let member_values =
                only_scattered.iter().map(|scattered| &scattered.members[member_id]).collect_vec();

            (
                *member_id,
                compute_remapped_variables(
                    &member_values,
                    require_remapping,
                    &member_path,
                    remapped_callback,
                ),
            )
        })
        .collect();

    Value::Scattered(Box::new(Scattered { concrete_struct_id, members }))
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
    concrete_struct_id: semantic::ConcreteStructId<'db>,
    members: OrderedHashMap<MemberId<'db>, Value<'db>>,
}
