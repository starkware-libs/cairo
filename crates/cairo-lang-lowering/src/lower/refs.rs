use cairo_lang_defs::ids::MemberId;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_semantic::expr::fmt::ExprFormatter;
use cairo_lang_semantic::usage::MemberPath;
use cairo_lang_semantic::{self as semantic};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::chain;

use crate::VariableId;
use crate::db::LoweringGroup;

//  Information about members captured by the closure and their types.
#[derive(Clone, Debug)]
pub struct ClosureInfo {
    // TODO(TomerStarkware): unite copiable members and snapshots into a single map.
    /// The members captured by the closure (not as snapshot).
    pub members: OrderedHashMap<MemberPath, semantic::TypeId>,
    /// The types of the captured snapshot variables.
    pub snapshots: OrderedHashMap<MemberPath, semantic::TypeId>,
}

#[derive(Clone, Default, Debug)]
pub struct SemanticLoweringMapping {
    /// Maps member paths ([MemberPath]) to lowered variable ids or scattered variable ids.
    scattered: OrderedHashMap<MemberPath, Value>,
    /// Maps captured member paths to a closure that captured them.
    pub captured: UnorderedHashMap<MemberPath, VariableId>,
    /// Maps captured member paths which are copiable to a closure that captured them.
    pub copiable_captured: UnorderedHashMap<MemberPath, VariableId>,
    /// Maps the variable id of a closure to the closure info.
    pub closures: UnorderedHashMap<VariableId, ClosureInfo>,
}
impl SemanticLoweringMapping {
    /// Returns the topmost mapped member path containing the given member path, or None no such
    /// member path exists in the mapping.
    pub fn topmost_mapped_containing_member_path(
        &mut self,
        mut member_path: MemberPath,
    ) -> Option<MemberPath> {
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

    /// Returns the scattered members of the given member path, or None if the member path is not
    /// scattered.
    pub fn get_scattered_members(&mut self, member_path: &MemberPath) -> Option<Vec<MemberPath>> {
        let Some(Value::Scattered(scattered)) = self.scattered.get(member_path) else {
            return None;
        };
        Some(
            scattered
                .members
                .iter()
                .map(|(member_id, _)| MemberPath::Member {
                    parent: member_path.clone().into(),
                    member_id: *member_id,
                    concrete_struct_id: scattered.concrete_struct_id,
                })
                .collect(),
        )
    }

    pub fn destructure_closure<TContext: StructRecomposer>(
        &mut self,
        ctx: &mut TContext,
        closure_var: VariableId,
        closure_info: &ClosureInfo,
    ) -> Vec<VariableId> {
        ctx.deconstruct_by_types(
            closure_var,
            chain!(closure_info.members.values(), closure_info.snapshots.values()).cloned(),
        )
    }

    pub fn invalidate_closure<TContext: StructRecomposer>(
        &mut self,
        ctx: &mut TContext,
        closure_var: VariableId,
    ) {
        let opt_closure = self.closures.remove(&closure_var);
        if let Some(closure_info) = opt_closure {
            let new_vars = self.destructure_closure(ctx, closure_var, &closure_info);

            // Note that members.keys() can be shorter than new_vars, as the members captured
            // as snapshots don't need to be updated.
            for (path, new_var) in closure_info.members.keys().zip(new_vars) {
                if self.captured.remove(path).is_some() {
                    self.update(ctx, path, new_var).unwrap();
                } else {
                    self.copiable_captured.remove(path);
                }
            }
        }
    }

    pub fn get<TContext: StructRecomposer>(
        &mut self,
        mut ctx: TContext,
        path: &MemberPath,
    ) -> Option<VariableId> {
        if let Some(closure_var) = self.captured.get(path) {
            self.invalidate_closure(&mut ctx, *closure_var);
        }
        let value = self.break_into_value(&mut ctx, path)?;
        Self::assemble_value(&mut ctx, value)
    }

    pub fn introduce(&mut self, path: MemberPath, var: VariableId) {
        self.scattered.insert(path, Value::Var(var));
    }

    pub fn update<TContext: StructRecomposer>(
        &mut self,
        ctx: &mut TContext,
        path: &MemberPath,
        var: VariableId,
    ) -> Option<()> {
        // TODO(TomerStarkware): check if path is captured by a closure and invalidate the closure.
        // Right now this can only happen if we take a snapshot of the variable (as the
        // snapshot function returns a new var).
        // we need the make sure the borrow checker invalidates the closure when mutable capture
        // is supported.

        let value = self.break_into_value(ctx, path)?;
        *value = Value::Var(var);
        Some(())
    }

    fn assemble_value<TContext: StructRecomposer>(
        ctx: &mut TContext,
        value: &mut Value,
    ) -> Option<VariableId> {
        Some(match value {
            Value::Var(var) => *var,
            Value::Scattered(scattered) => {
                let members = scattered
                    .members
                    .iter_mut()
                    .map(|(_, value)| Self::assemble_value(ctx, value))
                    .collect::<Option<_>>()?;
                let var = ctx.reconstruct(scattered.concrete_struct_id, members);
                *value = Value::Var(var);
                var
            }
        })
    }

    fn break_into_value<TContext: StructRecomposer>(
        &mut self,
        ctx: &mut TContext,
        path: &MemberPath,
    ) -> Option<&mut Value> {
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

                extract_matches!(parent_value, Value::Scattered).members.get_mut(member_id)
            }
            Value::Scattered(scattered) => scattered.members.get_mut(member_id),
        }
    }
}

/// A trait for deconstructing and constructing structs.
pub trait StructRecomposer {
    fn deconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        value: VariableId,
    ) -> OrderedHashMap<MemberId, VariableId>;

    fn deconstruct_by_types(
        &mut self,
        value: VariableId,
        types: impl Iterator<Item = semantic::TypeId>,
    ) -> Vec<VariableId>;

    fn reconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        members: Vec<VariableId>,
    ) -> VariableId;
    fn var_ty(&self, var: VariableId) -> semantic::TypeId;
    fn db(&self) -> &dyn LoweringGroup;
}

/// An intermediate value for a member path.
#[derive(Clone, Debug, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
enum Value {
    /// The value of member path is stored in a lowered variable.
    Var(VariableId),
    /// The value of the member path is not stored. It should be reconstructed from the member
    /// values.
    Scattered(Box<Scattered>),
}

/// A value for a non-stored member path. Recursively holds the [Value] for the members.
#[derive(Clone, Debug, DebugWithDb)]
#[debug_db(ExprFormatter<'a>)]
struct Scattered {
    concrete_struct_id: semantic::ConcreteStructId,
    members: OrderedHashMap<MemberId, Value>,
}
