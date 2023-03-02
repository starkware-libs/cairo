use cairo_lang_defs::ids::MemberId;
use cairo_lang_semantic as semantic;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use semantic::RefArg;

use crate::VariableId;

pub trait RefContext {
    fn construct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        members: Vec<VariableId>,
    ) -> VariableId;
    fn deconstruct(
        &mut self,
        concrete_struct_id: semantic::ConcreteStructId,
        value: VariableId,
    ) -> OrderedHashMap<MemberId, VariableId>;
    fn var_ty(&self, var: VariableId) -> semantic::TypeId;
}

#[derive(Clone, Debug)]
enum Value {
    Var(VariableId),
    Borrowed(Box<Borrowed>),
}

#[derive(Clone, Debug)]
struct Borrowed {
    concrete_struct_id: semantic::ConcreteStructId,
    members: OrderedHashMap<MemberId, Value>,
}

#[derive(Clone, Default, Debug)]
pub struct SemanticVars {
    borrowed: OrderedHashMap<semantic::VarId, Value>,
}
impl SemanticVars {
    pub fn contains(&mut self, semantic_var: &semantic::VarId) -> bool {
        self.borrowed.contains_key(semantic_var)
    }

    pub fn get<TContext: RefContext>(
        &mut self,
        mut ctx: TContext,
        semantic_var: &semantic::VarId,
    ) -> Option<VariableId> {
        let value = self.borrowed.get_mut(semantic_var)?;
        Self::assemble_value(&mut ctx, value)
    }

    pub fn get_ref<TContext: RefContext>(
        &mut self,
        mut ctx: TContext,
        ref_arg: &RefArg,
    ) -> Option<VariableId> {
        let value = self.break_into_value(&mut ctx, ref_arg)?;
        Self::assemble_value(&mut ctx, value)
    }

    pub fn insert(&mut self, semantic_var: semantic::VarId, var: VariableId) {
        self.borrowed.insert(semantic_var, Value::Var(var));
    }

    pub fn update_ref<TContext: RefContext>(
        &mut self,
        mut ctx: TContext,
        ref_arg: &RefArg,
        var: VariableId,
    ) -> Option<()> {
        let value = self.break_into_value(&mut ctx, ref_arg)?;
        *value = Value::Var(var);
        Some(())
    }

    fn assemble_value<TContext: RefContext>(
        ctx: &mut TContext,
        value: &mut Value,
    ) -> Option<VariableId> {
        eprintln!("assemble_value {value:?}");
        Some(match value {
            Value::Var(var) => *var,
            Value::Borrowed(borrowed) => {
                let members = borrowed
                    .members
                    .iter_mut()
                    .map(|(_, value)| Self::assemble_value(ctx, value))
                    .collect::<Option<_>>()?;
                // TODO: Emit construct statement.
                let var = ctx.construct(borrowed.concrete_struct_id, members);
                *value = Value::Var(var);
                var
            }
        })
    }

    fn break_into_value<TContext: RefContext>(
        &mut self,
        ctx: &mut TContext,
        ref_arg: &RefArg,
    ) -> Option<&mut Value> {
        eprintln!("break_into_value {ref_arg:?}");
        match ref_arg {
            RefArg::Var(expr) => self.borrowed.get_mut(&expr.var),
            RefArg::Member { parent, member_id, concrete_struct_id, .. } => {
                let parent_value = self.break_into_value(ctx, parent)?;
                match parent_value {
                    Value::Var(var) => {
                        // TODO: Emit destruct statement.
                        let members = ctx.deconstruct(*concrete_struct_id, *var);
                        let members = OrderedHashMap::from_iter(
                            members
                                .into_iter()
                                .map(|(member_id, var)| (member_id, Value::Var(var))),
                        );
                        let borrowed =
                            Borrowed { concrete_struct_id: *concrete_struct_id, members };
                        *parent_value = Value::Borrowed(Box::new(borrowed));

                        extract_matches!(parent_value, Value::Borrowed).members.get_mut(member_id)
                    }
                    Value::Borrowed(borrowed) => borrowed.members.get_mut(member_id),
                }
            }
        }
    }
}
