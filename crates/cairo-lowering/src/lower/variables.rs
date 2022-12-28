use utils::ordered_hash_set::OrderedHashSet;

use super::context::LoweringContext;
use crate::{Variable, VariableId};

/// Wrapper around VariableId, guaranteeing that the variable exists inside some [LivingVariables]
/// struct. Thus, it does not implement copy nor clone.
#[derive(Debug)]
pub struct LivingVar(VariableId);
impl LivingVar {
    /// Duplicates the variable if it is duplicatable.
    pub fn try_duplicate(&self, ctx: &LoweringContext<'_>) -> Option<Self> {
        if ctx.variables[self.0].duplicatable { Some(LivingVar(self.0)) } else { None }
    }
    /// Retrieves the [VariableId] of this var.
    pub fn var_id(&self) -> VariableId {
        self.0
    }
}

/// Wrapper around VariableId, which result from "using" or "taking" a living variable.
pub struct UsableVariable(VariableId);
impl UsableVariable {
    /// Retrieves the [VariableId] of this var.
    pub fn var_id(&self) -> VariableId {
        self.0
    }
}

/// Maintains the set of living variables at a certain scope.
#[derive(Default)]
pub struct LivingVariables {
    /// The set of living variables.
    living_variables: OrderedHashSet<VariableId>,
}
impl LivingVariables {
    /// Gets a variable, removing from `living_variables` if not duplicatable.
    pub fn use_var(&mut self, ctx: &LoweringContext<'_>, var: LivingVar) -> UsableVariable {
        let var_id = var.0;
        if ctx.variables[var_id].duplicatable {
            assert!(self.living_variables.contains(&var_id), "Unexpected dead variable.");
            return UsableVariable(var_id);
        }
        self.take_var(var)
    }

    /// Take a variable, removing from `living_variables`.
    pub fn take_var(&mut self, var: LivingVar) -> UsableVariable {
        let var_id = var.0;
        assert!(self.living_variables.swap_remove(&var_id), "Unexpected dead variable.");
        UsableVariable(var_id)
    }

    /// Introduces a new variable into `living_variables`.
    pub fn introduce_new_var(
        &mut self,
        ctx: &mut LoweringContext<'_>,
        ty: semantic::TypeId,
    ) -> LivingVar {
        let ty_info = ctx.db.type_info(ctx.lookup_context.clone(), ty).unwrap_or_default();
        let var_id = ctx.variables.alloc(Variable {
            duplicatable: ty_info.duplicatable,
            droppable: ty_info.droppable,
            ty,
        });
        self.introduce_var(UsableVariable(var_id))
    }

    /// Introduces a variable into `living_variables`.
    pub fn introduce_var(&mut self, var: UsableVariable) -> LivingVar {
        assert!(self.living_variables.insert(var.0), "Unexpected reintroduced variable.");
        LivingVar(var.0)
    }

    /// Retrieves the set of living variables as an ordered vector in order to drop / destruct.
    pub fn get_all(&self) -> Vec<VariableId> {
        self.living_variables.iter().copied().collect()
    }

    // Returns true if the given var_id exists as a living var.
    pub fn contains(&self, var_id: VariableId) -> bool {
        self.living_variables.contains(&var_id)
    }
}

/// A container for living variables that is allowed to "split" variables, even if they are not
/// duplicatable. This is intended for "parallel" scopes use case, for example, match statements
/// with multiple parallel scopes for each arm.
#[derive(Default)]
pub struct Splitter {
    /// The contained [LivingVariables].
    living_variables: LivingVariables,
}
impl Splitter {
    /// Adds a variable to the living variables.
    pub fn add(&mut self, var: UsableVariable) -> LivingVar {
        self.living_variables.introduce_var(var)
    }
    /// Splits the variable, and get a [UsableVariable] from it. This can be repeated indefinitely.
    /// It does not represent any ownership taking.
    pub fn split(&self, var: &LivingVar) -> UsableVariable {
        assert!(
            self.living_variables.living_variables.contains(&var.0),
            "Unexpected dead variable."
        );
        UsableVariable(var.0)
    }
}
