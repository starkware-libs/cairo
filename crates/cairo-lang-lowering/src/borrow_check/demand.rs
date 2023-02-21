use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

/// Demanded variables from a certain point in the flow until the end of the function.
#[derive(Clone)]
pub struct Demand<Var: std::hash::Hash + Eq + Copy> {
    pub vars: OrderedHashSet<Var>,
}
pub trait DemandReporter<Var, Position> {
    fn drop(&mut self, position: Position, var: Var);
    fn dup(&mut self, position: Position, var: Var);
}

impl<Var: std::hash::Hash + Eq + Copy> Default for Demand<Var> {
    fn default() -> Self {
        Self { vars: Default::default() }
    }
}
impl<Var: std::hash::Hash + Eq + Copy> Demand<Var> {
    /// Updates the demand when some variables are used right before the current flow.
    pub fn variables_used<Position: Copy, T: DemandReporter<Var, Position>>(
        &mut self,
        reporter: &mut T,
        vars: &[Var],
        position: Position,
    ) {
        for var in vars {
            if !self.vars.insert(*var) {
                // Variable already used. If it's not dup, that is an issue.
                reporter.dup(position, *var);
            }
        }
    }

    /// Updates the demand when some variables are introduced right before the current flow.
    pub fn variables_introduced<Position: Copy, T: DemandReporter<Var, Position>>(
        &mut self,
        reporter: &mut T,
        vars: &[Var],
        position: Position,
    ) {
        for var in vars {
            if !self.vars.swap_remove(var) {
                // Variable introduced, but not demanded. If it's not drop, that is an issue.
                reporter.drop(position, *var);
            }
        }
    }

    /// Merges [Demand]s from multiple branches into one, reporting diagnostics in the way.
    pub fn merge_demands<Position: Copy, T: DemandReporter<Var, Position>>(
        demands: Vec<Self>,
        reporter: &mut T,
        position: Position,
    ) -> Self {
        // Union demands.
        let mut demand = Self::default();
        for arm_demand in &demands {
            demand.vars.extend(arm_demand.vars.iter().copied());
        }
        // Check each var.
        for var in demand.vars.iter() {
            for arm_demand in &demands {
                if !arm_demand.vars.contains(var) {
                    // Variable demanded only on some branches. It should be dropped in other.
                    // If it's not drop, that is an issue.
                    reporter.drop(position, *var);
                }
            }
        }
        demand
    }
}
