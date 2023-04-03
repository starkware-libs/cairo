/// ! This module provides the Demand utility struct used for analyzing usage of variables.
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

/// A reporting trait that reports each variables dup, drop and last_use positions.
pub trait DemandReporter<Var> {
    type UsePosition: Copy;
    type IntroducePosition: Copy;
    fn drop(&mut self, _position: Self::IntroducePosition, _var: Var) {}
    fn dup(&mut self, _position: Self::UsePosition, _var: Var) {}
    fn last_use(&mut self, _position: Self::UsePosition, _var_index: usize, _var: Var) {}
    fn unused_mapped_var(&mut self, _var: Var) {}
}

/// Demanded variables from a certain point in the flow until the end of the function.
/// Needs to be updates in backwards order.
#[derive(Clone)]
pub struct Demand<Var: std::hash::Hash + Eq + Copy> {
    pub vars: OrderedHashSet<Var>,
}
impl<Var: std::hash::Hash + Eq + Copy> Default for Demand<Var> {
    fn default() -> Self {
        Self { vars: Default::default() }
    }
}
impl<Var: std::hash::Hash + Eq + Copy> Demand<Var> {
    /// Finalizes a demand. Returns a boolean representing success - if all the variable demands
    /// were satisfied.
    pub fn finalize(self) -> bool {
        self.vars.is_empty()
    }

    /// Updates the demand when a variable remapping occurs.
    pub fn apply_remapping<V: Into<Var>, T: DemandReporter<Var>>(
        &mut self,
        reporter: &mut T,
        remapping: impl Iterator<Item = (V, V)>,
    ) {
        for (dst, src) in remapping {
            let src = src.into();
            let dst = dst.into();
            if self.vars.swap_remove(&dst) {
                self.vars.insert(src);
            } else {
                reporter.unused_mapped_var(dst);
            }
        }
    }

    /// Updates the demand when some variables are used right before the current flow.
    pub fn variables_used<V: Copy + Into<Var>, T: DemandReporter<Var>>(
        &mut self,
        reporter: &mut T,
        vars: &[V],
        position: T::UsePosition,
    ) {
        for (var_index, var) in vars.iter().enumerate().rev() {
            if !self.vars.insert((*var).into()) {
                // Variable already used. If it's not dup, that is an issue.
                reporter.dup(position, (*var).into());
            } else {
                reporter.last_use(position, var_index, (*var).into());
            }
        }
    }

    /// Updates the demand when some variables are introduced right before the current flow.
    pub fn variables_introduced<V: Copy + Into<Var>, T: DemandReporter<Var>>(
        &mut self,
        reporter: &mut T,
        vars: &[V],
        position: T::IntroducePosition,
    ) {
        for var in vars {
            if !self.vars.swap_remove(&(*var).into()) {
                // Variable introduced, but not demanded. If it's not drop, that is an issue.
                reporter.drop(position, (*var).into());
            }
        }
    }

    /// Merges [Demand]s from multiple branches into one, reporting diagnostics in the way.
    pub fn merge_demands<T: DemandReporter<Var>>(
        demands: &[(Self, T::IntroducePosition)],
        reporter: &mut T,
    ) -> Self {
        // Union demands.
        let mut demand = Self::default();
        for (arm_demand, _) in demands {
            demand.vars.extend(arm_demand.vars.iter().copied());
        }
        // Check each var.
        for var in demand.vars.iter() {
            for (arm_demand, position) in demands {
                if !arm_demand.vars.contains(var) {
                    // Variable demanded only on some branches. It should be dropped in other.
                    // If it's not drop, that is an issue.
                    reporter.drop(*position, *var);
                }
            }
        }
        demand
    }
}
