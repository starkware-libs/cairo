/// ! This module provides the Demand utility struct used for analyzing usage of variables.
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

/// A reporting trait that reports each variables dup, drop and last_use positions.
pub trait DemandReporter<Var, Aux = ()> {
    type UsePosition: Copy;
    type IntroducePosition: Copy;
    fn drop_aux(&mut self, position: Self::IntroducePosition, var: Var, _aux: Aux) {
        self.drop(position, var);
    }
    fn drop(&mut self, _position: Self::IntroducePosition, _var: Var) {}
    fn dup(
        &mut self,
        _position: Self::UsePosition,
        _var: Var,
        _next_usage_position: Self::UsePosition,
    ) {
    }
    fn last_use(&mut self, _position: Self::UsePosition, _var: Var) {}
    fn unused_mapped_var(&mut self, _var: Var) {}
}

/// Demanded variables from a certain point in the flow until the end of the function.
/// Needs to be updates in backwards order.
#[derive(Clone)]
pub struct Demand<Var: std::hash::Hash + Eq + Copy, UsePosition, Aux: Clone + Default = ()> {
    pub vars: OrderedHashMap<Var, UsePosition>,
    pub aux: Aux,
}
impl<Var: std::hash::Hash + Eq + Copy, UsePosition, Aux: Clone + Default> Default
    for Demand<Var, UsePosition, Aux>
{
    fn default() -> Self {
        Self { vars: Default::default(), aux: Default::default() }
    }
}
impl<Var: std::hash::Hash + Eq + Copy, UsePosition: Copy, Aux: Clone + Default + AuxCombine>
    Demand<Var, UsePosition, Aux>
{
    /// Finalizes a demand. Returns a boolean representing success - if all the variable demands
    /// were satisfied.
    pub fn finalize(self) -> bool {
        self.vars.is_empty()
    }

    /// Updates the demand when a variable remapping occurs.
    pub fn apply_remapping<
        'a,
        V: Copy + Into<Var> + 'a,
        T: DemandReporter<Var, Aux, UsePosition = UsePosition>,
    >(
        &mut self,
        reporter: &mut T,
        remapping: impl std::iter::DoubleEndedIterator<Item = (&'a V, (&'a V, T::UsePosition))>
        + std::iter::ExactSizeIterator,
    ) {
        // Traverse the remapping in reverse order, as remappings can use the same variable more
        // than once, and the whole usage is analyzed in reverse.
        for (dst, (src, position)) in remapping.rev() {
            let src = (*src).into();
            let dst = (*dst).into();
            if let Some(dest_next_usage_position) = self.vars.swap_remove(&dst) {
                if let Some(next_usage_position) = self.vars.insert(src, dest_next_usage_position) {
                    reporter.dup(position, src, next_usage_position);
                } else {
                    reporter.last_use(position, src);
                }
            } else {
                reporter.unused_mapped_var(dst);
            }
        }
    }

    /// Updates the demand when some variables are used right before the current flow.
    pub fn variables_used<
        'a,
        V: Copy + Into<Var> + 'a,
        T: DemandReporter<Var, Aux, UsePosition = UsePosition>,
    >(
        &mut self,
        reporter: &mut T,
        vars: impl std::iter::DoubleEndedIterator<Item = (&'a V, T::UsePosition)>
        + std::iter::ExactSizeIterator,
    ) {
        for (var, position) in vars.rev() {
            if let Some(next_usage_position) = self.vars.insert((*var).into(), position) {
                // Variable already used. If it's not dup, that is an issue.
                reporter.dup(position, (*var).into(), next_usage_position);
            } else {
                reporter.last_use(position, (*var).into());
            }
        }
    }

    /// Updates the demand when some variables are introduced right before the current flow.
    pub fn variables_introduced<V: Copy + Into<Var>, T: DemandReporter<Var, Aux>>(
        &mut self,
        reporter: &mut T,
        vars: &[V],
        position: T::IntroducePosition,
    ) {
        for var in vars {
            if self.vars.swap_remove(&(*var).into()).is_none() {
                // Variable introduced, but not demanded. If it's not drop, that is an issue.
                reporter.drop_aux(position, (*var).into(), self.aux.clone());
            }
        }
    }

    /// Merges [Demand]s from multiple branches into one, reporting diagnostics in the way.
    pub fn merge_demands<T: DemandReporter<Var, Aux>>(
        demands: &[(Self, T::IntroducePosition)],
        reporter: &mut T,
    ) -> Self {
        // Union demands.
        let mut vars = OrderedHashMap::default();
        for (arm_demand, _) in demands {
            vars.extend(arm_demand.vars.iter().map(|(var, position)| (*var, *position)));
        }
        let demand = Self { vars, aux: Aux::merge(demands.iter().map(|(d, _)| &d.aux)) };
        // Check each var.
        for var in demand.vars.keys() {
            for (arm_demand, position) in demands {
                if !arm_demand.vars.contains_key(var) {
                    // Variable demanded only on some branches. It should be dropped in other.
                    // If it's not drop, that is an issue.
                    reporter.drop_aux(*position, *var, arm_demand.aux.clone());
                }
            }
        }
        demand
    }
}

pub trait AuxCombine {
    fn merge<'a, I: Iterator<Item = &'a Self>>(iter: I) -> Self
    where
        Self: 'a;
}

impl AuxCombine for () {
    fn merge<'a, I: Iterator<Item = &'a Self>>(_: I) -> Self {}
}
