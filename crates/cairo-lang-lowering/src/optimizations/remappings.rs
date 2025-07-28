//! Remove unnecessary remapping of variables optimization.
//!
//! At each convergence, we have one or more branches with remappings of variables.
//! A destination variable `dest` introduced by the remappings must be remapped at every branch
//! `b_i` by mapping a source variable `src_i->dest`.
//! We require that every use of `dest` refers to the correct `src_i`.
//! This means that the remappings to `dest` are not necessary in these cases:
//! 1. There is no flow that uses the "value" of `dest` after the convergence.
//! 2. All the `src_i` variables get the same "value".

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::utils::{Rebuilder, RebuilderEx};
use crate::{BlockEnd, BlockId, Lowered, VarRemapping, VariableId};

/// Visits all the reachable remappings in the function, calls `f` on each one and returns a vector
/// indicating which blocks are reachable.
pub(crate) fn visit_remappings<'db, F: FnMut(&VarRemapping<'db>)>(
    lowered: &mut Lowered<'db>,
    mut f: F,
) -> Vec<bool> {
    let mut stack = vec![BlockId::root()];
    let mut visited = vec![false; lowered.blocks.len()];
    while let Some(block_id) = stack.pop() {
        if visited[block_id.0] {
            continue;
        }
        visited[block_id.0] = true;
        match &lowered.blocks[block_id].end {
            BlockEnd::Goto(target_block_id, remapping) => {
                stack.push(*target_block_id);
                f(remapping)
            }
            BlockEnd::Match { info } => {
                stack.extend(info.arms().iter().map(|arm| arm.block_id));
            }
            BlockEnd::Return(..) | BlockEnd::Panic(_) => {}
            BlockEnd::NotSet => unreachable!(),
        }
    }

    visited
}

/// Context for the optimize remappings optimization.
#[derive(Default)]
pub(crate) struct Context<'db> {
    /// Maps a destination variable to the source variables that are remapped to it.
    pub dest_to_srcs: HashMap<VariableId<'db>, Vec<VariableId<'db>>>,
    /// Cache of a mapping from variable id in the old lowering to variable id in the new lowering.
    /// This mapping is built on demand.
    var_representatives: HashMap<VariableId<'db>, VariableId<'db>>,
    /// The set of variables that is used by a reachable blocks.
    variable_used: HashSet<VariableId<'db>>,
}
impl<'db> Context<'db> {
    /// Find the `canonical` variable that `var` maps to and mark it as used.
    pub fn set_used(&mut self, var: VariableId<'db>) {
        let var = self.map_var_id(var);
        if self.variable_used.insert(var) {
            for src in self.dest_to_srcs.get(&var).cloned().unwrap_or_default() {
                self.set_used(src);
            }
        }
    }
}

impl<'db> Rebuilder<'db> for Context<'db> {
    fn map_var_id(&mut self, var: VariableId<'db>) -> VariableId<'db> {
        if let Some(res) = self.var_representatives.get(&var) {
            *res
        } else {
            let srcs = self.dest_to_srcs.get(&var).cloned().unwrap_or_default();
            let src_representatives: HashSet<_> =
                srcs.iter().map(|src| self.map_var_id(*src)).collect();
            let src_representatives = src_representatives.into_iter().collect_vec();
            let new_var =
                if let [single_var] = &src_representatives[..] { *single_var } else { var };
            self.var_representatives.insert(var, new_var);
            new_var
        }
    }

    fn transform_remapping(&mut self, remapping: &mut VarRemapping<'db>) {
        let mut new_remapping = VarRemapping::default();
        for (dst, src) in remapping.iter() {
            if dst != &src.var_id && self.variable_used.contains(dst) {
                new_remapping.insert(*dst, *src);
            }
        }
        *remapping = new_remapping;
    }
}

pub fn optimize_remappings<'db>(lowered: &mut Lowered<'db>) {
    if lowered.blocks.has_root().is_err() {
        return;
    }

    // Find condition 1 (see module doc).
    let mut ctx = Context::default();
    let reachable = visit_remappings(lowered, |remapping| {
        for (dst, src) in remapping.iter() {
            ctx.dest_to_srcs.entry(*dst).or_default().push(src.var_id);
        }
    });

    // Find condition 2 (see module doc).
    for ((_, block), is_reachable) in lowered.blocks.iter().zip(reachable) {
        if !is_reachable {
            continue;
        }

        for stmt in &block.statements {
            for var_usage in stmt.inputs() {
                ctx.set_used(var_usage.var_id);
            }
        }
        match &block.end {
            BlockEnd::Return(returns, _location) => {
                for var_usage in returns {
                    ctx.set_used(var_usage.var_id);
                }
            }
            BlockEnd::Panic(data) => {
                ctx.set_used(data.var_id);
            }
            BlockEnd::Goto(_, _) => {}
            BlockEnd::Match { info } => {
                for var_usage in info.inputs() {
                    ctx.set_used(var_usage.var_id);
                }
            }
            BlockEnd::NotSet => unreachable!(),
        }
    }

    // Rebuild the blocks without unnecessary remappings.
    for block in lowered.blocks.iter_mut() {
        *block = ctx.rebuild_block(block);
    }
}
