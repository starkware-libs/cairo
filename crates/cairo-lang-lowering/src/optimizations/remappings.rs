//! Remove unnecessary remapping of variables optimization.
//!
//! At each convergence, we have one or more branches with remappings of variables.
//! A destination variable `dest` introduced by the remappings must be remapped at every branch
//! `b_i` by mapping a source variable `src_i->dest`.
//! We require that every use of `dest` refers to the correct `src_i`.
//! This means that the remappings to `dest` are not necessary in these cases:
//! 1. There is no flow that uses the "value" of `dest` after the convergence.
//! 2. All the `src_i` variables get the same "value".

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
pub(crate) struct Context {
    /// Maps a destination variable to the source variables that are remapped to it.
    pub dest_to_srcs: Vec<Vec<VariableId>>,
    /// Cache of a mapping from variable id in the old lowering to variable id in the new lowering.
    /// This mapping is built on demand.
    var_representatives: Vec<Option<VariableId>>,
    /// The set of variables that is used by a reachable blocks.
    variable_used: Vec<bool>,
}
impl Context {
    pub fn new(count: usize) -> Self {
        Self {
            dest_to_srcs: vec![Default::default(); count],
            var_representatives: vec![None; count],
            variable_used: vec![false; count],
        }
    }
    /// Find the `canonical` variable that `var` maps to and mark it as used.
    pub fn set_used(&mut self, var: VariableId) {
        set_used_impl(
            &self.dest_to_srcs,
            &mut self.var_representatives,
            &mut self.variable_used,
            var,
        );
    }
}
impl<'db> Rebuilder<'db> for Context {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        map_var_id_impl(&self.dest_to_srcs, &mut self.var_representatives, var)
    }

    fn transform_remapping(&mut self, remapping: &mut VarRemapping<'db>) {
        let mut new_remapping = VarRemapping::default();
        for (dst, src) in remapping.iter() {
            if dst != &src.var_id && self.variable_used[dst.index()] {
                new_remapping.insert(*dst, *src);
            }
        }
        *remapping = new_remapping;
    }
}

/// Implementation of the [Context::set_used] method, used directly on the members, so can be
/// partially mutable.
fn set_used_impl(
    dest_to_srcs: &Vec<Vec<VariableId>>,
    var_representatives: &mut Vec<Option<VariableId>>,
    variable_used: &mut Vec<bool>,
    var: VariableId,
) {
    let var = map_var_id_impl(dest_to_srcs, var_representatives, var);
    if !variable_used[var.index()] {
        variable_used[var.index()] = true;
        for src in dest_to_srcs[var.index()].iter() {
            set_used_impl(dest_to_srcs, var_representatives, variable_used, *src);
        }
    }
}

/// Implementation of the [Context::map_var_id] method, used directly on the members, so can be
/// partially mutable.
fn map_var_id_impl(
    dest_to_srcs: &Vec<Vec<VariableId>>,
    var_representatives: &mut Vec<Option<VariableId>>,
    var: VariableId,
) -> VariableId {
    if let Some(res) = var_representatives[var.index()] {
        return res;
    }
    let new_var = if let Some([single_var]) = dest_to_srcs[var.index()]
        .iter()
        .map(|src| map_var_id_impl(dest_to_srcs, var_representatives, *src))
        .unique()
        .collect_array()
    {
        single_var
    } else {
        var
    };
    var_representatives[var.index()] = Some(new_var);
    new_var
}

pub fn optimize_remappings<'db>(lowered: &mut Lowered<'db>) {
    if lowered.blocks.has_root().is_err() {
        return;
    }

    // Find condition 1 (see module doc).
    let mut ctx = Context::new(lowered.variables.len());
    let reachable = visit_remappings(lowered, |remapping| {
        for (dst, src) in remapping.iter() {
            ctx.dest_to_srcs[dst.index()].push(src.var_id);
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
