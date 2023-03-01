//! Remove unnecessary remapping of variables optimization.
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
use crate::{BlockId, FlatBlockEnd, FlatLowered, VarRemapping, VariableId};

fn visit_remappings<F: FnMut(&mut VarRemapping)>(lowered: &mut FlatLowered, mut f: F) {
    for block in lowered.blocks.0.iter_mut() {
        match &mut block.end {
            FlatBlockEnd::Fallthrough(_, remapping) | FlatBlockEnd::Goto(_, remapping) => {
                f(remapping)
            }
            FlatBlockEnd::Return(_) | FlatBlockEnd::Match { .. } => {}
            FlatBlockEnd::NotSet => unreachable!(),
        }
    }
}

#[derive(Default)]
struct Context {
    dest_to_srcs: HashMap<VariableId, Vec<VariableId>>,
    var_representatives: HashMap<VariableId, VariableId>,
    variable_used: HashSet<VariableId>,
}
impl Context {
    fn set_used(&mut self, var: VariableId) {
        if self.variable_used.insert(var) {
            for src in self.dest_to_srcs.get(&var).cloned().unwrap_or_default() {
                self.set_used(src);
            }
        }
    }
}
impl Rebuilder for Context {
    fn map_var_id(&mut self, var: VariableId) -> VariableId {
        if let Some(res) = self.var_representatives.get(&var) {
            *res
        } else {
            let srcs = self.dest_to_srcs.get(&var).cloned().unwrap_or_default();
            let src_representatives = srcs.iter().map(|src| self.map_var_id(*src)).collect_vec();
            let new_var =
                if let [single_var] = &src_representatives[..] { *single_var } else { var };
            self.var_representatives.insert(var, new_var);
            new_var
        }
    }

    fn map_block_id(&mut self, block: BlockId) -> BlockId {
        block
    }

    fn transform_remapping(&mut self, remapping: &mut VarRemapping) {
        let mut new_remapping = VarRemapping::default();
        for (dst, src) in remapping.iter() {
            if dst != src && self.variable_used.contains(dst) {
                new_remapping.insert(*dst, *src);
            }
        }
        *remapping = new_remapping;
    }
}

pub fn optimize_remappings(lowered: &mut FlatLowered) {
    // Find condition 1 (see module doc).
    let mut ctx = Context::default();
    visit_remappings(lowered, |remapping| {
        for (dst, src) in remapping.iter() {
            ctx.dest_to_srcs.entry(*dst).or_default().push(*src);
        }
    });

    // Find condition 2 (see module doc).
    for (_, block) in lowered.blocks.iter() {
        for stmt in &block.statements {
            for var in stmt.inputs() {
                let var = ctx.map_var_id(var);
                ctx.set_used(var);
            }
        }
        match &block.end {
            FlatBlockEnd::Return(returns) => {
                for var in returns {
                    let var = ctx.map_var_id(*var);
                    ctx.set_used(var);
                }
            }
            FlatBlockEnd::Fallthrough(_, _) | FlatBlockEnd::Goto(_, _) => {}
            FlatBlockEnd::Match { info } => {
                for var in info.inputs() {
                    let var = ctx.map_var_id(var);
                    ctx.set_used(var);
                }
            }
            FlatBlockEnd::NotSet => unreachable!(),
        }
    }

    // Rebuild the blocks without unnecessary remappings.
    for block in lowered.blocks.0.iter_mut() {
        *block = ctx.rebuild_block(block);
    }
}
