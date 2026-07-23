#[cfg(test)]
#[path = "trim_unused_params_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use itertools::Itertools;
use salsa::Database;

use crate::db::LoweringGroup;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey,
};
use crate::optimizations::config::Optimizations;
use crate::{BlockEnd, Lowered, LoweringStage, Statement, VariableId};

/// Query implementation of [crate::db::LoweringGroup::unused_parameters].
#[salsa::tracked(returns(ref))]
pub fn unused_parameters<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
) -> Vec<usize> {
    // The `TrimUnusedParams` phase is only part of the enabled optimization strategy, so when
    // optimizations are disabled no parameter is trimmed.
    if matches!(db.optimizations(), Optimizations::Disabled) {
        return vec![];
    }
    // Only compiler-generated loop functions are trimmed, as trimming the parameters of a
    // user-visible function would change its externally callable signature.
    if !matches!(
        function.long(db),
        ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
            key: GeneratedFunctionKey::Loop(_),
            ..
        })
    ) {
        return vec![];
    }
    let Ok(lowered) = db.lowered_body(function, LoweringStage::PreOptimizations) else {
        return vec![];
    };
    if lowered.blocks.is_empty() {
        return vec![];
    }
    let mut used = UnorderedHashSet::<VariableId>::default();
    for (_, block) in lowered.blocks.iter() {
        for stmt in &block.statements {
            used.extend(stmt.inputs().iter().map(|input| input.var_id));
        }
        match &block.end {
            BlockEnd::Return(vars, _) => used.extend(vars.iter().map(|var| var.var_id)),
            BlockEnd::Panic(var) => {
                used.insert(var.var_id);
            }
            BlockEnd::Goto(_, remapping) => used.extend(remapping.values().map(|src| src.var_id)),
            BlockEnd::Match { info } => used.extend(info.inputs().iter().map(|input| input.var_id)),
            BlockEnd::NotSet => {}
        }
    }
    lowered.parameters.iter().positions(|param| !used.contains(param)).collect()
}

/// Removes parameters that are never used by the function's body, from both the function's
/// signature and from every call site.
///
/// Note that the removed positions refer to the `PreOptimizations` stage layout, so this phase is
/// not idempotent and must be applied exactly once, directly on the `PreOptimizations` lowering.
pub fn trim_unused_params<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
) -> Maybe<()> {
    let unused = db.unused_parameters(function);
    if !unused.is_empty() {
        debug_assert_eq!(lowered.parameters.len(), lowered.signature.params.len());
        remove_indices(&mut lowered.parameters, unused);
        remove_indices(&mut lowered.signature.params, unused);
    }
    for block in lowered.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            let Statement::Call(call_stmt) = stmt else {
                continue;
            };
            let Some(callee) = call_stmt.function.body(db)? else {
                continue;
            };
            let unused = db.unused_parameters(callee);
            if !unused.is_empty() {
                // Note that if `call_stmt.with_coupon` is true, the last input is the coupon,
                // which is beyond the parameters and is therefore never removed.
                remove_indices(&mut call_stmt.inputs, unused);
            }
        }
    }
    Ok(())
}

/// Removes the elements at the given sorted `indices` from `vec`.
fn remove_indices<T>(vec: &mut Vec<T>, indices: &[usize]) {
    for idx in indices.iter().rev() {
        vec.remove(*idx);
    }
}
