#[cfg(test)]
#[path = "trim_unused_params_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use salsa::Database;

use crate::analysis::UseLocation;
use crate::analysis::use_sites::UseSites;
use crate::db::LoweringGroup;
use crate::ids::{
    ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction,
    GeneratedFunctionKey,
};
use crate::optimizations::config::Optimizations;
use crate::{Lowered, LoweringStage, Statement};

/// Query implementation of [crate::db::LoweringGroup::unused_parameters].
#[salsa::tracked(returns(ref))]
pub fn unused_parameters<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
) -> Vec<usize> {
    // Only compiler-generated loop functions and specialized functions are trimmed, as trimming
    // the parameters of a user-visible function would change its externally callable signature.
    if !matches!(
        function.long(db),
        ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
            key: GeneratedFunctionKey::Loop(_),
            ..
        }) | ConcreteFunctionWithBodyLongId::Specialized(_)
    ) {
        return vec![];
    }
    let Ok(lowered) = db.lowered_body(function, LoweringStage::PostBaseline) else {
        return vec![];
    };
    if lowered.blocks.is_empty() {
        return vec![];
    }
    let use_sites = UseSites::analyze(lowered);
    lowered
        .parameters
        .iter()
        .enumerate()
        .filter(|(position, param)| {
            use_sites.use_locs(**param).all(|(loc, count)| {
                // A parameter whose only use at a location is being passed to a recursive call of
                // the function at its own position is merely threaded through the recursion, so it
                // is not considered a use - if the parameter has no other use, the entire chain is
                // dead.
                // Note that the coupon input, if present, is beyond the parameters and is
                // therefore always considered used.
                if count == 1
                    && let UseLocation::Statement((block_id, stmt_idx)) = loc
                    && let Statement::Call(call_stmt) =
                        &lowered.blocks[block_id].statements[stmt_idx]
                    && call_stmt.function.body(db) == Ok(Some(function))
                    && call_stmt.inputs.get(*position).map(|input| input.var_id) == Some(**param)
                {
                    true
                } else {
                    false
                }
            })
        })
        .map(|(position, _)| position)
        .collect()
}

/// Removes parameters that are never used by the function's body, from both the function's
/// signature and from every call site.
///
/// Note that the removed positions refer to the `PostBaseline` stage layout, so this phase is
/// not idempotent and must be applied exactly once, directly on the `PostBaseline` lowering.
pub fn trim_unused_params<'db>(
    db: &'db dyn Database,
    function: ConcreteFunctionWithBodyId<'db>,
    lowered: &mut Lowered<'db>,
) -> Maybe<()> {
    // The `TrimUnusedParams` phase is only part of the enabled optimization strategy, so when
    // optimizations are disabled no parameter is trimmed.
    if matches!(db.optimizations(), Optimizations::Disabled) {
        return Ok(());
    }
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
            remove_indices(&mut call_stmt.inputs, db.unused_parameters(callee));
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
