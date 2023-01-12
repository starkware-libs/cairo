use std::collections::HashSet;

use cairo_lang_defs::ids::{FunctionWithBodyId, GenericFunctionId};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::FunctionId;

/// Query implementation of
/// [crate::db::SemanticGroup::function_with_body_definition_direct_callees].
pub fn function_with_body_definition_direct_callees(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<HashSet<FunctionId>> {
    let direct_callees = match function_id {
        FunctionWithBodyId::Free(free_function_id) => db
            .priv_free_function_definition_data(free_function_id)?
            .definition
            .direct_callees
            .clone(),
        FunctionWithBodyId::Impl(impl_function_id) => db
            .priv_impl_function_definition_data(impl_function_id)?
            .definition
            .direct_callees
            .clone(),
    };
    Ok(direct_callees)
}

/// Query implementation of
/// [crate::db::SemanticGroup::function_with_body_direct_function_with_body_callees].
pub fn function_with_body_direct_function_with_body_callees(
    db: &dyn SemanticGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<HashSet<FunctionWithBodyId>> {
    Ok(db
        .function_with_body_definition_direct_callees(function_id)?
        .into_iter()
        .filter_map(|function_id| {
            match db.lookup_intern_function(function_id).function.generic_function {
                GenericFunctionId::Free(free_function) => {
                    Some(FunctionWithBodyId::Free(free_function))
                }
                GenericFunctionId::ImplFunction(impl_function) => {
                    Some(FunctionWithBodyId::Impl(impl_function))
                }
                _ => None,
            }
        })
        .collect())
}
