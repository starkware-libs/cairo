#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering as lowering;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

use crate::block_generator::generate_function_statements;
use crate::db::SierraGenGroup;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::{SierraGenVar, find_variable_lifetime};
use crate::local_variables::{AnalyzeApChangesResult, analyze_ap_changes};
use crate::pre_sierra;
use crate::store_variables::{LibfuncInfo, LocalVariables, add_store_statements};
use crate::utils::{
    alloc_local_libfunc_id, disable_ap_tracking_libfunc_id, finalize_locals_libfunc_id,
    get_concrete_libfunc_id, get_libfunc_signature, revoke_ap_tracking_libfunc_id,
    simple_basic_statement,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SierraFunctionWithBodyData {
    pub function: Maybe<Arc<pre_sierra::Function>>,
}

/// Query implementation of [SierraGenGroup::priv_function_with_body_sierra_data].
pub fn priv_function_with_body_sierra_data(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> SierraFunctionWithBodyData {
    let function = get_function_code(db, function_id);
    SierraFunctionWithBodyData { function }
}

/// Query implementation of [SierraGenGroup::function_with_body_sierra].
pub fn function_with_body_sierra(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<Arc<pre_sierra::Function>> {
    db.priv_function_with_body_sierra_data(function_id).function
}

fn get_function_code(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> Maybe<Arc<pre_sierra::Function>> {
    let signature = function_id.signature(db.upcast())?;
    let lowered_function = &*db.final_concrete_function_with_body_lowered(function_id)?;
    let root_block = lowered_function.blocks.root_block()?;

    // Find the local variables.
    let AnalyzeApChangesResult { known_ap_change, local_variables, ap_tracking_configuration } =
        analyze_ap_changes(db, lowered_function)?;

    // Get lifetime information.
    let lifetime = find_variable_lifetime(lowered_function, &local_variables)?;

    let mut context = ExprGeneratorContext::new(
        db,
        lowered_function,
        function_id,
        &lifetime,
        ap_tracking_configuration,
    );

    // If the function starts with `revoke_ap_tracking` then we can avoid
    // the first `disable_ap_tracking`.
    if let Some(lowering::Statement::Call(call_stmt)) = root_block.statements.first() {
        if get_concrete_libfunc_id(db, call_stmt.function, false).1
            == revoke_ap_tracking_libfunc_id(db)
        {
            context.set_ap_tracking(false);
        }
    }

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let parameters = lowered_function
        .parameters
        .iter()
        .map(|param_id| {
            Ok(cairo_lang_sierra::program::Param {
                id: context.get_sierra_variable(*param_id),
                ty: db.get_concrete_type_id(lowered_function.variables[*param_id].ty)?,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let ret_types = vec![db.get_concrete_type_id(signature.return_type)?];

    context.push_statement(label);

    let sierra_local_variables = allocate_local_variables(&mut context, &local_variables)?;

    // Revoking ap tracking as the first non-local command for unknown ap-change function, to allow
    // proper ap-equation solving. TODO(orizi): Fix the solver to not require this constraint.
    if !known_ap_change && context.get_ap_tracking() {
        context.push_statement(simple_basic_statement(
            disable_ap_tracking_libfunc_id(db),
            &[],
            &[],
        ));
        context.set_ap_tracking(false);
    }

    // Generate the function's code.
    let statements = generate_function_statements(context)?;

    let statements = add_store_statements(
        db,
        statements,
        &|concrete_lib_func_id: ConcreteLibfuncId| -> LibfuncInfo {
            LibfuncInfo { signature: get_libfunc_signature(db, concrete_lib_func_id) }
        },
        sierra_local_variables,
        &parameters,
    );

    // TODO(spapini): Don't intern objects for the semantic model outside the crate. These should
    // be regarded as private.
    Ok(pre_sierra::Function {
        id: function_id.function_id(db.upcast())?.intern(db),
        body: statements,
        entry_point: label_id,
        parameters,
        ret_types,
    }
    .into())
}

/// Allocates space for the local variables.
/// Returns:
/// * A map from a Sierra variable that should be stored as local variable to its allocated space
///   (uninitialized local variable).
/// * A list of Sierra statements.
fn allocate_local_variables(
    context: &mut ExprGeneratorContext<'_>,
    local_variables: &OrderedHashSet<lowering::VariableId>,
) -> Maybe<LocalVariables> {
    let mut sierra_local_variables =
        OrderedHashMap::<cairo_lang_sierra::ids::VarId, cairo_lang_sierra::ids::VarId>::default();
    for lowering_var_id in local_variables.iter() {
        let sierra_var_id = context.get_sierra_variable(*lowering_var_id);
        let uninitialized_local_var_id =
            context.get_sierra_variable(SierraGenVar::UninitializedLocal(*lowering_var_id));
        context.push_statement(simple_basic_statement(
            alloc_local_libfunc_id(
                context.get_db(),
                context.get_variable_sierra_type(*lowering_var_id)?,
            ),
            &[],
            &[uninitialized_local_var_id.clone()],
        ));

        sierra_local_variables.insert(sierra_var_id, uninitialized_local_var_id);
    }

    // Add finalize_locals() statement.
    if !local_variables.is_empty() {
        context.push_statement(simple_basic_statement(
            finalize_locals_libfunc_id(context.get_db()),
            &[],
            &[],
        ));
    }

    Ok(sierra_local_variables)
}
