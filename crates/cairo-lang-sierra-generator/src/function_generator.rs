#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_lowering::{self as lowering, Lowered, LoweringStage};
use cairo_lang_sierra::extensions::lib_func::SierraApChange;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use itertools::{Itertools, zip_eq};
use salsa::Database;

use crate::block_generator::generate_function_result;
use crate::db::SierraGenGroup;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::{SierraGenVar, find_variable_lifetime};
use crate::local_variables::{AnalyzeApChangesResult, analyze_ap_changes};
use crate::pre_sierra;
use crate::store_variables::{LocalVariables, add_store_statements};
use crate::utils::{
    alloc_local_libfunc_id, disable_ap_tracking_libfunc_id, dummy_call_libfunc_id,
    finalize_locals_libfunc_id, get_concrete_libfunc_id, get_libfunc_signature, return_statement,
    revoke_ap_tracking_libfunc_id, simple_basic_statement,
};

#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct SierraFunctionWithBodyData<'db> {
    pub function: Maybe<pre_sierra::Function<'db>>,
    pub ap_change: SierraApChange,
}

/// Query implementation of [SierraGenGroup::priv_function_with_body_sierra_data].
#[salsa::tracked(returns(ref))]
pub fn priv_function_with_body_sierra_data<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<SierraFunctionWithBodyData<'db>> {
    let lowered_function = db.lowered_body(function_id, LoweringStage::Final)?;
    lowered_function.blocks.has_root()?;

    // Find the local variables.
    let analyze_ap_changes_result = analyze_ap_changes(db, lowered_function)?;

    let ap_change = match analyze_ap_changes_result.known_ap_change {
        true => SierraApChange::Known { new_vars_only: false },
        false => SierraApChange::Unknown,
    };

    let function = get_function_ap_change_and_code(
        db,
        function_id,
        lowered_function,
        analyze_ap_changes_result,
    );
    Ok(SierraFunctionWithBodyData { ap_change, function })
}

fn get_function_ap_change_and_code<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
    lowered_function: &Lowered<'db>,
    analyze_ap_change_result: AnalyzeApChangesResult,
) -> Maybe<pre_sierra::Function<'db>> {
    let root_block = lowered_function.blocks.root_block()?;
    let AnalyzeApChangesResult { known_ap_change, local_variables, ap_tracking_configuration } =
        analyze_ap_change_result;

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
                ty: db.get_concrete_type_id(lowered_function.variables[*param_id].ty)?.clone(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

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
    let result = generate_function_result(context)?;

    let statements = add_store_statements(
        db,
        result.statements,
        &|id: ConcreteLibfuncId| get_libfunc_signature(db, &id),
        sierra_local_variables,
        &parameters,
    );

    // TODO(spapini): Don't intern objects for the semantic model outside the crate. These should
    // be regarded as private.
    Ok(pre_sierra::Function {
        id: db.intern_sierra_function(function_id.function_id(db)?),
        body: statements,
        entry_point: label_id,
        parameters,
        variable_locations: result.variable_locations,
        signature_location: lowered_function.signature.location,
    })
}

/// Query implementation of [SierraGenGroup::priv_get_dummy_function].
#[salsa::tracked(returns(ref))]
pub fn priv_get_dummy_function<'db>(
    db: &'db dyn Database,
    function_id: ConcreteFunctionWithBodyId<'db>,
) -> Maybe<pre_sierra::Function<'db>> {
    // TODO(ilya): Remove the following query.
    let lowered_function = db.lowered_body(function_id, LoweringStage::PreOptimizations)?;
    let ap_tracking_configuration = Default::default();
    let lifetime = Default::default();

    let mut context = ExprGeneratorContext::new(
        db,
        lowered_function,
        function_id,
        &lifetime,
        ap_tracking_configuration,
    );

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();
    context.push_statement(label);

    let sierra_id = db.intern_sierra_function(function_id.function_id(db)?);
    let sierra_signature = db.get_function_signature(sierra_id.clone()).unwrap();

    let param_vars = (0..sierra_signature.param_types.len() as u64)
        .map(cairo_lang_sierra::ids::VarId::new)
        .collect_vec();

    let ret_vars = (0..sierra_signature.ret_types.len() as u64)
        .map(cairo_lang_sierra::ids::VarId::new)
        .collect_vec();

    // Generate Sierra variables for the function parameters.
    let parameters = zip_eq(&param_vars, &sierra_signature.param_types)
        .map(|(id, ty)| Ok(cairo_lang_sierra::program::Param { id: id.clone(), ty: ty.clone() }))
        .collect::<Result<Vec<_>, _>>()?;

    context.push_statement(simple_basic_statement(
        dummy_call_libfunc_id(db, sierra_id, sierra_signature),
        &param_vars[..],
        &ret_vars[..],
    ));

    context.push_statement(return_statement(ret_vars));

    Ok(pre_sierra::Function {
        id: db.intern_sierra_function(function_id.function_id(db)?),
        body: context.result().statements,
        entry_point: label_id,
        parameters,
        variable_locations: vec![],
        signature_location: lowered_function.signature.location,
    })
}

/// Allocates space for the local variables.
/// Returns:
/// * A map from a Sierra variable that should be stored as local variable to its allocated space
///   (uninitialized local variable).
/// * A list of Sierra statements.
fn allocate_local_variables<'db>(
    context: &mut ExprGeneratorContext<'db, '_>,
    local_variables: &OrderedHashSet<lowering::VariableId>,
) -> Maybe<LocalVariables> {
    let mut sierra_local_variables =
        OrderedHashMap::<cairo_lang_sierra::ids::VarId, cairo_lang_sierra::ids::VarId>::default();
    for lowering_var_id in local_variables {
        let sierra_var_id = context.get_sierra_variable(*lowering_var_id);
        let uninitialized_local_var_id =
            context.get_sierra_variable(SierraGenVar::UninitializedLocal(*lowering_var_id));
        context.push_statement(simple_basic_statement(
            alloc_local_libfunc_id(
                context.get_db(),
                context.get_variable_sierra_type(*lowering_var_id)?,
            ),
            &[],
            std::slice::from_ref(&uninitialized_local_var_id),
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
