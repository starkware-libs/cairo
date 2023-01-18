#[cfg(test)]
#[path = "function_generator_test.rs"]
mod test;

use std::sync::Arc;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::ids::ConcreteLibfuncId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use {cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::block_generator::{generate_block_code, generate_return_code};
use crate::db::SierraGenGroup;
use crate::expr_generator_context::ExprGeneratorContext;
use crate::lifetime::{find_variable_lifetime, SierraGenVar, StatementLocation};
use crate::local_variables::find_local_variables;
use crate::pre_sierra::{self, Statement};
use crate::store_variables::{add_store_statements, LibfuncInfo, LocalVariables};
use crate::utils::{
    alloc_local_libfunc_id, finalize_locals_libfunc_id, get_libfunc_signature, simple_statement,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SierraFreeFunctionData {
    pub function: Maybe<Arc<pre_sierra::Function>>,
}

/// Query implementation of [SierraGenGroup::priv_function_with_body_sierra_data].
pub fn priv_function_with_body_sierra_data(
    db: &dyn SierraGenGroup,
    function_id: ConcreteFunctionWithBodyId,
) -> SierraFreeFunctionData {
    let function = get_function_code(db, function_id);
    SierraFreeFunctionData { function }
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
    let signature = db.concrete_function_signature(function_id.function_id(db.upcast()))?;
    let lowered_function = &*db.concrete_function_with_body_lowered(function_id)?;
    let block_id = lowered_function.root?;
    let block = &lowered_function.blocks[block_id];

    // Find the local variables.
    let local_variables = find_local_variables(db, lowered_function)?;

    // Get lifetime information.
    let lifetime = find_variable_lifetime(lowered_function, &local_variables)?;

    let mut context = ExprGeneratorContext::new(db, lowered_function, function_id, &lifetime);

    // Generate a label for the function's body.
    let (label, label_id) = context.new_label();

    // Generate Sierra variables for the function parameters.
    let mut parameters: Vec<cairo_lang_sierra::program::Param> = Vec::new();
    for param_id in &block.inputs {
        let var = &lowered_function.variables[*param_id];

        parameters.push(cairo_lang_sierra::program::Param {
            id: context.get_sierra_variable(*param_id),
            ty: db.get_concrete_type_id(var.ty)?,
        })
    }

    let ret_types = vec![db.get_concrete_type_id(signature.return_type)?];

    let mut statements: Vec<pre_sierra::Statement> = vec![label];

    let (sierra_local_variables, allocate_local_statements) =
        allocate_local_variables(&mut context, &local_variables)?;
    statements.extend(allocate_local_statements);

    let prolog_size = statements.len();
    // Generate the function's body.
    let body_statements = generate_block_code(&mut context, block_id, block)?;
    statements.extend(body_statements);

    // Generate the return statement if necessary.
    let return_statement_location: StatementLocation = (block_id, block.statements.len());
    match &block.end {
        lowering::FlatBlockEnd::Callsite(_) => panic!("Root block may not end with callsite."),
        lowering::FlatBlockEnd::Return(returned_variables) => {
            statements.extend(generate_return_code(
                &mut context,
                returned_variables,
                &return_statement_location,
            )?);
        }
        lowering::FlatBlockEnd::Unreachable => {}
    };

    let statements = add_store_statements(
        context.get_db(),
        statements,
        &|concrete_lib_func_id: ConcreteLibfuncId| -> LibfuncInfo {
            LibfuncInfo { signature: get_libfunc_signature(context.get_db(), concrete_lib_func_id) }
        },
        sierra_local_variables,
    );

    // TODO(spapini): Don't intern objects for the semantic model outside the crate. These should
    // be regarded as private.
    Ok(pre_sierra::Function {
        id: db.intern_sierra_function(db.intern_function(semantic::FunctionLongId {
            function: function_id.concrete(db.upcast()),
        })),
        prolog_size,
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
) -> Maybe<(LocalVariables, Vec<Statement>)> {
    let mut statements: Vec<pre_sierra::Statement> = vec![];
    let mut sierra_local_variables =
        OrderedHashMap::<cairo_lang_sierra::ids::VarId, cairo_lang_sierra::ids::VarId>::default();
    for lowering_var_id in local_variables.iter() {
        let sierra_var_id = context.get_sierra_variable(*lowering_var_id);
        let uninitialized_local_var_id =
            context.get_sierra_variable(SierraGenVar::UninitializedLocal(*lowering_var_id));
        statements.push(simple_statement(
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
        statements.push(simple_statement(finalize_locals_libfunc_id(context.get_db()), &[], &[]));
    }

    Ok((sierra_local_variables, statements))
}
