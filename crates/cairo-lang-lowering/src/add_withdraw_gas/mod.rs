use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::{
    core_array_felt252_ty, core_felt252_ty, core_module, core_submodule, get_function_id,
    get_ty_by_name, option_none_variant, option_some_variant, unit_ty,
};
use cairo_lang_semantic::{GenericArgumentId, MatchArmSelector, TypeLongId};
use num_bigint::{BigInt, Sign};

use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId, SemanticFunctionIdEx};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementLiteral, StatementStructConstruct, VarUsage,
};

/// Main function for the add_withdraw_gas lowering phase. Adds a `withdraw_gas` statement to the
/// given function, if needed.
pub fn add_withdraw_gas(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    if db.needs_withdraw_gas(function)? {
        add_withdraw_gas_to_function(db, function, lowered)?;
    }

    Ok(())
}

/// Adds a `withdraw_gas_all` call statement to the given function.
/// Creates a new root block that matches on `withdraw_gas_all`, moves the old root block to the
/// success arm of it, and creates a new panic block for the failure arm.
fn add_withdraw_gas_to_function(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    let location = LocationId::from_stable_location(db, function.stable_location(db)?)
        .with_auto_generation_note(db, "withdraw_gas");
    let panic_block = create_panic_block(db, function, lowered, location)?;

    let old_root_block = lowered.blocks.root_block()?.clone();
    let old_root_new_id = lowered.blocks.push(old_root_block);
    let panic_block_id = lowered.blocks.push(panic_block);
    let gas_module = core_submodule(db.upcast(), "gas");

    // Add variable of type BuiltinCosts.
    let mut variables = VariableAllocator::new(
        db,
        function.function_with_body_id(db).base_semantic_function(db),
        lowered.variables.clone(),
    )?;

    let builtin_costs_var = variables.new_var(VarRequest {
        ty: get_ty_by_name(db.upcast(), gas_module, "BuiltinCosts".into(), Vec::new()),
        location,
    });
    lowered.variables = variables.variables;

    let new_root_block = FlatBlock {
        statements: vec![
            // A statement call to `get_builtin_costs`.
            Statement::Call(StatementCall {
                function: get_function_id(
                    db.upcast(),
                    gas_module,
                    "get_builtin_costs".into(),
                    vec![],
                )
                .lowered(db),
                inputs: vec![],
                outputs: vec![builtin_costs_var],
                location,
            }),
        ],
        end: FlatBlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: get_function_id(
                    db.upcast(),
                    gas_module,
                    "withdraw_gas_all".into(),
                    vec![],
                )
                .lowered(db),
                inputs: vec![VarUsage { var_id: builtin_costs_var, location }],
                arms: vec![
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_some_variant(
                            db.upcast(),
                            GenericArgumentId::Type(unit_ty(db.upcast())),
                        )),
                        block_id: old_root_new_id,
                        var_ids: vec![],
                    },
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_none_variant(
                            db.upcast(),
                            GenericArgumentId::Type(unit_ty(db.upcast())),
                        )),
                        block_id: panic_block_id,
                        var_ids: vec![],
                    },
                ],
                location,
            }),
        },
    };

    lowered.blocks.reset_block(BlockId(0), new_root_block);

    Ok(())
}

/// Creates the panic block for the case `withdraw_gas` failure.
fn create_panic_block(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
    location: LocationId,
) -> Maybe<FlatBlock> {
    let mut variables = VariableAllocator::new(
        db,
        function.function_with_body_id(db).base_semantic_function(db),
        lowered.variables.clone(),
    )?;
    let new_array_var =
        variables.new_var(VarRequest { ty: core_array_felt252_ty(db.upcast()), location });
    let out_of_gas_err_var =
        variables.new_var(VarRequest { ty: core_felt252_ty(db.upcast()), location });
    let panic_instance_var = variables.new_var(VarRequest {
        ty: get_ty_by_name(db.upcast(), core_module(db.upcast()), "Panic".into(), vec![]),
        location,
    });
    let panic_data_var =
        variables.new_var(VarRequest { ty: core_array_felt252_ty(db.upcast()), location });
    let err_data_var = variables.new_var(VarRequest {
        ty: db.intern_type(TypeLongId::Tuple(vec![
            variables[panic_instance_var].ty,
            variables[panic_data_var].ty,
        ])),
        location,
    });
    lowered.variables = variables.variables;

    let array_module = core_submodule(db.upcast(), "array");

    let add_location = |var_id| VarUsage { var_id, location };

    // The block consists of creating a new array, appending 'Out of gas' to it and panic with this
    // array as panic data.
    Ok(FlatBlock {
        statements: vec![
            Statement::Call(StatementCall {
                function: get_function_id(
                    db.upcast(),
                    array_module,
                    "array_new".into(),
                    vec![GenericArgumentId::Type(core_felt252_ty(db.upcast()))],
                )
                .lowered(db),
                inputs: vec![],
                outputs: vec![new_array_var],
                location,
            }),
            Statement::Literal(StatementLiteral {
                value: BigInt::from_bytes_be(Sign::Plus, "Out of gas".as_bytes()),
                output: out_of_gas_err_var,
            }),
            Statement::Call(StatementCall {
                function: get_function_id(
                    db.upcast(),
                    array_module,
                    "array_append".into(),
                    vec![GenericArgumentId::Type(core_felt252_ty(db.upcast()))],
                )
                .lowered(db),
                inputs: vec![new_array_var, out_of_gas_err_var]
                    .into_iter()
                    .map(add_location)
                    .collect(),
                outputs: vec![panic_data_var],
                location,
            }),
            Statement::StructConstruct(StatementStructConstruct {
                inputs: vec![],
                output: panic_instance_var,
            }),
            Statement::StructConstruct(StatementStructConstruct {
                inputs: vec![panic_instance_var, panic_data_var]
                    .into_iter()
                    .map(add_location)
                    .collect(),
                output: err_data_var,
            }),
        ],
        end: FlatBlockEnd::Panic(VarUsage { var_id: err_data_var, location }),
    })
}
