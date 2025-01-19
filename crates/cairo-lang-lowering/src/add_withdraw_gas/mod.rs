use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::{
    core_module, get_function_id, get_ty_by_name, option_none_variant, option_some_variant,
};
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{GenericArgumentId, MatchArmSelector, TypeLongId};
use cairo_lang_utils::Intern;
use num_bigint::{BigInt, Sign};

use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId, SemanticFunctionIdEx};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementConst, StatementStructConstruct, VarUsage,
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

/// Adds a `withdraw_gas` call statement to the given function.
/// Creates a new root block that matches on `withdraw_gas`, moves the old root block to the success
/// arm of it, and creates a new panic block for the failure arm.
fn add_withdraw_gas_to_function(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    let location = LocationId::from_stable_location(db, function.stable_location(db)?)
        .with_auto_generation_note(db, "withdraw_gas");
    let panic_block = create_panic_block(db, function, lowered, location)?;
    let info = db.defs_info();

    let old_root_block = lowered.blocks.root_block()?.clone();
    let old_root_new_id = lowered.blocks.push(old_root_block);
    let panic_block_id = lowered.blocks.push(panic_block);
    let new_root_block = FlatBlock {
        statements: vec![],
        end: FlatBlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: info.withdraw_gas_fn.lowered(db),
                inputs: vec![],
                arms: vec![
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_some_variant(
                            db.upcast(),
                            info.unit_ty,
                        )),
                        block_id: old_root_new_id,
                        var_ids: vec![],
                    },
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_none_variant(
                            db.upcast(),
                            info.unit_ty,
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
    let info = db.defs_info();
    let new_array_var = variables.new_var(VarRequest { ty: info.array_felt252_ty, location });
    let out_of_gas_err_var = variables.new_var(VarRequest { ty: info.felt252_ty, location });
    let panic_instance_var = variables.new_var(VarRequest {
        ty: get_ty_by_name(db.upcast(), core_module(db.upcast()), "Panic".into(), vec![]),
        location,
    });
    let panic_data_var = variables.new_var(VarRequest { ty: info.array_felt252_ty, location });
    let err_data_var = variables.new_var(VarRequest {
        ty: TypeLongId::Tuple(vec![variables[panic_instance_var].ty, variables[panic_data_var].ty])
            .intern(db),
        location,
    });
    lowered.variables = variables.variables;

    let add_location = |var_id| VarUsage { var_id, location };

    // The block consists of creating a new array, appending 'Out of gas' to it and panic with this
    // array as panic data.
    Ok(FlatBlock {
        statements: vec![
            Statement::Call(StatementCall {
                function: get_function_id(db.upcast(), info.array_mod, "array_new".into(), vec![
                    GenericArgumentId::Type(info.felt252_ty),
                ])
                .lowered(db),
                inputs: vec![],
                with_coupon: false,
                outputs: vec![new_array_var],
                location,
            }),
            Statement::Const(StatementConst {
                value: ConstValue::Int(
                    BigInt::from_bytes_be(Sign::Plus, "Out of gas".as_bytes()),
                    info.felt252_ty,
                ),
                output: out_of_gas_err_var,
            }),
            Statement::Call(StatementCall {
                function: get_function_id(
                    db.upcast(),
                    info.array_mod,
                    "array_append".into(),
                    vec![GenericArgumentId::Type(info.felt252_ty)],
                )
                .lowered(db),
                inputs: vec![new_array_var, out_of_gas_err_var]
                    .into_iter()
                    .map(add_location)
                    .collect(),
                with_coupon: false,
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
