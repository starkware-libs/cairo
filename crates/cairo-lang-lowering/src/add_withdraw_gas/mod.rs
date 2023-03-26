use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::{
    core_array_felt252_ty, core_felt252_ty, core_submodule, get_function_id, option_none_variant,
    option_some_variant, unit_ty,
};
use cairo_lang_semantic::{ConcreteFunctionWithBodyId, GenericArgumentId};
use num_bigint::{BigInt, Sign};

use crate::db::LoweringGroup;
use crate::lower::context::{LoweringContextBuilder, VarRequest};
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementLiteral,
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

/// Adds a `withdraw_gas` statement to the given function.
/// Creates a new root block that matches on `withdraw_gas`, moves the old root block to the success
/// arm of it, and creates a new panic block for the failure arm.
fn add_withdraw_gas_to_function(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    let panic_block = create_panic_block(db, function, lowered)?;

    let old_root_block = lowered.blocks.root_block()?.clone();
    let old_root_new_id = lowered.blocks.push(old_root_block);
    let panic_block_id = lowered.blocks.push(panic_block);

    let new_root_block = FlatBlock {
        statements: vec![],
        end: FlatBlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: get_function_id(
                    db.upcast(),
                    core_submodule(db.upcast(), "gas"),
                    "withdraw_gas".into(),
                    vec![],
                ),
                inputs: vec![],
                arms: vec![
                    MatchArm {
                        variant_id: option_some_variant(
                            db.upcast(),
                            GenericArgumentId::Type(unit_ty(db.upcast())),
                        ),
                        block_id: old_root_new_id,
                        var_ids: vec![],
                    },
                    MatchArm {
                        variant_id: option_none_variant(
                            db.upcast(),
                            GenericArgumentId::Type(unit_ty(db.upcast())),
                        ),
                        block_id: panic_block_id,
                        var_ids: vec![],
                    },
                ],
                location: StableLocationOption::None,
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
) -> Maybe<FlatBlock> {
    let lowering_builder =
        LoweringContextBuilder::new(db, function.function_with_body_id(db.upcast()))?;
    let lowering_context = lowering_builder.ctx()?;

    // Here we use `create_new_var` directly (and not `new_var`) as the var should be added to
    // `lowered.variables`, and not to the context's arena.
    let new_array_var = lowered.variables.alloc(lowering_context.create_new_var(VarRequest {
        ty: core_array_felt252_ty(db.upcast()),
        location: StableLocationOption::None,
    }));
    let out_of_gas_err_var = lowered.variables.alloc(lowering_context.create_new_var(VarRequest {
        ty: core_felt252_ty(db.upcast()),
        location: StableLocationOption::None,
    }));
    let panic_data_var = lowered.variables.alloc(lowering_context.create_new_var(VarRequest {
        ty: core_array_felt252_ty(db.upcast()),
        location: StableLocationOption::None,
    }));

    // The block consists of creating a new array, appending 'Out of gas' to it and panic with this
    // array as panic data.
    Ok(FlatBlock {
        statements: vec![
            Statement::Call(StatementCall {
                function: get_function_id(
                    db.upcast(),
                    core_submodule(db.upcast(), "array"),
                    "array_new".into(),
                    vec![GenericArgumentId::Type(core_felt252_ty(db.upcast()))],
                ),
                inputs: vec![],
                outputs: vec![new_array_var],
                location: StableLocationOption::None,
            }),
            Statement::Literal(StatementLiteral {
                value: BigInt::from_bytes_be(Sign::Plus, "Out of gas".as_bytes()),
                output: out_of_gas_err_var,
            }),
            Statement::Call(StatementCall {
                function: get_function_id(
                    db.upcast(),
                    core_submodule(db.upcast(), "array"),
                    "array_append".into(),
                    vec![GenericArgumentId::Type(core_felt252_ty(db.upcast()))],
                ),
                inputs: vec![new_array_var, out_of_gas_err_var],
                outputs: vec![panic_data_var],
                location: StableLocationOption::None,
            }),
        ],
        end: FlatBlockEnd::Panic(panic_data_var),
    })
}
