use cairo_lang_defs::diagnostic_utils::StableLocationOption;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::{
    core_array_felt252_ty, core_felt252_ty, get_core_function_id, option_none_variant,
    option_some_variant, unit_ty,
};
use cairo_lang_semantic::{ConcreteFunctionWithBodyId, GenericArgumentId};
use num_bigint::{BigInt, Sign};

use crate::db::LoweringGroup;
use crate::{
    BlockId, FlatBlock, FlatBlockEnd, FlatLowered, MatchArm, MatchExternInfo, MatchInfo, Statement,
    StatementCall, StatementLiteral, Variable,
};

/// Main function for the add_withdraw_gas lowering phase. Adds a `withdraw_gas` statement to the
/// given function, if needed.
pub fn add_withdraw_gas(
    db: &dyn LoweringGroup,
    root_function_id: ConcreteFunctionWithBodyId,
    lowered: &mut FlatLowered,
) -> Maybe<()> {
    if db.needs_withdraw_gas(root_function_id)? {
        add_withdraw_gas_to_function(db, lowered)?;
    }

    Ok(())
}

/// Adds a `withdraw_gas` statement to the given function.
/// Creates a new root block that matches on `withdraw_gas`, moves the old root block to the success
/// arm of it, and creates a new panic block for the failure arm.
fn add_withdraw_gas_to_function(db: &dyn LoweringGroup, lowered: &mut FlatLowered) -> Maybe<()> {
    let panic_block = create_panic_block(db, lowered);

    let old_root_block = lowered.blocks.root_block()?.clone();
    let old_root_new_id = lowered.blocks.push(old_root_block);
    let panic_block_id = lowered.blocks.push(panic_block);

    let new_root_block = FlatBlock {
        statements: vec![],
        end: FlatBlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: get_core_function_id(db.upcast(), "withdraw_gas".into(), vec![]),
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
fn create_panic_block(db: &dyn LoweringGroup, lowered: &mut FlatLowered) -> FlatBlock {
    let new_array_var = lowered.variables.alloc(Variable {
        duplicatable: Ok(()),
        droppable: Ok(()),
        ty: core_array_felt252_ty(db.upcast()),
        location: StableLocationOption::None,
    });
    let out_of_gas_err_var = lowered.variables.alloc(Variable {
        duplicatable: Ok(()),
        droppable: Ok(()),
        ty: core_felt252_ty(db.upcast()),
        location: StableLocationOption::None,
    });
    let panic_data_var = lowered.variables.alloc(Variable {
        duplicatable: Ok(()),
        droppable: Ok(()),
        ty: core_array_felt252_ty(db.upcast()),
        location: StableLocationOption::None,
    });

    // The block consists of creating a new array, appending 'Out of gas' to it and panic with this
    // array as panic data.
    FlatBlock {
        statements: vec![
            Statement::Call(StatementCall {
                function: get_core_function_id(
                    db.upcast(),
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
                function: get_core_function_id(
                    db.upcast(),
                    "array_append".into(),
                    vec![GenericArgumentId::Type(core_felt252_ty(db.upcast()))],
                ),
                inputs: vec![new_array_var, out_of_gas_err_var],
                outputs: vec![panic_data_var],
                location: StableLocationOption::None,
            }),
        ],
        end: FlatBlockEnd::Panic(panic_data_var),
    }
}
