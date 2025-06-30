use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic::corelib::{
    core_module, core_submodule, get_function_id, never_ty, option_none_variant,
    option_some_variant, unit_ty,
};
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::{ConcreteTypeId, GenericArgumentId, MatchArmSelector, TypeLongId};
use cairo_lang_utils::{Intern, LookupIntern, extract_matches};
use num_bigint::{BigInt, Sign};

use crate::db::LoweringGroup;
use crate::ids::{ConcreteFunctionWithBodyId, LocationId, SemanticFunctionIdEx};
use crate::lower::context::{VarRequest, VariableAllocator};
use crate::{
    Block, BlockEnd, BlockId, Lowered, MatchArm, MatchEnumInfo, MatchExternInfo, MatchInfo,
    Statement, StatementCall, VarUsage,
};

/// Main function for the add_withdraw_gas lowering phase. Adds a `withdraw_gas` statement to the
/// given function, if needed.
pub fn add_withdraw_gas(
    db: &dyn LoweringGroup,
    function: ConcreteFunctionWithBodyId,
    lowered: &mut Lowered,
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
    lowered: &mut Lowered,
) -> Maybe<()> {
    let location = LocationId::from_stable_location(db, function.stable_location(db)?)
        .with_auto_generation_note(db, "withdraw_gas");
    let panic_block = create_panic_block(db, function, lowered, location)?;

    let old_root_block = lowered.blocks.root_block()?.clone();
    let old_root_new_id = lowered.blocks.push(old_root_block);
    let panic_block_id = lowered.blocks.push(panic_block);
    let new_root_block = Block {
        statements: vec![],
        end: BlockEnd::Match {
            info: MatchInfo::Extern(MatchExternInfo {
                function: get_function_id(
                    db,
                    core_submodule(db, "gas"),
                    "withdraw_gas".into(),
                    vec![],
                )
                .lowered(db),
                inputs: vec![],
                arms: vec![
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_some_variant(
                            db,
                            unit_ty(db),
                        )),
                        block_id: old_root_new_id,
                        var_ids: vec![],
                    },
                    MatchArm {
                        arm_selector: MatchArmSelector::VariantId(option_none_variant(
                            db,
                            unit_ty(db),
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
    lowered: &mut Lowered,
    location: LocationId,
) -> Maybe<Block> {
    let mut variables = VariableAllocator::new(
        db,
        function.base_semantic_function(db).function_with_body_id(db),
        std::mem::take(&mut lowered.variables),
    )
    .unwrap();
    let never_ty = never_ty(db);
    let never_var = variables.new_var(VarRequest { ty: never_ty, location });
    lowered.variables = variables.variables;

    let gas_panic_fn = get_function_id(
        db,
        core_module(db),
        "panic_with_const_felt252".into(),
        vec![GenericArgumentId::Constant(
            ConstValue::Int(
                BigInt::from_bytes_be(Sign::Plus, "Out of gas".as_bytes()),
                db.core_info().felt252,
            )
            .intern(db),
        )],
    )
    .lowered(db);

    let never_enum_id = extract_matches!(
        extract_matches!(never_ty.lookup_intern(db), TypeLongId::Concrete),
        ConcreteTypeId::Enum
    );

    // The block consists of calling  panic_with_const_felt252::<'Out of gas'> and matching on its
    // `never` result.
    Ok(Block {
        statements: vec![Statement::Call(StatementCall {
            function: gas_panic_fn,
            inputs: vec![],
            with_coupon: false,
            outputs: vec![never_var],
            location,
        })],
        end: BlockEnd::Match {
            info: MatchInfo::Enum(MatchEnumInfo {
                concrete_enum_id: never_enum_id,
                input: VarUsage { var_id: never_var, location },
                arms: vec![],
                location,
            }),
        },
    })
}
