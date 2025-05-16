use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::gas_coupon::GasCouponConcreteLibfunc;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    BuiltinInfo, CostValidationInfo, add_input_variables, get_non_fallthrough_statement_id,
};

/// Builds instructions for Sierra GasCoupon operations.
pub fn build(
    libfunc: &GasCouponConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        GasCouponConcreteLibfunc::Buy(_) => build_gas_coupon_buy(builder),
    }
}

/// Handles the gas_coupon_buy invocation.
fn build_gas_coupon_buy(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [range_check, gas_counter, amount] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(1) range_check;
        deref gas_counter;
        deref amount;
    };

    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar has_enough_gas;
        tempvar gas_minus_amount = gas_counter - amount;
        hint TestLessThanOrEqual {lhs: amount, rhs: gas_counter} into {dst: has_enough_gas};
        jump HasEnoughGas if has_enough_gas != 0;

        // Failure. Prove that gas_minus_amount < 0.
        const u128_bound = (BigInt::from(u128::MAX) + 1) as BigInt;
        tempvar tmp = gas_minus_amount + u128_bound;
        assert tmp = *(range_check++);
        jump Failure;

        HasEnoughGas:
        assert gas_minus_amount = *(range_check++);
    };

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[gas_minus_amount], &[amount]], None),
            ("Failure", &[&[range_check], &[gas_counter]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}
