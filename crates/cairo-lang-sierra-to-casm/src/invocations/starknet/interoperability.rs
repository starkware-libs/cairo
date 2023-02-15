use cairo_felt::Felt;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use cairo_lang_sierra_gas::core_libfunc_cost::SYSTEM_CALL_COST;
use num_bigint::{BigInt, ToBigInt};
use num_traits::Signed;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};
use crate::references::ReferenceExpression;

/// Builds instructions for Starknet call contract system call.
pub fn build_call_contract(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "CallContract".as_bytes());

    let [expr_gas_builtin, expr_system, expr_address, expr_arr] = builder.try_get_refs()?;
    let gas_builtin = expr_gas_builtin.try_unpack_single()?;
    let system = expr_system.try_unpack_single()?;
    let contract_address = expr_address.try_unpack_single()?;
    let [call_data_start, call_data_end] = expr_arr.try_unpack()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(8) system;
        deref gas_builtin;
        deref contract_address;
        deref call_data_start;
        deref call_data_end;
    };
    casm_build_extend! {casm_builder,
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        let original_system = system;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert contract_address = *(system++);
        assert call_data_start = *(system++);
        assert call_data_end = *(system++);
        hint SystemCall { system: original_system };

        let updated_gas_builtin = *(system++);
        // `failure_flag` is 0 on success, nonzero on failure/revert.
        tempvar failure_flag = *(system++);
        let res_start = *(system++);
        let res_end = *(system++);
        jump Failure if failure_flag != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[system], &[res_start, res_end]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[res_start, res_end]],
                Some(failure_handle_statement_id),
            ),
        ],
        CostValidationInfo {
            range_check_info: None,
            extra_costs: Some([SYSTEM_CALL_COST, SYSTEM_CALL_COST]),
        },
    ))
}

/// Handles the contract_address_const libfunc.
pub fn build_contract_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = BigInt::from(1) << 251;
    if libfunc.c.is_negative() || libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))].into_iter(),
    ))
}

/// Handles the contract_address_try_from_felt libfunc.
pub fn build_contract_address_try_from_felt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound: BigInt = BigInt::from(1) << 251;
    let [range_check, value] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref value;
    };
    let auxiliary_vars: [_; 4] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    casm_build_extend! {casm_builder,
        const limit = addr_bound.clone();
        let orig_range_check = range_check;
        tempvar is_valid_address;
        hint TestLessThan {lhs: value, rhs: limit} into {dst: is_valid_address};
        jump IsValidAddress if is_valid_address != 0;
        tempvar shifted_value = value - limit;
    }
    validate_under_limit::<1>(
        &mut casm_builder,
        &(-Felt::from(addr_bound.clone())).to_biguint().to_bigint().unwrap(),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend! {casm_builder,
        jump Failure;
        IsValidAddress:
    };
    validate_under_limit::<1>(&mut casm_builder, &addr_bound, value, range_check, &auxiliary_vars);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Failure", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}
