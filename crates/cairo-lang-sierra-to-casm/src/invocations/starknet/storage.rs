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

/// Handles the storage_base_address_const libfunc.
pub fn build_storage_base_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = (BigInt::from(1) << 251) - 256;
    if libfunc.c.is_negative() || libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }

    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.clone()))].into_iter(),
    ))
}

/// Handles the storage_address_from_base_and_offset libfunc.
pub fn build_storage_address_from_base_and_offset(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [base, offset] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref base;
        deref_or_immediate offset;
    };
    casm_build_extend!(casm_builder, let res = base + offset;);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[res]], None)],
        Default::default(),
    ))
}

/// Handles the storage_base_address_const libfunc.
pub fn build_storage_base_address_from_felt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound: BigInt = (BigInt::from(1) << 251) - 256;
    let [range_check, addr] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref addr;
    };
    let auxiliary_vars: [_; 5] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    casm_build_extend! {casm_builder,
        const limit = addr_bound.clone();
        let orig_range_check = range_check;
        // Allocating all vars in the beginning for easier AP-Alignment between the two branches,
        // as well as making sure we use `res` as the last cell, making it the last on stack.
        tempvar is_small;
        tempvar res;
        hint TestLessThan {lhs: addr, rhs: limit} into {dst: is_small};
        jump IsSmall if is_small != 0;
        assert res = addr - limit;
    }
    validate_under_limit::<1>(
        &mut casm_builder,
        &(-Felt::from(addr_bound.clone())).to_biguint().to_bigint().unwrap(),
        res,
        range_check,
        &auxiliary_vars[..4],
    );
    casm_build_extend! {casm_builder,
        jump Done;
        IsSmall:
        assert res = addr;
    }
    validate_under_limit::<2>(&mut casm_builder, &addr_bound, res, range_check, &auxiliary_vars);
    casm_build_extend! {casm_builder,
        Done:
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[range_check], &[res]], None)],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
}

/// Builds instructions for Starknet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "StorageRead".as_bytes());

    let [gas_builtin, system, address_domain, storage_address] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref gas_builtin;
        buffer(7) system;
        deref address_domain;
        deref storage_address;
    };

    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert address_domain = *(system++);
        assert storage_address = *(system++);
        hint SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        tempvar failure_flag = *(system++);
        let response_0 = *(system++);

        // The response in the success case is smaller than in the failure case.
        let success_final_system = system;
        let response_1 = *(system++);
        jump Failure if failure_flag != 0;

    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            (
                "Fallthrough",
                &[&[updated_gas_builtin], &[success_final_system], &[response_0]],
                None,
            ),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[response_0, response_1]],
                Some(failure_handle_statement_id),
            ),
        ],
        CostValidationInfo {
            range_check_info: None,
            extra_costs: Some([SYSTEM_CALL_COST, SYSTEM_CALL_COST]),
        },
    ))
}

/// Builds instructions for Starknet write system call.
pub fn build_storage_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "StorageWrite".as_bytes());

    let [gas_builtin, system, address_domain, storage_address, value] =
        builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(8) system;
        deref gas_builtin;
        deref address_domain;
        deref storage_address;
        deref value;
    };
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
        assert address_domain = *(system++);
        assert storage_address = *(system++);
        assert value = *(system++);
        hint SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        tempvar failure_flag = *(system++);
        // The response in the success case is smaller than in the failure case.
        let success_final_system = system;
        let revert_reason_start = *(system++);
        let revert_reason_end = *(system++);
        jump Failure if failure_flag != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[success_final_system]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[revert_reason_start, revert_reason_end]],
                Some(failure_handle_statement_id),
            ),
        ],
        CostValidationInfo {
            range_check_info: None,
            extra_costs: Some([SYSTEM_CALL_COST, SYSTEM_CALL_COST]),
        },
    ))
}
