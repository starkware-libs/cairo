use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};
use crate::references::ReferenceExpression;

/// Handles the storage_base_address_const libfunc.
pub fn build_storage_base_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = (BigInt::from(1) << 251) - 256;
    if libfunc.c >= addr_bound {
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
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[res]], None)]))
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
    // For both checks later:
    // We show that a number is in the range [0, bound) by writing it as:
    //   A * x + y,
    // where:
    //   * K = low positive number (the lower the better, here it is 1 or 2).
    //   * max_x = 2**128 - K.
    //   * A = bound // max_x.
    //   * B = bound % max_x.
    //   * x is in the range [0, max_x],
    //   * y is in the range [0, B):
    //     * y is in the range [0, 2**128).
    //     * y + 2**128 - B is in the range [0, 2**128).
    //
    // Note that the minimal possible value of the expression A * x + y is min_val = 0 (where x = y
    // = 0), and the maximal value is obtained where x = max_x and y = B - 1:
    //   max_val = (A * max_x + B) - 1 = bound - 1.
    //
    // As long as A <= B, every number in the range can be represented.
    // In the second case, we choose K to be 2 in order to find A <= B.
    casm_build_extend! {casm_builder,
        const addr_bound = addr_bound;
        const u128_limit_minus_1 = u128::MAX;
        // Allocating all vars in the beginning for easier AP-Alignment between the two branches,
        // as well as making sure we use `res` as the last cell, making it the last on stack.
        tempvar is_small;
        tempvar x;
        tempvar y;
        tempvar x_part;
        tempvar y_fixed;
        tempvar diff;
        tempvar res;
        hint TestLessThan {lhs: addr, rhs: addr_bound} into {dst: is_small};
        jump IsSmall if is_small != 0;
        assert res = addr - addr_bound;
        // Here we want to make sure that `addr` > ADDR_BOUND and `res` < ADDR_BOUND,
        // for that it is enough to show that `res` < PRIME - ADDR_BOUND.
        // We use the method described above with (A, B) = divmod(PRIME - ADDR_BOUND, 2**128 - 1)
        const a_imm = 0x110000000000000000_u128;
        // 2**128 - B.
        const b_imm_fix = (u128::MAX - 0x110000000000000101u128 + 1);
        hint LinearSplit {value: res, scalar: a_imm, max_x: u128_limit_minus_1} into {x: x, y: y};
        assert x_part = x * a_imm;
        assert res = x_part + y;
        // x < 2**128
        assert x = *(range_check++);
        // y < 2**128
        assert y = *(range_check++);
        // y + 2**128 - B < 2**128 ==> y < B
        assert y_fixed = y + b_imm_fix;
        assert y_fixed = *(range_check++);
        jump Done;
        IsSmall:
        assert res = addr;
        // We now want to make sure `res` is less than ADDR_BOUND.
        // We use the method described above with (A, B) = divmod(ADDR_BOUND, 2**128 - 2)
        const a_imm = 0x8000000000000000000000000000000_u128;
        // 2**128 - B.
        const b_imm_fix = (u128::MAX - 0xfffffffffffffffffffffffffffff00_u128 + 1);
        const u128_limit_minus_2 = u128::MAX - 1;
        hint LinearSplit {value: res, scalar: a_imm, max_x: u128_limit_minus_2} into {x: x, y: y};
        assert x_part = x * a_imm;
        assert res = x_part + y;
        // y < 2**128
        assert y = *(range_check++);
        // y + 2**128 - B < 2**128 ==> y < B
        assert y_fixed = y + b_imm_fix;
        assert y_fixed = *(range_check++);
        // x < 2**128 && x != 2**128 - 1 ==> x < 2**128 - 1
        assert x = *(range_check++);
        assert diff = x - u128_limit_minus_1;
        jump Done if diff != 0;
        InfiniteLoop:
        jump InfiniteLoop;
        Done:
    };
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[range_check], &[res]], None)]))
}

/// Builds instructions for StarkNet read system call.
pub fn build_storage_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "StorageRead".as_bytes());

    let [gas_builtin, system, address_domain, storage_address] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref gas_builtin;
        buffer(3) system;
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
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        let updated_gas_builtin = *(system++);
        tempvar revert_reason = *(system++);
        let read_value = *(system++);
        jump Failure if revert_reason != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[system], &[read_value]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[revert_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
    ))
}

/// Builds instructions for StarkNet write system call.
pub fn build_storage_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, "StorageWrite".as_bytes());

    let [gas_builtin, system, address_domain, storage_address, value] =
        builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(6) system;
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
        // `revert_reason` is 0 on success, nonzero on failure/revert.
        tempvar revert_reason = *(system++);
        jump Failure if revert_reason != 0;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[updated_gas_builtin], &[system]], None),
            (
                "Failure",
                &[&[updated_gas_builtin], &[system], &[revert_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
    ))
}
