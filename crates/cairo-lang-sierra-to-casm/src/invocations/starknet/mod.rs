use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::operand::ResOperand;
use cairo_lang_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;
use num_bigint::BigInt;

use self::interoperability::{build_call_contract, build_contract_address_const};
use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::InvocationError;
use crate::references::{CellExpression, ReferenceExpression};

mod storage;
use storage::{build_storage_read, build_storage_write};

mod interoperability;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &StarkNetConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarkNetConcreteLibfunc::CallContract(_) => build_call_contract(builder),
        StarkNetConcreteLibfunc::ContractAddressConst(libfunc) => {
            build_contract_address_const(builder, libfunc)
        }
        StarkNetConcreteLibfunc::StorageRead(_) => build_storage_read(builder),
        StarkNetConcreteLibfunc::StorageWrite(_) => build_storage_write(builder),
        StarkNetConcreteLibfunc::StorageAddressConst(libfunc) => {
            build_storage_address_const(builder, libfunc)
        }
        StarkNetConcreteLibfunc::StorageAddressFromFelt(_) => {
            build_storage_address_from_felt(builder)
        }
    }
}

/// Handles the storage_address_const libfunc.
fn build_storage_address_const(
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

/// Handles the storage_address_const libfunc.
fn build_storage_address_from_felt(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound: BigInt = (BigInt::from(1) << 251) - 256;
    let [range_check_expr, addr_expr] = builder.try_get_refs()?;
    let range_check = range_check_expr.try_unpack_single()?.to_buffer(2)?;
    let addr = addr_expr.try_unpack_single()?.to_deref()?;
    let mut casm_builder = CasmBuilder::default();
    let range_check = casm_builder.add_var(range_check);
    let addr = casm_builder.add_var(ResOperand::Deref(addr));
    // For both checks later:
    // We show that a number is in the range [0, bound) by writing it as:
    //   A * x + y,
    // where:
    //   * K = low positive number (the lower the better, here it is 1 or 2).
    //   * x_bound = 2**128 - K.
    //   * A = bound // x_bound.
    //   * B = bound % x_bound.
    //   * x is in the range [0, x_bound],
    //   * y is in the range [0, B):
    //     * y is in the range [0, 2**128).
    //     * y + 2**128 - B is in the range [0, 2**128).
    //
    // Note that the minimal possible value of the expression A * x + y is min_val = 0 (where x = y
    // = 0), and the maximal value is obtained where x = x_bound and y = B - 1:
    //   max_val = (A * x_bound + B) - 1 = bound - 1.
    //
    // As long as A <= B, every number in the range can be represented.
    // In the second case, we choose K to be 2 in order to find A <= B.
    casm_build_extend! {casm_builder,
        const addr_bound = addr_bound;
        const u128_limit_minus_1 = u128::MAX;
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
        // We now want to make sure `res` is less than PRIME - ADDR_BOUND.
        // (A, B) = divmod(PRIME - ADDR_BOUND, 2**128 - 1)
        const a_imm = 313594649253062377472u128;
        const b_imm_fix = (u128::MAX - 313594649253062377729u128 + 1);
        hint LinearSplit {value: res, scalar: a_imm, x_bound: u128_limit_minus_1} into {x: x, y: y};
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
        // (A, B) = divmod(ADDR_BOUND, 2**128 - 2)
        const a_imm = 10633823966279326983230456482242756608u128;
        const b_imm_fix = (u128::MAX - 21267647932558653966460912964485512960u128 + 1);
        const u128_limit_minus_2 = u128::MAX - 1;
        hint LinearSplit {value: res, scalar: a_imm, x_bound: u128_limit_minus_2} into {x: x, y: y};
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
