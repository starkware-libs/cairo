use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use num_bigint::{BigInt, ToBigInt};
use num_traits::Signed;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::misc::{build_single_cell_const, validate_under_limit};
use crate::invocations::{add_input_variables, CostValidationInfo};

/// Handles the storage_base_address_const libfunc.
pub fn build_storage_base_address_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = (BigInt::from(1) << 251) - 256;
    if libfunc.c.is_negative() || libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }
    build_single_cell_const(builder, libfunc.c.clone())
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
pub fn build_storage_base_address_from_felt252(
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
        &(Felt252::prime().to_bigint().unwrap() - addr_bound.clone()),
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
