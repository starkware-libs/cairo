use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use num_bigint::BigInt;
use num_traits::Signed;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::ReferenceExpression;

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
