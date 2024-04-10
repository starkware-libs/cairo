use cairo_lang_sierra::extensions::bounded_int::BoundedIntConcreteLibfunc;
use cairo_lang_sierra::extensions::felt252::Felt252BinaryOperator;

use crate::invocations::felt252::build_felt252_op_with_var;
use crate::invocations::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for bounded int operations.
pub fn build(
    libfunc: &BoundedIntConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoundedIntConcreteLibfunc::Add(_) => {
            build_felt252_op_with_var(builder, Felt252BinaryOperator::Add)
        }
        BoundedIntConcreteLibfunc::Sub(_) => {
            build_felt252_op_with_var(builder, Felt252BinaryOperator::Sub)
        }
        BoundedIntConcreteLibfunc::Mul(_) => {
            build_felt252_op_with_var(builder, Felt252BinaryOperator::Mul)
        }
    }
}
