use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::CellOperator;
use cairo_lang_sierra::extensions::bounded_int::BoundedIntConcreteLibfunc;

use crate::invocations::{
    add_input_variables, CompiledInvocation, CompiledInvocationBuilder, CostValidationInfo,
    InvocationError,
};

/// Builds instructions for bounded int operations.
pub fn build(
    libfunc: &BoundedIntConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoundedIntConcreteLibfunc::Add(_) => build_simple_op(builder, CellOperator::Add),
        BoundedIntConcreteLibfunc::Sub(_) => build_simple_op(builder, CellOperator::Sub),
        BoundedIntConcreteLibfunc::Mul(_) => build_simple_op(builder, CellOperator::Mul),
    }
}

/// Build instructions for simple operations of bounded ints.
fn build_simple_op(
    builder: CompiledInvocationBuilder<'_>,
    op: CellOperator,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref b;
    };
    // Valid since bounded int type is currently bounded by felt252 type range.
    let result = casm_builder.bin_op(op, a, b);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[result]], None)],
        CostValidationInfo::default(),
    ))
}
