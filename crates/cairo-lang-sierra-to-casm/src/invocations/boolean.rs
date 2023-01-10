use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::boolean::BoolConcreteLibfunc;

use super::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;

/// Builds instructions for Sierra bool operations.
pub fn build(
    libfunc: &BoolConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoolConcreteLibfunc::And(_) => build_bool_and(builder),
        BoolConcreteLibfunc::Not(_) => build_bool_not(builder),
        BoolConcreteLibfunc::Xor(_) => build_bool_xor(builder),
        BoolConcreteLibfunc::Equal(_) => misc::build_cell_eq(builder),
    }
}

/// Handles instructions for boolean AND.
fn build_bool_and(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_a, expr_b] = builder.try_get_refs()?;
    let a = expr_a.try_unpack_single()?;
    let b = expr_b.try_unpack_single()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref b;
    };
    casm_build_extend!(casm_builder, let res = a * b;);
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[res]], None)]))
}

/// Handles instructions for boolean NOT.
fn build_bool_not(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let a = builder.try_get_refs::<1>()?[0].try_unpack_single()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder, deref a; };
    casm_build_extend! {casm_builder,
        const one_imm = 1;
        tempvar one = one_imm;
        let res = one - a;
    };
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[res]], None)]))
}

/// Handles instructions for boolean XOR.
fn build_bool_xor(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_a, expr_b] = builder.try_get_refs()?;
    let a = expr_a.try_unpack_single()?;
    let b = expr_b.try_unpack_single()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref b;
    };

    // Outputs `(a - b)^2`.
    casm_build_extend! {casm_builder,
        tempvar diff = a - b;
        let res = diff * diff;
    }
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[res]], None)]))
}
