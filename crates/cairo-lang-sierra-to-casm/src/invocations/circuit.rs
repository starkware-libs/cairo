use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::circuit::CircuitConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::circuit::CircuitInfo;
use crate::invocations::add_input_variables;

/// Builds instructions for Sierra array operations.
pub fn build(
    libfunc: &CircuitConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        CircuitConcreteLibfunc::InitCircuitData(libfunc) => {
            build_init_circuit_data(&libfunc.ty, builder)
        }
    }
}

/// Handles a Sierra statement for initializing circuit data.
fn build_init_circuit_data(
    circuit_ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_rc96] = builder.try_get_refs()?;
    let rc96 = expr_rc96.try_unpack_single()?;

    let CircuitInfo { n_inputs, n_values } = (builder.program_info.get_circuit_info)(circuit_ty);

    let mut casm_builder = CasmBuilder::default();

    add_input_variables! {casm_builder,
        buffer(1) rc96;
    };
    casm_build_extend! {casm_builder,
        const n_inputs = n_inputs;
        let inputs_end = rc96 + n_inputs;
        const n_vals = n_values;
        let vals_end = rc96 + n_vals;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[vals_end], &[rc96, inputs_end]], None)],
        Default::default(),
    ))
}
