use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_sierra::extensions::poseidon::PoseidonConcreteLibfunc;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::add_input_variables;

/// Builds instructions for Sierra poseidon operations.
pub fn build(
    libfunc: &PoseidonConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        PoseidonConcreteLibfunc::HadesPermutation(_) => build_poseidon_permutation(builder),
    }
}

/// Handles instruction for computing the hades permutation on 3 felt252s.
fn build_poseidon_permutation(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [poseidon, s0, s1, s2] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref s0;
        deref s1;
        deref s2;
        buffer(5) poseidon;
    };
    casm_build_extend! {casm_builder,
        assert s0 = *(poseidon++);
        assert s1 = *(poseidon++);
        assert s2 = *(poseidon++);
        let r0 = *(poseidon++);
        let r1 = *(poseidon++);
        let r2 = *(poseidon++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[poseidon], &[r0], &[r1], &[r2]], None)],
        Default::default(),
    ))
}
