use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::CostValidationInfo;
/// Builds instructions unsafe_panic;
pub fn build(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();
    casm_build_extend! {casm_builder,
        fail;
    }
    Ok(builder.build_from_casm_builder(casm_builder, [], CostValidationInfo::default()))
}
