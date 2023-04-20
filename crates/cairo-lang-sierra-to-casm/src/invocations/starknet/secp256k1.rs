use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_sierra::extensions::starknet::secp256k1::Secp256K1EcConcreteLibfunc;

use super::{build_syscalls, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra secp256k1 operations.
pub fn build(
    libfunc: &Secp256K1EcConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Secp256K1EcConcreteLibfunc::New(_) => {
            build_syscalls(builder, "Secp256K1EcNew", [2, 2], [2])
        }
        Secp256K1EcConcreteLibfunc::Add(_) => {
            build_syscalls(builder, "Secp256K1EcAdd", [1, 1], [1])
        }
        Secp256K1EcConcreteLibfunc::Mul(_) => {
            build_syscalls(builder, "Secp256K1EcMul", [1, 2], [1])
        }
        Secp256K1EcConcreteLibfunc::GetPointFromX(_) => {
            build_syscalls(builder, "Secp256K1EcGetPointFromX", [2, 1], [2])
        }
        Secp256K1EcConcreteLibfunc::DivModN(_) => build_u256_div_mod_n(builder),
    }
}

/// Handles a Sierra statement for computing a*b^(-1) modulo n.
fn build_u256_div_mod_n(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let mut casm_builder = CasmBuilder::default();
    // TODO(yg): implement.
    Ok(builder.build_from_casm_builder(casm_builder, [], Default::default()))
}
