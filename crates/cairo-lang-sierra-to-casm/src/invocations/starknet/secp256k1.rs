use cairo_lang_sierra::extensions::starknet::secp256k1::Secp256K1EcConcreteLibfunc;

use super::{build_syscalls, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra secp256k1 operations.
pub fn build(
    libfunc: &Secp256K1EcConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Secp256K1EcConcreteLibfunc::Add(_) => {
            build_syscalls(builder, "Secp256K1EcAdd", [1, 1], [1])
        }
        Secp256K1EcConcreteLibfunc::Mul(_) => {
            build_syscalls(builder, "Secp256K1EcMul", [1, 2], [1])
        }
        Secp256K1EcConcreteLibfunc::GetPointFromX(_) => {
            build_syscalls(builder, "Secp256K1EcGetPointFromX", [2, 1], [2])
        }
    }
}
