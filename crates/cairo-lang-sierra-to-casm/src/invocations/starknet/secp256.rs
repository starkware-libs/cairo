use cairo_lang_sierra::extensions::starknet::secp256::{
    Secp256EcConcreteLibfunc, Secp256EcOpConcreteLibfunc,
};

use super::{build_syscalls, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra secp256k1 operations.
pub fn build(
    libfunc: &Secp256EcConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Secp256EcConcreteLibfunc::K1(libfunc) => match libfunc {
            Secp256EcOpConcreteLibfunc::New(_) => {
                build_syscalls(builder, "Secp256k1EcNew", [2, 2], [2])
            }
            Secp256EcOpConcreteLibfunc::Add(_) => {
                build_syscalls(builder, "Secp256k1EcAdd", [1, 1], [1])
            }
            Secp256EcOpConcreteLibfunc::Mul(_) => {
                build_syscalls(builder, "Secp256k1EcMul", [1, 2], [1])
            }
            Secp256EcOpConcreteLibfunc::GetPointFromX(_) => {
                build_syscalls(builder, "Secp256k1EcGetPointFromX", [2, 1], [2])
            }
            Secp256EcOpConcreteLibfunc::GetCoordinates(_) => {
                build_syscalls(builder, "Secp256k1EcGetCoordinates", [1], [2, 2])
            }
        },
        Secp256EcConcreteLibfunc::R1(libfunc) => match libfunc {
            Secp256EcOpConcreteLibfunc::New(_) => {
                build_syscalls(builder, "Secp256r1EcNew", [2, 2], [2])
            }
            Secp256EcOpConcreteLibfunc::Add(_) => {
                build_syscalls(builder, "Secp256r1EcAdd", [1, 1], [1])
            }
            Secp256EcOpConcreteLibfunc::Mul(_) => {
                build_syscalls(builder, "Secp256r1EcMul", [1, 2], [1])
            }
            Secp256EcOpConcreteLibfunc::GetPointFromX(_) => {
                build_syscalls(builder, "Secp256r1EcGetPointFromX", [2, 1], [2])
            }
            Secp256EcOpConcreteLibfunc::GetCoordinates(_) => {
                build_syscalls(builder, "Secp256r1EcGetCoordinates", [1], [2, 2])
            }
        },
    }
}
