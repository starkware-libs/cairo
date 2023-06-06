use cairo_lang_sierra::extensions::starknet::secp256::{
    Secp256ConcreteLibfunc, Secp256OpConcreteLibfunc,
};

use super::{build_syscalls, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra secp256k1 operations.
pub fn build(
    libfunc: &Secp256ConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        Secp256ConcreteLibfunc::K1(libfunc) => match libfunc {
            Secp256OpConcreteLibfunc::New(_) => {
                build_syscalls(builder, "Secp256k1EcNew", [2, 2], [2])
            }
            Secp256OpConcreteLibfunc::Add(_) => {
                build_syscalls(builder, "Secp256k1EcAdd", [1, 1], [1])
            }
            Secp256OpConcreteLibfunc::Mul(_) => {
                build_syscalls(builder, "Secp256k1EcMul", [1, 2], [1])
            }
            Secp256OpConcreteLibfunc::GetPointFromX(_) => {
                build_syscalls(builder, "Secp256k1EcGetPointFromX", [2, 1], [2])
            }
            Secp256OpConcreteLibfunc::GetCoordinates(_) => {
                build_syscalls(builder, "Secp256k1EcGetCoordinates", [1], [2, 2])
            }
        },
        Secp256ConcreteLibfunc::R1(libfunc) => match libfunc {
            Secp256OpConcreteLibfunc::New(_) => {
                build_syscalls(builder, "Secp256r1EcNew", [2, 2], [2])
            }
            Secp256OpConcreteLibfunc::Add(_) => {
                build_syscalls(builder, "Secp256r1EcAdd", [1, 1], [1])
            }
            Secp256OpConcreteLibfunc::Mul(_) => {
                build_syscalls(builder, "Secp256r1EcMul", [1, 2], [1])
            }
            Secp256OpConcreteLibfunc::GetPointFromX(_) => {
                build_syscalls(builder, "Secp256r1EcGetPointFromX", [2, 1], [2])
            }
            Secp256OpConcreteLibfunc::GetCoordinates(_) => {
                build_syscalls(builder, "Secp256r1EcGetCoordinates", [1], [2, 2])
            }
        },
    }
}
