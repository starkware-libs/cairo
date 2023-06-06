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
                build_syscalls(builder, "Secp256k1New", [2, 2], [2])
            }
            Secp256OpConcreteLibfunc::Add(_) => {
                build_syscalls(builder, "Secp256k1Add", [1, 1], [1])
            }
            Secp256OpConcreteLibfunc::Mul(_) => {
                build_syscalls(builder, "Secp256k1Mul", [1, 2], [1])
            }
            Secp256OpConcreteLibfunc::GetPointFromX(_) => {
                build_syscalls(builder, "Secp256k1GetPointFromX", [2, 1], [2])
            }
            Secp256OpConcreteLibfunc::GetXy(_) => {
                build_syscalls(builder, "Secp256k1GetXy", [1], [2, 2])
            }
        },
        Secp256ConcreteLibfunc::R1(libfunc) => match libfunc {
            Secp256OpConcreteLibfunc::New(_) => {
                build_syscalls(builder, "Secp256r1New", [2, 2], [2])
            }
            Secp256OpConcreteLibfunc::Add(_) => {
                build_syscalls(builder, "Secp256r1Add", [1, 1], [1])
            }
            Secp256OpConcreteLibfunc::Mul(_) => {
                build_syscalls(builder, "Secp256r1Mul", [1, 2], [1])
            }
            Secp256OpConcreteLibfunc::GetPointFromX(_) => {
                build_syscalls(builder, "Secp256r1GetPointFromX", [2, 1], [2])
            }
            Secp256OpConcreteLibfunc::GetXy(_) => {
                build_syscalls(builder, "Secp256r1GetXy", [1], [2, 2])
            }
        },
    }
}
