use cairo_lang_sierra::extensions::int::signed::{SintConcrete, SintTraits};
use cairo_lang_sierra::extensions::int::IntMulTraits;
use cairo_lang_sierra::extensions::is_zero::IsZeroTraits;

use super::{build_const, build_small_wide_mul};
use crate::invocations::{misc, CompiledInvocation, CompiledInvocationBuilder, InvocationError};

/// Builds instructions for Sierra s8/s16/s32/s64 operations.
pub fn build_sint<TSintTraits: SintTraits + IntMulTraits + IsZeroTraits>(
    libfunc: &SintConcrete<TSintTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        SintConcrete::Const(libfunc) => build_const(libfunc, builder),
        SintConcrete::Equal(_) => misc::build_cell_eq(builder),
        SintConcrete::ToFelt252(_) => misc::build_identity(builder),
        SintConcrete::FromFelt252(_) => todo!(),
        SintConcrete::IsZero(_) => misc::build_is_zero(builder),
        SintConcrete::WideMul(_) => build_small_wide_mul(builder),
    }
}
