use cairo_lang_casm::casm;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::operand::{CellRef, DerefOrImmediate, Register};
use cairo_lang_sierra::extensions::lib_func::SignatureAndTypeConcreteLibfunc;
use num_bigint::ToBigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::compiler::UtilityId;
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra get_temp_ptr operations.
///
/// Strategy: Use a call to the GetTempPtr utility segment containing "call rel 2; ret;".
/// This reuses the utility segment for the second call and ret point.
///
/// Underlying pattern:
/// 1. call rel 0   // Call to utility segment
/// 2. call rel 2   // Call to same utility segment
/// 3. ret;
pub fn build(
    libfunc: &SignatureAndTypeConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let type_size =
        builder.program_info.type_sizes.get(&libfunc.ty).ok_or(InvocationError::UnknownTypeData)?;
    // Compute the offset: ap - 4 - |T|
    let offset = -type_size.to_bigint().unwrap();

    // Two calls to the utility segment
    let ctx = casm! {
        call rel 0;   // Will be relocated to utility segment
    };

    // Add relocations to make both calls point to the GetTempPtr utility segment
    let relocations = vec![RelocationEntry {
        instruction_idx: 0,
        relocation: Relocation::UtilitySegment(UtilityId::GetTempPtr),
    }];

    // Return a deferred reference (BinOp expression) representing [ap - 2] - type_size
    Ok(builder.build(
        ctx.instructions,
        relocations,
        [vec![ReferenceExpression {
            cells: vec![CellExpression::BinOp {
                op: CellOperator::Add,
                a: CellRef { register: Register::AP, offset: -2 },
                b: DerefOrImmediate::Immediate(offset.into()),
            }],
        }]
        .into_iter()]
        .into_iter(),
    ))
}
