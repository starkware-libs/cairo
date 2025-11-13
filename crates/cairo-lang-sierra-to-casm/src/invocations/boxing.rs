use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::cell_expression::{CellExpression, CellOperator};
use cairo_lang_casm::operand::{CellRef, DerefOrImmediate, Register};
use cairo_lang_casm::{casm, casm_build_extend};
use cairo_lang_sierra::extensions::boxing::BoxConcreteLibfunc;
use cairo_lang_sierra::extensions::lib_func::SignatureAndTypeConcreteLibfunc;
use cairo_lang_sierra::ids::ConcreteTypeId;
use num_bigint::ToBigInt;

use super::misc::build_identity;
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::compiler::UtilityId;
use crate::invocations::add_input_variables;
use crate::references::ReferenceExpression;
use crate::relocations::{Relocation, RelocationEntry};

/// Builds instructions for Sierra box operations.
pub fn build(
    libfunc: &BoxConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        BoxConcreteLibfunc::Into(_) => build_into_box(builder),
        BoxConcreteLibfunc::Unbox(libfunc) => build_unbox(&libfunc.ty, builder),
        BoxConcreteLibfunc::ForwardSnapshot(_) => build_identity(builder),
        BoxConcreteLibfunc::FromTempStore(libfunc) => build_from_temp_store(libfunc, builder),
    }
}

/// Handles instruction for creating a box.
fn build_into_box(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [operand] = builder.try_get_refs()?;
    let mut casm_builder = CasmBuilder::default();
    let addr = if operand.cells.is_empty() {
        // In cases of a zero-sized variable, we just simulate a non-zero address.
        casm_build_extend!(casm_builder,
            const one = 1;
            tempvar addr = one;
        );
        addr
    } else {
        casm_build_extend!(casm_builder,
            const operand_size = operand.cells.len().to_bigint().unwrap();
            tempvar addr;
            hint AllocConstantSize { size: operand_size } into { dst: addr };
        );
        for (index, cell) in operand.cells.iter().enumerate() {
            add_input_variables!(casm_builder, deref cell;);
            casm_build_extend!(casm_builder, assert cell = addr[index as i16];);
        }
        addr
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[addr]], None)],
        Default::default(),
    ))
}

/// Handles instruction for unboxing a box.
fn build_unbox(
    ty: &ConcreteTypeId,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let size = builder.program_info.type_sizes[ty];
    let operand = builder.try_get_single_cells::<1>()?[0]
        .to_deref()
        .ok_or(InvocationError::InvalidReferenceExpressionForArgument)?;
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression {
            cells: (0..size).map(|idx| CellExpression::DoubleDeref(operand, idx)).collect(),
        }]
        .into_iter(),
    ))
}

/// Builds instructions for Sierra box_from_temp_store operations.
///
/// Strategy: Use a call to the BoxFromTempStore utility segment containing "call rel 2; ret;".
/// This reuses the utility segment for the second call and ret point.
///
/// Underlying pattern:
/// 1. call rel 0   // Call to utility segment
/// 2. call rel 2   // Call to same utility segment
/// 3. ret;
fn build_from_temp_store(
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

    // Add relocations to make both calls point to the BoxFromTempStore utility segment
    let relocations = vec![RelocationEntry {
        instruction_idx: 0,
        relocation: Relocation::UtilitySegment(UtilityId::BoxFromTempStore),
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
