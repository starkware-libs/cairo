use cairo_lang_casm::casm;
use cairo_lang_casm::operand::Register;
use cairo_lang_sierra::extensions::function_call::SignatureAndFunctionConcreteLibfunc;
use cairo_lang_sierra::extensions::ConcreteLibfunc;

use super::{
    check_references_on_stack, CompiledInvocation, CompiledInvocationBuilder, InvocationError,
};
use crate::references::{build_deref_reference, ReferenceExpression};
use crate::relocations::{Relocation, RelocationEntry};

/// Handles a function call.
pub fn build(
    libfunc: &SignatureAndFunctionConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    check_references_on_stack(builder.refs)?;

    let output_types = libfunc.output_types();
    let fallthrough_outputs = &output_types[0];

    let output_sizes = fallthrough_outputs
        .iter()
        .map(|output_type| {
            builder
                .program_info
                .type_sizes
                .get(output_type)
                .copied()
                .ok_or(InvocationError::UnknownVariableData)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let refs = build_output_references(&output_sizes)?;

    Ok(builder.build(
        casm! { call rel 0; }.instructions,
        vec![RelocationEntry {
            instruction_idx: 0,
            relocation: Relocation::RelativeStatementId(libfunc.function.entry_point),
        }],
        [refs.into_iter()].into_iter(),
    ))
}

fn build_output_references(
    output_sizes: &[i16],
) -> Result<Vec<ReferenceExpression>, InvocationError> {
    let mut refs = Vec::with_capacity(output_sizes.len());
    let mut offset = -1_i64;
    for size in output_sizes.iter().rev() {
        refs.push(
            build_deref_reference(Register::AP, offset, *size)
                .ok_or(InvocationError::IntegerOverflow)?,
        );
        offset -= i64::from(*size);
    }
    Ok(refs.into_iter().rev().collect())
}

#[cfg(test)]
mod tests {
    use cairo_lang_casm::cell_expression::CellExpression;
    use cairo_lang_casm::operand::CellRef;
    use cairo_lang_sierra::ids::ConcreteTypeId;
    use cairo_lang_sierra::program::StatementIdx;

    use super::*;
    use crate::references::{IntroductionPoint, ReferenceValue};

    #[test]
    fn wide_return_offsets_boundaries() {
        for sizes in [&[32767][..], &[16384, 16384][..], &[16383, 16385][..]] {
            let refs = build_output_references(sizes).unwrap();
            assert_eq!(
                refs.iter().map(|reference| reference.cells.len()).collect::<Vec<_>>(),
                sizes.iter().map(|size| usize::try_from(*size).unwrap()).collect::<Vec<_>>()
            );
            let first_cell = refs.first().unwrap().cells.first();
            let first_offset = -sizes.iter().map(|size| i64::from(*size)).sum::<i64>();
            assert_eq!(
                first_cell,
                Some(&CellExpression::Deref(CellRef {
                    register: Register::AP,
                    offset: first_offset.try_into().unwrap(),
                }))
            );
            assert_eq!(
                refs.last().unwrap().cells.last(),
                Some(&CellExpression::Deref(CellRef { register: Register::AP, offset: -1 }))
            );
        }

        assert_eq!(build_output_references(&[16384, 16385]), Err(InvocationError::IntegerOverflow));
    }

    #[test]
    fn full_width_return_is_on_stack() {
        let refs = build_output_references(&[16384, 16384])
            .unwrap()
            .into_iter()
            .enumerate()
            .map(|(output_idx, expression)| ReferenceValue {
                expression,
                ty: ConcreteTypeId::from(output_idx as u64),
                stack_idx: None,
                introduction_point: IntroductionPoint {
                    source_statement_idx: Some(StatementIdx(0)),
                    destination_statement_idx: StatementIdx(1),
                    output_idx,
                },
            })
            .collect::<Vec<_>>();

        assert_eq!(check_references_on_stack(&refs), Ok(()));
    }
}
