use cairo_lang_casm::{builder::{CasmBuilder},  casm_build_extend};

use crate::invocations::{add_input_variables, get_non_fallthrough_statement_id};
use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};

pub fn build_declare(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [contract ] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref contract;
    };

    casm_build_extend! {casm_builder,
        tempvar error_reason;
        tempvar result;
        hint Declare {contract: contract} into {result: error_reason, error_code: error_reason};
        ap += 1;
        jump Failure if error_reason != 0;
    };


    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[result]], None),
            (
                "Failure",
                &[&[error_reason]],
                Some(failure_handle_statement_id),
            ),
        ],
        None
    ))

}
