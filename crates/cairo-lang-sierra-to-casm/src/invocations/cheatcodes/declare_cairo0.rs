use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo,
};

// pub const SYSTEM_CALL_COST: i32 =
//     ConstCost { steps: 100, holes: 0, range_checks: 0 }.cost();

pub fn build_declare_cairo0(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [contract] = builder.try_get_single_cells()?;

    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref contract;
    };

    casm_build_extend! {casm_builder,
        tempvar err_code;
        tempvar result;
        hint ProtostarHint::DeclareCairo0 {contract: contract} into {result: result, err_code: err_code};
        ap += 1;
        jump Failure if err_code != 0;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[result]], None),
            ("Failure", &[&[err_code]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo { range_check_info: None, extra_costs: None },
    ))
}
