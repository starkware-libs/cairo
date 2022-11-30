use casm::instructions::Instruction;
use casm::{casm, casm_extend};
use num_bigint::BigInt;
use sierra_to_casm::test_utils::build_metadata;

/// Add instructions pushing arguments, calling a function immediately after the generated code,
/// and returning the function's return values:
///     [ap] = arg0, ap++;
///     [ap] = arg1, ap++;
///     ...
///     call rel 3; // This jumps over the call and the ret.
///     ret;
fn generate_function_runner(params: &[BigInt]) -> Vec<Instruction> {
    let mut ctx = casm!();
    for param in params {
        casm_extend!(ctx, [ap] = (param.clone()), ap++;);
    }
    let before_call_offset = ctx.current_code_offset;
    casm_extend!(ctx, call rel 3; ret;);
    assert_eq!(ctx.current_code_offset, before_call_offset + 3);
    ctx.instructions
}

/// Returns casm that would simulate running the code with the given params.
pub fn get_runnable_casm(
    sierra_program: &sierra::program::Program,
    params: &[BigInt],
    calculate_gas_info: bool,
) -> Vec<Instruction> {
    let mut program = generate_function_runner(params);
    let func = sierra_to_casm::compiler::compile(
        sierra_program,
        &build_metadata(sierra_program, &[], calculate_gas_info),
        calculate_gas_info,
    )
    .unwrap()
    .instructions;
    program.extend(func);
    program
}
