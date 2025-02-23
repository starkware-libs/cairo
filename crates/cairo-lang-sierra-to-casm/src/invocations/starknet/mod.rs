use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::StarknetHint;
use cairo_lang_sierra::extensions::starknet::StarknetConcreteLibfunc;
use cairo_lang_sierra_gas::core_libfunc_cost::SYSTEM_CALL_COST;
use itertools::Itertools;
use num_bigint::BigInt;

use self::storage::{
    build_storage_address_from_base_and_offset, build_storage_base_address_from_felt252,
};
use super::misc::{build_identity, build_single_cell_const, build_unsigned_try_from_felt252};
use super::{CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::{
    CostValidationInfo, InvocationError, add_input_variables, get_non_fallthrough_statement_id,
};

mod testing;

mod secp256;
mod storage;

/// Builds instructions for Sierra starknet operations.
pub fn build(
    libfunc: &StarknetConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarknetConcreteLibfunc::ClassHashConst(libfunc)
        | StarknetConcreteLibfunc::ContractAddressConst(libfunc)
        | StarknetConcreteLibfunc::StorageBaseAddressConst(libfunc) => {
            build_single_cell_const(builder, libfunc.c.clone())
        }
        StarknetConcreteLibfunc::ClassHashTryFromFelt252(_)
        | StarknetConcreteLibfunc::ContractAddressTryFromFelt252(_)
        | StarknetConcreteLibfunc::StorageAddressTryFromFelt252(_) => {
            build_unsigned_try_from_felt252(builder, 251)
        }
        StarknetConcreteLibfunc::ClassHashToFelt252(_)
        | StarknetConcreteLibfunc::ContractAddressToFelt252(_)
        | StarknetConcreteLibfunc::StorageAddressToFelt252(_) => build_identity(builder),
        StarknetConcreteLibfunc::StorageBaseAddressFromFelt252(_) => {
            build_storage_base_address_from_felt252(builder)
        }
        StarknetConcreteLibfunc::StorageAddressFromBase(_) => build_identity(builder),
        StarknetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => {
            build_storage_address_from_base_and_offset(builder)
        }
        StarknetConcreteLibfunc::StorageRead(_) => {
            build_syscalls(builder, "StorageRead", [1, 1], [1])
        }
        StarknetConcreteLibfunc::StorageWrite(_) => {
            build_syscalls(builder, "StorageWrite", [1, 1, 1], [])
        }
        StarknetConcreteLibfunc::CallContract(_) => {
            build_syscalls(builder, "CallContract", [1, 1, 2], [2])
        }
        StarknetConcreteLibfunc::EmitEvent(_) => build_syscalls(builder, "EmitEvent", [2, 2], []),
        StarknetConcreteLibfunc::GetBlockHash(_) => {
            build_syscalls(builder, "GetBlockHash", [1], [1])
        }
        StarknetConcreteLibfunc::GetExecutionInfo(_)
        | StarknetConcreteLibfunc::GetExecutionInfoV2(_) => {
            build_syscalls(builder, "GetExecutionInfo", [], [1])
        }
        StarknetConcreteLibfunc::Deploy(_) => {
            build_syscalls(builder, "Deploy", [1, 1, 2, 1], [1, 2])
        }
        StarknetConcreteLibfunc::Keccak(_) => build_syscalls(builder, "Keccak", [2], [2]),
        StarknetConcreteLibfunc::Sha256ProcessBlock(_) => {
            build_syscalls(builder, "Sha256ProcessBlock", [1, 1], [1])
        }
        StarknetConcreteLibfunc::Sha256StateHandleInit(_) => build_identity(builder),
        StarknetConcreteLibfunc::Sha256StateHandleDigest(_) => build_identity(builder),
        StarknetConcreteLibfunc::LibraryCall(_) => {
            build_syscalls(builder, "LibraryCall", [1, 1, 2], [2])
        }
        StarknetConcreteLibfunc::ReplaceClass(_) => {
            build_syscalls(builder, "ReplaceClass", [1], [])
        }
        StarknetConcreteLibfunc::GetClassHashAt(_) => {
            build_syscalls(builder, "GetClassHashAt", [1], [1])
        }
        StarknetConcreteLibfunc::SendMessageToL1(_) => {
            build_syscalls(builder, "SendMessageToL1", [1, 2], [])
        }
        StarknetConcreteLibfunc::MetaTxV0(_) => {
            build_syscalls(builder, "MetaTxV0", [1, 1, 2, 2], [2])
        }
        StarknetConcreteLibfunc::Testing(libfunc) => testing::build(libfunc, builder),
        StarknetConcreteLibfunc::Secp256(libfunc) => secp256::build(libfunc, builder),
    }
}

/// Builds instructions for Starknet system calls.
pub fn build_syscalls<const INPUT_COUNT: usize, const OUTPUT_COUNT: usize>(
    builder: CompiledInvocationBuilder<'_>,
    selector: &str,
    input_sizes: [i16; INPUT_COUNT],
    output_sizes: [i16; OUTPUT_COUNT],
) -> Result<CompiledInvocation, InvocationError> {
    let selector_imm = BigInt::from_bytes_be(num_bigint::Sign::Plus, selector.as_bytes());
    // +2 for Gas and System builtins.
    if builder.refs.len() != INPUT_COUNT + 2 {
        return Err(InvocationError::WrongNumberOfArguments {
            expected: INPUT_COUNT + 2,
            actual: builder.refs.len(),
        });
    }
    let [gas_builtin] = builder.refs[0].expression.try_unpack()?;
    let [system] = builder.refs[1].expression.try_unpack()?;
    let mut casm_builder = CasmBuilder::default();
    // +2 for Gas and Selector cells.
    let total_input_size = input_sizes.iter().sum::<i16>() + 2;
    let success_output_size = output_sizes.iter().sum::<i16>();
    // Start and end of revert reason array.
    let failure_output_size: i16 = 2;
    add_input_variables! {casm_builder,
        deref gas_builtin;
        // +2 for Gas and failure flag.
        buffer(total_input_size + success_output_size.max(failure_output_size) + 2) system;
    };
    casm_build_extend! {casm_builder,
        let original_system = system;
        const selector_imm = selector_imm;
        tempvar selector = selector_imm;
        assert selector = *(system++);
        assert gas_builtin = *(system++);
    };
    for (i, input_size) in input_sizes.iter().enumerate() {
        let cells = &builder.refs[i + 2].expression.cells;
        if *input_size as usize != cells.len() {
            return Err(InvocationError::InvalidReferenceExpressionForArgument);
        }
        for cell in cells {
            add_input_variables!(casm_builder, deref cell;);
            casm_build_extend!(casm_builder, assert cell = *(system++););
        }
    }
    casm_build_extend! {casm_builder,
        hint StarknetHint::SystemCall { system: original_system };
        let updated_gas_builtin = *(system++);
        tempvar failure_flag = *(system++);
    };
    let mut success_final_system = None;
    let mut failure_final_system = None;
    let max_output_size = std::cmp::max(success_output_size, failure_output_size);
    let response_vars = (0..max_output_size)
        .map(|i| {
            if i == success_output_size {
                casm_build_extend!(casm_builder, let curr_system = system;);
                success_final_system = Some(curr_system);
            }
            if i == failure_output_size {
                casm_build_extend!(casm_builder, let curr_system = system;);
                failure_final_system = Some(curr_system);
            }
            casm_build_extend!(casm_builder, let response = *(system++););
            response
        })
        .collect_vec();
    let updated_gas_builtin = [updated_gas_builtin];
    let success_final_system = [success_final_system.unwrap_or(system)];
    let failure_final_system = [failure_final_system.unwrap_or(system)];
    let mut success_vars = vec![&updated_gas_builtin[..], &success_final_system[..]];
    let mut offset = 0;
    for output_size in output_sizes {
        success_vars.push(&response_vars[offset..(offset + output_size as usize)]);
        offset += output_size as usize;
    }

    casm_build_extend!(casm_builder, jump Failure if failure_flag != 0;);

    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &success_vars, None),
            (
                "Failure",
                &[
                    &updated_gas_builtin,
                    &failure_final_system[..],
                    &[response_vars[0], response_vars[1]],
                ],
                Some(failure_handle_statement_id),
            ),
        ],
        CostValidationInfo {
            builtin_infos: vec![],
            extra_costs: Some([SYSTEM_CALL_COST, SYSTEM_CALL_COST]),
        },
    ))
}
