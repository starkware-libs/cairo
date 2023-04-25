use cairo_felt::Felt252;
use cairo_lang_casm::builder::CasmBuilder;
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::hints::StarknetHint;
use cairo_lang_sierra::extensions::consts::SignatureAndConstConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;
use cairo_lang_sierra_gas::core_libfunc_cost::SYSTEM_CALL_COST;
use itertools::Itertools;
use num_bigint::{BigInt, ToBigInt};
use num_traits::Signed;

use self::storage::{
    build_storage_address_from_base_and_offset, build_storage_base_address_const,
    build_storage_base_address_from_felt252,
};
use super::misc::{build_identity, build_single_cell_const};
use super::{misc, CompiledInvocation, CompiledInvocationBuilder};
use crate::invocations::misc::validate_under_limit;
use crate::invocations::{
    add_input_variables, get_non_fallthrough_statement_id, CostValidationInfo, InvocationError,
};

mod testing;

mod secp256k1;
mod storage;

/// Builds instructions for Sierra starknet operations.
pub fn build(
    libfunc: &StarkNetConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        StarkNetConcreteLibfunc::ClassHashConst(libfunc)
        | StarkNetConcreteLibfunc::ContractAddressConst(libfunc) => {
            build_u251_const(builder, libfunc)
        }
        StarkNetConcreteLibfunc::ClassHashTryFromFelt252(_)
        | StarkNetConcreteLibfunc::ContractAddressTryFromFelt252(_)
        | StarkNetConcreteLibfunc::StorageAddressTryFromFelt252(_) => {
            build_u251_try_from_felt252(builder)
        }
        StarkNetConcreteLibfunc::ClassHashToFelt252(_)
        | StarkNetConcreteLibfunc::ContractAddressToFelt252(_)
        | StarkNetConcreteLibfunc::StorageAddressToFelt252(_) => build_identity(builder),
        StarkNetConcreteLibfunc::StorageBaseAddressConst(libfunc) => {
            build_storage_base_address_const(builder, libfunc)
        }
        StarkNetConcreteLibfunc::StorageBaseAddressFromFelt252(_) => {
            build_storage_base_address_from_felt252(builder)
        }
        StarkNetConcreteLibfunc::StorageAddressFromBase(_) => misc::build_identity(builder),
        StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => {
            build_storage_address_from_base_and_offset(builder)
        }
        StarkNetConcreteLibfunc::StorageRead(_) => {
            build_syscalls(builder, "StorageRead", [1, 1], [1])
        }
        StarkNetConcreteLibfunc::StorageWrite(_) => {
            build_syscalls(builder, "StorageWrite", [1, 1, 1], [])
        }
        StarkNetConcreteLibfunc::CallContract(_) => {
            build_syscalls(builder, "CallContract", [1, 1, 2], [2])
        }
        StarkNetConcreteLibfunc::EmitEvent(_) => build_syscalls(builder, "EmitEvent", [2, 2], []),
        StarkNetConcreteLibfunc::GetExecutionInfo(_) => {
            build_syscalls(builder, "GetExecutionInfo", [], [1])
        }
        StarkNetConcreteLibfunc::Deploy(_) => {
            build_syscalls(builder, "Deploy", [1, 1, 2, 1], [1, 2])
        }
        StarkNetConcreteLibfunc::Keccak(_) => build_syscalls(builder, "Keccak", [2], [2]),
        StarkNetConcreteLibfunc::LibraryCall(_) => {
            build_syscalls(builder, "LibraryCall", [1, 1, 2], [2])
        }
        StarkNetConcreteLibfunc::ReplaceClass(_) => {
            build_syscalls(builder, "ReplaceClass", [1], [])
        }
        StarkNetConcreteLibfunc::SendMessageToL1(_) => {
            build_syscalls(builder, "SendMessageToL1", [1, 2], [])
        }
        StarkNetConcreteLibfunc::Testing(libfunc) => testing::build(libfunc, builder),
        StarkNetConcreteLibfunc::Secp256K1(libfunc) => secp256k1::build(libfunc, builder),
    }
}

/// Handles the contract_address_const libfunc.
pub fn build_u251_const(
    builder: CompiledInvocationBuilder<'_>,
    libfunc: &SignatureAndConstConcreteLibfunc,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound = BigInt::from(1) << 251;
    if libfunc.c.is_negative() || libfunc.c >= addr_bound {
        return Err(InvocationError::InvalidGenericArg);
    }
    build_single_cell_const(builder, libfunc.c.clone())
}

/// builds a libfunc that tries to convert a felt252 to type with values in the range[0, 2**251).
pub fn build_u251_try_from_felt252(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let addr_bound: BigInt = BigInt::from(1) << 251;
    let [range_check, value] = builder.try_get_single_cells()?;
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(2) range_check;
        deref value;
    };
    let auxiliary_vars: [_; 4] = std::array::from_fn(|_| casm_builder.alloc_var(false));
    casm_build_extend! {casm_builder,
        const limit = addr_bound.clone();
        let orig_range_check = range_check;
        tempvar is_valid_address;
        hint TestLessThan {lhs: value, rhs: limit} into {dst: is_valid_address};
        jump IsValidAddress if is_valid_address != 0;
        tempvar shifted_value = value - limit;
    }
    validate_under_limit::<1>(
        &mut casm_builder,
        &(Felt252::prime().to_bigint().unwrap() - addr_bound.clone()),
        shifted_value,
        range_check,
        &auxiliary_vars,
    );
    casm_build_extend! {casm_builder,
        jump Failure;
        IsValidAddress:
    };
    validate_under_limit::<1>(&mut casm_builder, &addr_bound, value, range_check, &auxiliary_vars);
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[value]], None),
            ("Failure", &[&[range_check]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            range_check_info: Some((orig_range_check, range_check)),
            extra_costs: None,
        },
    ))
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
            range_check_info: None,
            extra_costs: Some([SYSTEM_CALL_COST, SYSTEM_CALL_COST]),
        },
    ))
}
