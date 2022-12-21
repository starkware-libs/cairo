use std::str::FromStr;
use std::vec;

use casm::ap_change::ApplyApChange;
use casm::builder::{CasmBuildResult, CasmBuilder};
use casm::hints::Hint;
use casm::instructions::{AddApInstruction, Instruction, InstructionBody};
use casm::operand::{CellRef, DerefOrImmediate, Register, ResOperand};
use casm::{casm, casm_build_extend, casm_extend};
use num_bigint::BigInt;
use sierra::extensions::dict_felt_to::DictFeltToConcreteLibFunc;
use sierra::extensions::felt::FeltBinaryOperator;
use sierra_ap_change::core_libfunc_ap_change;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{BinOpExpression, CellExpression, ReferenceExpression, ReferenceValue};

/// Builds instructions for Sierra single cell dict operations.
pub fn build(
    libfunc: &DictFeltToConcreteLibFunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        DictFeltToConcreteLibFunc::New(_) => build_dict_felt_to_new(builder),
        DictFeltToConcreteLibFunc::Read(_) => build_dict_felt_to_read(builder),
        DictFeltToConcreteLibFunc::Write(_) => build_dict_felt_to_write(builder),
        DictFeltToConcreteLibFunc::Squash(_) => build_dict_felt_to_squash(builder),
    }
}

/// Handles instruction for creating a new single cell dict.
fn build_dict_felt_to_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let dict_manager_ptr_ref = match builder.refs {
        [ReferenceValue { expression: expr_dict_manager, .. }] => {
            expr_dict_manager.try_unpack_single()?.to_deref()?
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    let mut casm_builder = CasmBuilder::default();
    // A pointer into the dict manager segment, where the current dict manager info is stored.
    // The new dict manager info will be written after the current info and the returned dict
    // manager pointer will point to the new dict manager info.
    let dict_manager_ptr = casm_builder.add_var(ResOperand::Deref(dict_manager_ptr_ref));
    // The current dict manager info.
    let dict_infos_start = casm_builder.add_var(ResOperand::DoubleDeref(dict_manager_ptr_ref, 0));
    let n_dicts = casm_builder.add_var(ResOperand::DoubleDeref(dict_manager_ptr_ref, 1));
    let n_destructed = casm_builder.add_var(ResOperand::DoubleDeref(dict_manager_ptr_ref, 2));
    // New dict manager info to be writtern.
    let new_dict_infos_start =
        casm_builder.add_var(ResOperand::DoubleDeref(dict_manager_ptr_ref, 3));
    let new_n_dicts = casm_builder.add_var(ResOperand::DoubleDeref(dict_manager_ptr_ref, 4));
    let new_n_destructed = casm_builder.add_var(ResOperand::DoubleDeref(dict_manager_ptr_ref, 5));
    let imm_1 = casm_builder.add_var(ResOperand::Immediate(1.into()));
    let imm_3 = casm_builder.add_var(ResOperand::Immediate(3.into()));
    casm_build_extend! {casm_builder,
        // A pointer to the end of the newly created dict, currently same as the start of the new dict.
        tempvar new_dict_end;
        hint AllocDictFeltTo {dict_manager_ptr: dict_manager_ptr} into {new_dict_end_dst: new_dict_end};
        // Temporary cells on the stack for a copy of the current dict manager data.
        tempvar temp_dict_infos_start;
        tempvar temp_n_dicts;
        tempvar temp_n_destructed;
        assert temp_dict_infos_start = dict_infos_start;
        assert temp_n_dicts = n_dicts;
        assert temp_n_destructed = n_destructed;
        // Increment the number of dicts by 1.
        tempvar temp_n_dicts_plus1;
        assert temp_n_dicts_plus1 = temp_n_dicts + imm_1;
        // Write the dict manager data back into the dict manager segment.
        assert temp_dict_infos_start = new_dict_infos_start;
        assert temp_n_dicts_plus1 = new_n_dicts;
        assert temp_n_destructed = new_n_destructed;
        // Create a pointer to the new dict manager data.
        tempvar new_dict_manager_ptr;
        // TODO(Gil): add view to dict manager and return offseted end instead of store.
        assert new_dict_manager_ptr = dict_manager_ptr + imm_3;
    };
    let CasmBuildResult { instructions, fallthrough_state, .. } = casm_builder.build();
    assert_eq!(
        core_libfunc_ap_change::core_libfunc_ap_change(builder.libfunc),
        [sierra_ap_change::ApChange::Known(fallthrough_state.ap_change)]
    );
    Ok(builder.build(
        instructions,
        vec![],
        [[
            ReferenceExpression::from_cell(CellExpression::Deref(
                fallthrough_state.get_adjusted_as_cell_ref(new_dict_manager_ptr),
            )),
            ReferenceExpression::from_cell(CellExpression::Deref(
                fallthrough_state.get_adjusted_as_cell_ref(new_dict_end),
            )),
        ]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles instruction for reading from a single cell dict.
fn build_dict_felt_to_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (mut dict_ptr, mut dict_offset, mut key) = match builder.refs {
        [
            ReferenceValue { expression: expr_dict, .. },
            ReferenceValue { expression: expr_key, .. },
        ] => {
            let (dict_ptr, dict_offset) = expr_dict.try_unpack_single()?.to_deref_with_offset()?;
            let key = expr_key.try_unpack_single()?.to_deref()?;
            (dict_ptr, dict_offset, key)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 2,
                actual: refs.len(),
            });
        }
    };

    let mut instructions = vec![Instruction {
        body: InstructionBody::AddAp(AddApInstruction { operand: ResOperand::from(1) }),
        inc_ap: false,
        hints: vec![Hint::DictFeltToRead {
            dict_ptr,
            dict_offset: dict_offset as u16,
            value_dst: CellRef { register: Register::AP, offset: 0 },
            key,
        }],
    }];
    // Correct references for the stack changes in the hint above.
    let ap_change = 1;
    key = key.unchecked_apply_known_ap_change(ap_change);
    dict_ptr = dict_ptr.unchecked_apply_known_ap_change(ap_change);
    instructions.extend(
        DictFeltToAccess {
            key,
            prev_value: CellRef { register: Register::AP, offset: -1 },
            new_value: CellRef { register: Register::AP, offset: -1 },
        }
        .get_instructions(dict_ptr, dict_offset),
    );
    dict_offset += DictFeltToAccess::size() as i16;

    Ok(builder.build(
        instructions,
        vec![],
        [[
            ReferenceExpression {
                cells: vec![CellExpression::BinOp(BinOpExpression {
                    op: FeltBinaryOperator::Add,
                    a: dict_ptr,
                    b: DerefOrImmediate::Immediate(BigInt::from(dict_offset)),
                })],
            },
            ReferenceExpression {
                cells: vec![CellExpression::Deref(CellRef { register: Register::AP, offset: -1 })],
            },
        ]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles instruction for writing to a single cell dict.
fn build_dict_felt_to_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (mut dict_ptr, mut dict_offset, mut key, mut value) = match builder.refs {
        [
            ReferenceValue { expression: expr_dict, .. },
            ReferenceValue { expression: expr_key, .. },
            ReferenceValue { expression: expr_value, .. },
        ] => {
            let (dict_ptr, dict_offset) = expr_dict.try_unpack_single()?.to_deref_with_offset()?;
            let key = expr_key.try_unpack_single()?.to_deref()?;
            let value = expr_value.try_unpack_single()?.to_deref()?;
            (dict_ptr, dict_offset, key, value)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 3,
                actual: refs.len(),
            });
        }
    };
    let mut instructions = vec![Instruction {
        body: InstructionBody::AddAp(AddApInstruction { operand: ResOperand::from(1) }),
        inc_ap: false,
        hints: vec![Hint::DictFeltToWrite {
            dict_ptr,
            dict_offset: dict_offset as u16,
            key,
            value,
            prev_value_dst: CellRef { register: Register::AP, offset: 0 },
        }],
    }];
    // Correct references for the stack changes in the hint above.
    let ap_change = 1;
    key = key.unchecked_apply_known_ap_change(ap_change);
    value = value.unchecked_apply_known_ap_change(ap_change);
    dict_ptr = dict_ptr.unchecked_apply_known_ap_change(ap_change);
    instructions.extend(
        DictFeltToAccess {
            key,
            prev_value: CellRef { register: Register::AP, offset: -1 },
            new_value: value,
        }
        .get_instructions(dict_ptr, dict_offset),
    );
    dict_offset += DictFeltToAccess::size() as i16;
    Ok(builder.build(
        instructions,
        vec![],
        [[ReferenceExpression {
            cells: vec![CellExpression::BinOp(BinOpExpression {
                op: FeltBinaryOperator::Add,
                a: dict_ptr,
                b: DerefOrImmediate::Immediate(BigInt::from(dict_offset)),
            })],
        }]
        .into_iter()]
        .into_iter(),
    ))
}

/// Handles the dict_squash instruction.
fn build_dict_felt_to_squash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let (range_check, mut dict_ptr, dict_offset) = match builder.refs {
        [
            ReferenceValue { expression: expr_range_check, .. },
            ReferenceValue { expression: expr_dict, .. },
        ] => {
            let (dict_ptr, dict_offset) = expr_dict.try_unpack_single()?.to_deref_with_offset()?;
            let range_check = expr_range_check.try_unpack_single()?.to_deref()?;
            (range_check, dict_ptr, dict_offset)
        }
        refs => {
            return Err(InvocationError::WrongNumberOfArguments {
                expected: 1,
                actual: refs.len(),
            });
        }
    };
    // ceil((PRIME / 2) / 2 ** 128).
    let prime_over_2_high = BigInt::from_str("3544607988759775765608368578435044694").unwrap();
    // ceil((PRIME / 3) / 2 ** 128).
    let prime_over_3_high = BigInt::from_str("5316911983139663648412552867652567041").unwrap();
    // 1/3 mod PRIME
    let inverse3 = BigInt::from_str(
        "1206167596222043737899107594365023368541035738443865566657697352045290673494",
    )
    .unwrap();
    dict_ptr = dict_ptr.unchecked_apply_known_ap_change(2);
    let mut casm_ctx = casm!(
        [ap] = range_check, ap++;
        [ap] = dict_ptr + dict_offset, ap++;
        call rel 61;
        jmp rel 174;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 1 });
    casm_extend!(casm_ctx,
        [ap] = [[fp + (-5)]], ap++;
        [ap] = [[fp + (-5)] + 1], ap++;
        [ap] = [ap + (-1)] * prime_over_2_high, ap++;
        [ap] = [ap + (-3)] + [ap + (-1)], ap++;
        [ap] = [[fp + (-5)] + 2], ap++;
        [ap] = [[fp + (-5)] + 3], ap++;
        [ap] = [ap + (-1)] * prime_over_3_high, ap++;
        [ap] = [ap + (-3)] + [ap + (-1)], ap++;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 2 });
    casm_extend!(casm_ctx,
        jmp rel 14 if [ap] != 0, ap++;
        [ap] = (-1), ap++;
        [ap + (-1)] = [ap] + [fp + (-4)], ap++;
        [ap + (-1)] = [ap + (-8)] + [ap + (-4)];
        [fp + (-4)] = [ap] + [fp + (-3)] , ap++;
        [ap] = [fp + (-3)] + 1, ap++;
        [ap] = [ap + (-2)] * [ap + (-1)], ap++;
        [ap + (-1)] = [ap + (-11)] * [ap + (-7)];
        [ap] = [fp + (-5)] + 4, ap++;
        ret;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 3 });
    casm_extend!(casm_ctx,
        [ap] = (-1), ap++;
        [ap + (-1)] = [ap] + [fp + (-3)], ap++;
        [ap] = [fp + (-4)] + [ap + (-1)], ap++;
        [ap + (-1)] = [ap + (-10)] + [ap + (-6)];
        [ap] = [fp + (-4)] * [ap + (-2)], ap++;
        [ap + (-1)] = [ap + (-11)] * [ap + (-7)];
        [ap] = [fp + (-5)] + 4, ap++;
        ret;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 4 });
    casm_extend!(casm_ctx,
        [fp + (-3)] = [ap + (-7)] + [ap + (-3)];
        [fp + (-3)] =  [ap] + [fp + (-4)], ap++;
        [ap] = [fp + (-4)] * [ap + (-1)], ap++;
        [ap + (-1)] = [ap + (-9)] * [ap + (-5)];
        ap += 2;
        [ap] = [fp + (-5)] + 4, ap++;
        ret;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 5 });
    casm_extend!(casm_ctx,
        [fp + (-4)] = [ap] + [fp + (-3)], ap++;
        jmp rel 4 if [ap + (-1)] != 0;
        [fp + (-4)] = [fp + (-4)] + 1;
        [ap] = [fp + (-5)], ap++;
        [ap] = [fp + (-4)], ap++;
        [ap] = [fp + (-3)], ap++;
        call rel (-53);
        ret;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 6 });
    casm_extend!(casm_ctx,
        ap += 1;
        ret;
        ap += 1;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 7 });
    casm_extend!(casm_ctx,
        call rel (-5);
        [fp] = [ap + (-1)];
    );
    casm_ctx.current_hints.push(Hint::ExitScope);
    casm_extend!(casm_ctx,
        [ap] = [fp + (-5)], ap++;
        [ap] = [fp + (-4)], ap++;
        [ap] = [fp + (-3)], ap++;
        [ap] = [fp], ap++;
        call rel 6;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 8 });
    casm_extend!(casm_ctx,
        [ap] = [ap + (-2)], ap++;
        [ap] = [fp], ap++;
        [ap] = [ap + (-3)], ap++;
        ret;
        ap += 3;
    );
    casm_ctx.current_hints.push(Hint::EnterScope);
    casm_extend!(casm_ctx,
        [fp + (-4)] = [fp] + [fp + (-5)];
        jmp rel 5 if [fp] != 0;
    );
    casm_ctx.current_hints.push(Hint::ExitScope);
    casm_extend!(casm_ctx,
        [ap] = [fp + (-6)], ap++;
        [ap] = [fp + (-3)], ap++;
        ret;
        [ap] = [fp] * inverse3, ap++;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 9 });
    casm_extend!(casm_ctx,
        jmp rel 7 if [fp + 2] != 0;
        [fp + 1] = [[fp + (-6)]];
        [ap] = [fp + (-6)] + 1, ap++;
        jmp rel 3;
        [ap] = [fp + (-6)], ap++;
        [ap] = [fp + (-5)], ap++;
        [ap] = [fp + (-4)] + (-1), ap++;
        [ap] = [fp + 1], ap++;
        [ap] = [ap + (-5)], ap++;
        [ap] = [fp + (-3)], ap++;
        [ap] = [fp + 2], ap++;
        call rel 3;
    );
    casm_ctx.current_hints.push(Hint::ExitScope);
    casm_extend!(casm_ctx,
        ret;
        ap += 2;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 10 });
    casm_extend!(casm_ctx,
        [ap] = [[fp + (-9)]], ap++;
        [ap] = [ap + (-1)] * 3, ap++;
        [ap + 1] = [fp + (-8)] + [ap + (-1)], ap++;
        [ap + (-1)] = [[ap] + 2], ap++;
        [ap] = [fp + (-9)] + 1, ap++;
        [fp + (-6)] = [[ap + (-2)]];
        [fp + (-6)] = [[fp + (-4)]];
        [fp] = [[ap + (-2)] + 1];
        [fp] = [[fp + (-4)] + 1];
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 11 });
    casm_extend!(casm_ctx, jmp rel 15 if [fp + 1] != 0;);
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 12 });
    casm_extend!(casm_ctx,
        [ap] = [[ap + (-1)]], ap++;
        [ap] = [ap + (-1)] + 1, ap++;
        [ap] = [ap + (-1)] * 3, ap++;
        [ap + 2] = [ap + (-5)] + [ap + (-1)], ap++;
        [ap + (-7)] = [[ap + 1] + 1];
        [ap] = [[ap + 1] + 2], ap++;
        [fp + (-6)] = [[ap]];
        [ap + 1] = [ap + (-6)] + 1, ap++;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 13 });
    casm_extend!(casm_ctx, jmp rel (-11) if [ap + (-3)] != 0, ap++;);
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 14 });
    casm_extend!(casm_ctx,
        [fp + (-7)] = [ap] + [ap + (-2)];
        [ap] = [[ap + (-1)]], ap++;
        [ap + (-2)] = [ap] + [fp + (-9)], ap++;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 15 });
    casm_extend!(casm_ctx,
        [ap + (-5)] = [[fp + (-4)] + 2];
        [fp + (-5)] = [ap] + [ap + (-1)], ap++;
        jmp rel 7 if [ap + (-1)] != 0;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 16 });
    casm_extend!(casm_ctx,
        [ap] = [ap + (-4)] + 1, ap++;
        [ap] = [fp + (-4)] + 3, ap++;
        ret;
        ap += 1;
    );
    casm_ctx.current_hints.push(Hint::DictSquashHints { hint_index: 17 });
    casm_extend!(casm_ctx,
        jmp rel 14 if [fp + (-3)] != 0;
        [ap] = [fp + (-6)] + 1, ap++;
        [ap + (-2)] = [ap] + [ap + (-1)], ap++;
        [ap + (-1)] = [[ap + (-7)] + 1];
        [ap] = [ap + (-7)] + 2, ap++;
        [ap] = [fp + (-8)], ap++;
        [ap] = [fp + (-7)], ap++;
        [ap] = [ap + (-6)], ap++;
        [ap] = [ap + (-8)], ap++;
        jmp rel 12;
        [ap] = [ap + (-5)] + 1, ap++;
        [ap] = [fp + (-6)], ap++;
        [ap] = [ap + (-3)], ap++;
        call rel (-117);
        [ap] = [fp + (-8)], ap++;
        [ap] = [fp + (-7)], ap++;
        [ap] = [ap + (-29)], ap++;
        [ap] = [ap + (-31)], ap++;
        [ap] = [fp + (-4)] + 3, ap++;
        [ap] = [fp + (-3)], ap++;
        call rel (-69);
        ret;
    );
    Ok(builder.build(
        casm_ctx.instructions,
        vec![],
        [[
            // Range check pointer
            ReferenceExpression {
                cells: vec![CellExpression::Deref(CellRef { register: Register::AP, offset: -3 })],
            },
            // Squashed dict
            ReferenceExpression {
                cells: vec![
                    CellExpression::Deref(CellRef { register: Register::AP, offset: -2 }),
                    CellExpression::Deref(CellRef { register: Register::AP, offset: -1 }),
                ],
            },
        ]
        .into_iter()]
        .into_iter(),
    ))
}

/// Represents a read/write access to the dict.
struct DictFeltToAccess {
    key: CellRef,
    prev_value: CellRef,
    new_value: CellRef,
}

impl DictFeltToAccess {
    /// Returns a set of instructions for storing the dict access data into the set of consecutive
    /// cells at the end of the dict_segment.
    fn get_instructions(&self, dict_ptr: CellRef, dict_offset: i16) -> Vec<Instruction> {
        // TODO(Gil): Try to avoid the following assignments.
        let key = self.key;
        let prev_value = self.prev_value;
        let new_value = self.new_value;
        casm! {
           key = [[&dict_ptr] + dict_offset];
           prev_value = [[&dict_ptr] + (dict_offset + 1)];
           new_value = [[&dict_ptr] + (dict_offset + 2)];
        }
        .instructions
    }
    /// Returns the number casm cells representing a DictAccess.
    fn size() -> usize {
        3
    }
}
