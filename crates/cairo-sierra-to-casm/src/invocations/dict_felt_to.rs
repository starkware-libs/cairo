use std::str::FromStr;
use std::vec;

use cairo_casm::ap_change::ApplyApChange;
use cairo_casm::builder::CasmBuilder;
use cairo_casm::hints::Hint;
use cairo_casm::operand::{CellRef, Register, ResOperand};
use cairo_casm::{casm, casm_build_extend, casm_extend};
use cairo_sierra::extensions::dict_felt_to::DictFeltToConcreteLibfunc;
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::references::{CellExpression, ReferenceExpression};

/// Builds instructions for Sierra single cell dict operations.
pub fn build(
    libfunc: &DictFeltToConcreteLibfunc,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    match libfunc {
        DictFeltToConcreteLibfunc::New(_) => build_dict_felt_to_new(builder),
        DictFeltToConcreteLibfunc::Read(_) => build_dict_felt_to_read(builder),
        DictFeltToConcreteLibfunc::Write(_) => build_dict_felt_to_write(builder),
        DictFeltToConcreteLibfunc::Squash(_) => build_dict_felt_to_squash(builder),
    }
}

/// Handles instruction for creating a new single cell dict.
fn build_dict_felt_to_new(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let dict_manager_ptr_ref = builder.try_get_refs::<1>()?[0].try_unpack_single()?.to_buffer(2)?;
    let mut casm_builder = CasmBuilder::default();
    let dict_manager_ptr = casm_builder.add_var(dict_manager_ptr_ref);
    casm_build_extend! {casm_builder,
        hint AllocDictFeltTo {dict_manager_ptr: dict_manager_ptr};
        // Previous dict info
        tempvar dict_infos_start = *(dict_manager_ptr++);
        tempvar n_dicts = *(dict_manager_ptr++);
        tempvar n_destructed = *(dict_manager_ptr++);
        let new_dict_manager_ptr = dict_manager_ptr;
        // New dict info
        assert dict_infos_start = *(dict_manager_ptr++);
        const imm_1 = 1;
        tempvar new_n_dicts = n_dicts + imm_1;
        assert new_n_dicts = *(dict_manager_ptr++);
        assert n_destructed = *(dict_manager_ptr++);
        const imm_3 = 3;
        tempvar offset = n_dicts * imm_3;
        tempvar new_dict_end_ptr = dict_infos_start + offset;
        let new_dict_end = *new_dict_end_ptr;
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[new_dict_manager_ptr], &[new_dict_end]], None)],
    ))
}

/// Handles instruction for reading from a single cell dict.
fn build_dict_felt_to_read(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_dict, expr_key] = builder.try_get_refs()?;
    let dict_ptr = expr_dict.try_unpack_single()?.to_buffer(2)?;
    let key = expr_key.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let dict_ptr = casm_builder.add_var(dict_ptr);
    let key = casm_builder.add_var(ResOperand::Deref(key));
    casm_build_extend! {casm_builder,
        tempvar value;
        hint DictFeltToRead {dict_ptr: dict_ptr, key: key} into {value_dst: value};
        // Write the new dict access.
        assert key = *(dict_ptr++);
        assert value = *(dict_ptr++);
        assert value = *(dict_ptr++);
    }
    Ok(builder
        .build_from_casm_builder(casm_builder, [("Fallthrough", &[&[dict_ptr], &[value]], None)]))
}

/// Handles instruction for writing to a single cell dict.
fn build_dict_felt_to_write(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_dict, expr_key, expr_value] = builder.try_get_refs()?;
    let dict_ptr = expr_dict.try_unpack_single()?.to_buffer(2)?;
    let key = expr_key.try_unpack_single()?.to_deref()?;
    let value = expr_value.try_unpack_single()?.to_deref()?;

    let mut casm_builder = CasmBuilder::default();
    let dict_ptr = casm_builder.add_var(dict_ptr);
    let key = casm_builder.add_var(ResOperand::Deref(key));
    let value = casm_builder.add_var(ResOperand::Deref(value));
    casm_build_extend! {casm_builder,
        tempvar prev_value;
        hint DictFeltToWrite {dict_ptr: dict_ptr, key: key, value: value} into {prev_value_dst: prev_value};
        // Write the new dict access.
        assert key = *(dict_ptr++);
        assert prev_value = *(dict_ptr++);
        assert value = *(dict_ptr++);
    }
    Ok(builder.build_from_casm_builder(casm_builder, [("Fallthrough", &[&[dict_ptr]], None)]))
}

/// Handles the dict_squash instruction.
fn build_dict_felt_to_squash(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [expr_range_check, expr_dict] = builder.try_get_refs()?;
    let (mut dict_ptr, dict_offset) = expr_dict.try_unpack_single()?.to_deref_with_offset()?;
    let range_check = expr_range_check.try_unpack_single()?.to_deref()?;

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
