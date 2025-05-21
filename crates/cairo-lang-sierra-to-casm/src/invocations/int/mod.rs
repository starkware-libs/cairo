use cairo_lang_casm::builder::{CasmBuilder, Var};
use cairo_lang_casm::casm_build_extend;
use cairo_lang_casm::cell_expression::CellExpression;
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::extensions::int::{IntConstConcreteLibfunc, IntTraits};
use num_bigint::BigInt;

use super::{CompiledInvocation, CompiledInvocationBuilder, InvocationError};
use crate::invocations::{
    BuiltinInfo, CostValidationInfo, add_input_variables, get_non_fallthrough_statement_id,
};
use crate::references::ReferenceExpression;

pub mod bounded;
pub mod signed;
pub mod signed128;
pub mod unsigned;
pub mod unsigned128;
pub mod unsigned256;
pub mod unsigned512;

/// Builds invocations for uint const values.
fn build_const<TIntTraits: IntTraits>(
    libfunc: &IntConstConcreteLibfunc<TIntTraits>,
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    Ok(builder.build_only_reference_changes(
        [ReferenceExpression::from_cell(CellExpression::Immediate(libfunc.c.into()))].into_iter(),
    ))
}

/// Handles a small uint wide multiplication.
pub fn build_small_wide_mul(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let [a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        deref a;
        deref_or_immediate b;
    };

    casm_build_extend! {casm_builder,
        let res = a * b;
    };

    Ok(builder.build_from_casm_builder(
        casm_builder,
        [("Fallthrough", &[&[res]], None)],
        CostValidationInfo::default(),
    ))
}

/// Helper struct for creating libfuncs that handle small integer diff operations.
pub struct SmallDiffHelper<'a> {
    builder: CompiledInvocationBuilder<'a>,
    casm_builder: CasmBuilder,
    orig_range_check: Var,
    pub range_check: Var,
    pub a: Var,
    pub b: Var,
    pub a_minus_b: Var,
    pub wrapping_a_minus_b: Var,
}
impl<'a> SmallDiffHelper<'a> {
    /// Constructs the helper.
    pub fn new(
        builder: CompiledInvocationBuilder<'a>,
        limit: BigInt,
    ) -> Result<Self, InvocationError> {
        let [range_check, a, b] = builder.try_get_single_cells()?;
        let mut casm_builder = CasmBuilder::default();
        add_input_variables! {casm_builder,
            buffer(0) range_check;
            deref a;
            deref b;
        };
        casm_build_extend! {casm_builder,
            let orig_range_check = range_check;
            tempvar a_ge_b;
            tempvar a_minus_b = a - b;
            const u128_limit = BigInt::from(u128::MAX) + BigInt::from(1);
            const limit = limit;
            hint TestLessThan {lhs: a_minus_b, rhs: limit} into {dst: a_ge_b};
            jump NoOverflow if a_ge_b != 0;
            // Overflow (negative):
            // Here we know that 0 - (limit - 1) <= a - b < 0.
            tempvar fixed_a_minus_b = a_minus_b + u128_limit;
            assert fixed_a_minus_b = *(range_check++);
            let wrapping_a_minus_b = a_minus_b + limit;
            jump Overflow;
        NoOverflow:
            assert a_minus_b = *(range_check++);
        };

        Ok(Self {
            builder,
            casm_builder,
            orig_range_check,
            range_check,
            a,
            b,
            a_minus_b,
            wrapping_a_minus_b,
        })
    }

    /// Finalizes the libfunc builder.
    pub fn finalize(
        self,
        no_overflow_res: &[&[Var]],
        overflow_res: &[&[Var]],
    ) -> Result<CompiledInvocation, InvocationError> {
        let failure_handle_statement_id = get_non_fallthrough_statement_id(&self.builder);
        Ok(self.builder.build_from_casm_builder(
            self.casm_builder,
            [
                ("Fallthrough", no_overflow_res, None),
                ("Overflow", overflow_res, Some(failure_handle_statement_id)),
            ],
            CostValidationInfo {
                builtin_infos: vec![BuiltinInfo {
                    cost_token_ty: CostTokenType::RangeCheck,
                    start: self.orig_range_check,
                    end: self.range_check,
                }],
                extra_costs: None,
            },
        ))
    }
}

/// Handles a small integer diff operation.
/// The absolute distance between the inputs must be smaller than `limit`.
fn build_small_diff(
    builder: CompiledInvocationBuilder<'_>,
    limit: BigInt,
) -> Result<CompiledInvocation, InvocationError> {
    let data = SmallDiffHelper::new(builder, limit)?;
    let no_overflow_res: &[&[Var]] = &[&[data.range_check], &[data.a_minus_b]];
    let overflow_res: &[&[Var]] = &[&[data.range_check], &[data.wrapping_a_minus_b]];
    data.finalize(no_overflow_res, overflow_res)
}

/// Handles a 128 bit diff operation.
fn build_128bit_diff(
    builder: CompiledInvocationBuilder<'_>,
) -> Result<CompiledInvocation, InvocationError> {
    let failure_handle_statement_id = get_non_fallthrough_statement_id(&builder);
    let [range_check, a, b] = builder.try_get_single_cells()?;
    let mut casm_builder = CasmBuilder::default();
    add_input_variables! {casm_builder,
        buffer(0) range_check;
        deref a;
        deref b;
    };
    casm_build_extend! {casm_builder,
        let orig_range_check = range_check;
        tempvar a_ge_b;
        tempvar a_minus_b = a - b;
        const u128_limit = BigInt::from(u128::MAX) + BigInt::from(1);
        hint TestLessThan {lhs: a_minus_b, rhs: u128_limit} into {dst: a_ge_b};
        jump NoOverflow if a_ge_b != 0;
        // Overflow (negative):
        // Here we know that 0 - (2**128 - 1) <= a - b < 0.
        tempvar wrapping_a_minus_b = a_minus_b + u128_limit;
        assert wrapping_a_minus_b = *(range_check++);
        jump Target;
    NoOverflow:
        assert a_minus_b = *(range_check++);
    };
    Ok(builder.build_from_casm_builder(
        casm_builder,
        [
            ("Fallthrough", &[&[range_check], &[a_minus_b]], None),
            ("Target", &[&[range_check], &[wrapping_a_minus_b]], Some(failure_handle_statement_id)),
        ],
        CostValidationInfo {
            builtin_infos: vec![BuiltinInfo {
                cost_token_ty: CostTokenType::RangeCheck,
                start: orig_range_check,
                end: range_check,
            }],
            extra_costs: None,
        },
    ))
}
