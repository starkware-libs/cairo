use casm::ap_change::ApChange;
use casm::casm;
use num_bigint::BigInt;
use pretty_assertions::assert_eq;
use sierra::program::StatementIdx;
use test_log::test;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;
use crate::relocations::{Relocation, RelocationEntry};

#[test]
fn test_add() {
    assert_eq!(
        compile_libfunc(
            "uint128_checked_add",
            vec![ref_expr!([fp + 2]), ref_expr!([fp + 1]), ref_expr!([ap - 7])]
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = [fp + 1] + [ap - 7], ap++;
                %{ memory[ap + 0] = memory [ap - 1] < (BigInt::from(2).pow(128)) %}
                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap - 2] + (-BigInt::from(2).pow(128)), ap++;
                [ap - 1] = [[fp + 2]];
                jmp rel 0;
                [ap - 2] = [[fp + 2]];
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 4,
                relocation: Relocation::RelativeStatementId(StatementIdx(1))
            }],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![ref_expr!([fp + 2] + 1), ref_expr!([ap - 2])],
                    ap_change: ApChange::Known(2)
                },
                ReducedBranchChanges {
                    refs: vec![ref_expr!([fp + 2] + 1)],
                    ap_change: ApChange::Known(3)
                }
            ]
        }
    );
}

#[test]
fn test_sub() {
    assert_eq!(
        compile_libfunc(
            "uint128_checked_sub",
            vec![ref_expr!([ap - 2]), ref_expr!([ap - 1]), ref_expr!([fp + 7])]
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap - 1] = [ap + 0] + [fp + 7], ap++;
                %{ memory[ap + 0] = memory [ap - 1] < (BigInt::from(2).pow(128)) %}
                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap - 2] + (BigInt::from(2).pow(128)), ap++;
                [ap - 1] = [[ap - 5]];
                jmp rel 0;
                [ap - 2] = [[ap - 4]];
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 4,
                relocation: Relocation::RelativeStatementId(StatementIdx(1))
            }],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![ref_expr!([ap - 4] + 1), ref_expr!([ap - 2])],
                    ap_change: ApChange::Known(2)
                },
                ReducedBranchChanges {
                    refs: vec![ref_expr!([ap - 5] + 1)],
                    ap_change: ApChange::Known(3)
                }
            ]
        }
    );
}

#[test]
fn test_lt() {
    assert_eq!(
        compile_libfunc(
            "uint128_lt",
            vec![ref_expr!([fp - 5]), ref_expr!([ap - 7]), ref_expr!([ap - 6])]
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                %{ memory[ap + 0] = memory[ap - 6] <= memory[ap - 7] %}
                jmp rel 8 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -8] + 1, ap++;
                [ap + -8] = [ap + 0] + [ap + -1], ap++;
                [ap - 1] = [[fp - 5]];
                jmp rel 0;
                [ap + -8] = [ap + 0] + [ap + -7], ap++;
                [ap - 1] = [[fp + -5]];
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 4,
                relocation: Relocation::RelativeStatementId(StatementIdx(1))
            }],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![ref_expr!([fp - 5] + 1)],
                    ap_change: ApChange::Known(2)
                },
                ReducedBranchChanges {
                    refs: vec![ref_expr!([fp - 5] + 1)],
                    ap_change: ApChange::Known(3)
                }
            ]
        }
    );
}

#[test]
fn test_le() {
    assert_eq!(
        compile_libfunc(
            "uint128_le",
            vec![ref_expr!([fp - 5]), ref_expr!([ap - 7]), ref_expr!([ap - 6])]
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                %{ memory[ap + 0] = memory[ap - 6] < memory[ap - 7] %}
                jmp rel 6 if [ap + 0] != 0, ap++;
                [ap + -7] = [ap + 0] + [ap + -8], ap++;
                [ap - 1] = [[fp + -5]];
                jmp rel 0;
                [ap + 0] = [ap + -7] + 1, ap++;
                [ap + -9] = [ap + 0] + [ap + -1], ap++;
                [ap - 1] = [[fp - 5]];
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 3,
                relocation: Relocation::RelativeStatementId(StatementIdx(1))
            }],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![ref_expr!([fp - 5] + 1)],
                    ap_change: ApChange::Known(3)
                },
                ReducedBranchChanges {
                    refs: vec![ref_expr!([fp - 5] + 1)],
                    ap_change: ApChange::Known(2)
                }
            ]
        }
    );
}
