use casm::ap_change::ApChange;
use casm::casm;
use pretty_assertions::assert_eq;
use sierra::program::StatementIdx;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;
use crate::relocations::{Relocation, RelocationEntry};

// TODO(yuval): move tests to test infrastructure.

#[test]
fn test_storage_read() {
    assert_eq!(
        compile_libfunc("storage_read_syscall", vec![ref_expr!([fp + 1] + 3), ref_expr!([ap + 5])],),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = 31066245855454734213960397939u128, ap++;
                [ap + -1] = [[fp + 1] + 3];
                [ap + 4] = [[fp + 1] + 4];
                %{ syscall_handler.syscall(segments=segments, syscall_ptr=memory[fp + 1] + 3) %}
                [ap + 0] = [[fp + 1] + 5], ap++;
            }
            .instructions,
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![ref_expr!([fp + 1] + 6), ref_expr!([ap - 1])],
                ap_change: ApChange::Known(2)
            }]
        }
    );
}

#[test]
fn test_storage_write() {
    assert_eq!(
        compile_libfunc(
            "storage_write_syscall",
            vec![
                ref_expr!([fp + 1]),
                ref_expr!([fp + 2]),
                ref_expr!([ap + 5]),
                ref_expr!([ap + 6])
            ],
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = 8038072152842849266968829064307u128, ap++;
                [ap + -1] = [[fp + 2] + 0];
                [fp + 1] = [[fp + 2] + 1];
                [ap + 4] = [[fp + 2] + 2];
                [ap + 5] = [[fp + 2] + 3];
                %{ syscall_handler.syscall(segments=segments, syscall_ptr=memory[fp + 2]) %}
                [ap + 0] = [[fp + 2] + 5], ap++;
                jmp rel 0 if [ap + -1] != 0;
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 6,
                relocation: Relocation::RelativeStatementId(StatementIdx(1,),),
            },],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![ref_expr!([[fp + 2] + 4]), ref_expr!([fp + 2] + 7)],
                    ap_change: ApChange::Known(2)
                },
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 2] + 4]),
                        ref_expr!([fp + 2] + 7),
                        ref_expr!([ap - 1])
                    ],
                    ap_change: ApChange::Known(2)
                }
            ]
        }
    );
}
