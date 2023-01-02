use cairo_casm::ap_change::ApChange;
use cairo_casm::casm;
use cairo_sierra::program::StatementIdx;
use pretty_assertions::assert_eq;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;
use crate::relocations::{Relocation, RelocationEntry};

// TODO(yuval): move tests to test infrastructure.

#[test]
fn test_storage_read() {
    assert_eq!(
        compile_libfunc(
            "storage_read_syscall",
            vec![
                // gas_builtin.
                ref_expr!([fp + 0]),
                // Syscall ptr.
                ref_expr!([fp + 1] + 3),
                // address domain.
                ref_expr!([ap + 3]),
                // address.
                ref_expr!([ap + 5]),
            ],
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = 100890693370601760042082660u128, ap++;
                [ap + -1] = [[fp + 1] + 3];
                [fp + 0] = [[fp + 1] + 4];
                [ap + 2] = [[fp + 1] + 5];
                [ap + 4] = [[fp + 1] + 6];
                %{ syscall_handler.syscall(syscall_ptr=memory[fp + 1] + 3) %}
                [ap + 0] = [[fp + 1] + 8], ap++;
                jmp rel 0 if [ap + -1] != 0;
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 6,
                relocation: Relocation::RelativeStatementId(StatementIdx(1,),),
            },],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 1] + 7]),
                        ref_expr!([fp + 1] + 10),
                        ref_expr!([[fp + 1] + 9]),
                    ],
                    ap_change: ApChange::Known(2)
                },
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 1] + 7]),
                        ref_expr!([fp + 1] + 10),
                        ref_expr!([ap - 1]),
                    ],
                    ap_change: ApChange::Known(2)
                }
            ]
        }
    );
}

#[test]
fn test_storage_write() {
    assert_eq!(
        compile_libfunc(
            "storage_write_syscall",
            vec![
                // gas_builtin.
                ref_expr!([fp + 1]),
                // Syscall ptr.
                ref_expr!([fp + 2]),
                // address domain.
                ref_expr!([ap + 3]),
                // address.
                ref_expr!([ap + 5]),
                // value.
                ref_expr!([ap + 6])
            ],
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = 25828017502874050592466629733u128, ap++;
                [ap + -1] = [[fp + 2] + 0];
                [fp + 1] = [[fp + 2] + 1];
                [ap + 2] = [[fp + 2] + 2];
                [ap + 4] = [[fp + 2] + 3];
                [ap + 5] = [[fp + 2] + 4];
                %{ syscall_handler.syscall(syscall_ptr=memory[fp + 2]) %}
                [ap + 0] = [[fp + 2] + 6], ap++;
                jmp rel 0 if [ap + -1] != 0;
            }
            .instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 7,
                relocation: Relocation::RelativeStatementId(StatementIdx(1,),),
            },],
            results: vec![
                ReducedBranchChanges {
                    refs: vec![ref_expr!([[fp + 2] + 5]), ref_expr!([fp + 2] + 7)],
                    ap_change: ApChange::Known(2)
                },
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 2] + 5]),
                        ref_expr!([fp + 2] + 7),
                        ref_expr!([ap - 1])
                    ],
                    ap_change: ApChange::Known(2)
                }
            ]
        }
    );
}
