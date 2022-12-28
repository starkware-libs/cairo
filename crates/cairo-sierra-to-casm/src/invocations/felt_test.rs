use casm::ap_change::ApChange;
use casm::casm;
use sierra::program::StatementIdx;
use test_log::test;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;
use crate::relocations::{Relocation, RelocationEntry};

#[test]
fn test_felt_add() {
    assert_eq!(
        compile_libfunc("felt_add", vec![ref_expr!([fp + 5]), ref_expr!([ap + 5])]),
        ReducedCompiledInvocation {
            instructions: vec![],
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![ref_expr!([fp + 5] + [ap + 5])],
                ap_change: ApChange::Known(0)
            }]
        }
    );
}

#[test]
fn test_store_temp() {
    assert_eq!(
        compile_libfunc("store_temp<felt>", vec![ref_expr!([fp + 5] + [ap + 5])]),
        ReducedCompiledInvocation {
            instructions: casm! {[ap + 0] = [fp + 5] + [ap + 5], ap++;}.instructions,
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![ref_expr!([ap - 1])],
                ap_change: ApChange::Known(1)
            }]
        }
    );
}

#[test]
fn test_jump_nz() {
    assert_eq!(
        compile_libfunc("felt_jump_nz", vec![ref_expr!([ap - 5])]),
        ReducedCompiledInvocation {
            instructions: casm! {jmp rel 0 if [ap - 5] != 0;}.instructions,
            relocations: vec![RelocationEntry {
                instruction_idx: 0,
                relocation: Relocation::RelativeStatementId(StatementIdx(1))
            }],
            results: vec![
                ReducedBranchChanges { refs: vec![], ap_change: ApChange::Known(0) },
                ReducedBranchChanges {
                    refs: vec![ref_expr!([ap - 5])],
                    ap_change: ApChange::Known(0)
                }
            ]
        }
    );
}
