use casm::ap_change::ApChange;
use casm::casm;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;

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
