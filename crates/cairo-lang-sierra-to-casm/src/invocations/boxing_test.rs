use cairo_lang_casm::ap_change::ApChange;
use cairo_lang_casm::casm;
use cairo_lang_test_utils::test;

use crate::invocations::test_utils::{
    ReducedBranchChanges, ReducedCompiledInvocation, compile_libfunc,
};
use crate::ref_expr;

#[test]
fn test_into_repr_ptr() {
    assert_eq!(
        compile_libfunc("into_repr_ptr<felt252>", vec![ref_expr!([fp + 5])]),
        ReducedCompiledInvocation {
            instructions: casm! {
                call rel 2;
                jmp rel 2;
                ret;
                [ap + 0] = [ap + -2] + 5, ap++;
            }
            .instructions,
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![ref_expr!([ap - 1])],
                ap_change: ApChange::Known(1)
            }]
        }
    );
}
