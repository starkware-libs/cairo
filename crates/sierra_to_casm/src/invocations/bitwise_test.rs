use casm::ap_change::ApChange;
use casm::hints::Hint;
use casm::{casm, casm_extend, res};
use pretty_assertions::assert_eq;
use test_log::test;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;

#[test]
fn test_bitwise() {
    let mut expected_casm = casm! {
        [fp + 2] = [[fp + 1] + 32763];
        [ap + 5] = [[fp + 1] + 32764];
    };
    expected_casm.current_hints.push(Hint::Bitwise { ptr: res!([fp + 1] + 32763) });
    casm_extend!(expected_casm, ap+=0;);
    assert_eq!(
        compile_libfunc(
            "bitwise",
            vec![ref_expr!([fp + 1] + (i16::MAX - 4)), ref_expr!([fp + 2]), ref_expr!([ap + 5])]
        ),
        ReducedCompiledInvocation {
            instructions: expected_casm.instructions,
            relocations: vec![],
            results: vec![ReducedBranchChanges {
                refs: vec![
                    ref_expr!([fp + 1] + 32768),
                    ref_expr!([[fp + 1] + 32765]),
                    ref_expr!([[fp + 1] + 32766]),
                    ref_expr!([[fp + 1] + 32767])
                ],
                ap_change: ApChange::Known(0)
            }]
        }
    );
}
