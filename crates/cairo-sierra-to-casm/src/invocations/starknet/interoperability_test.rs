use cairo_casm::ap_change::ApChange;
use cairo_casm::casm;
use cairo_sierra::program::StatementIdx;
use pretty_assertions::assert_eq;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;
use crate::relocations::{Relocation, RelocationEntry};

#[test]
fn test_call_contract() {
    let gas_builtin = ref_expr!([fp + 1]);
    let system = ref_expr!([fp + 2]);
    let contract_address = ref_expr!([fp + 3]);
    let call_data = ref_expr!([fp + 5], [fp + 6]);

    assert_eq!(
        compile_libfunc(
            "call_contract_syscall",
            vec![gas_builtin, system, contract_address, call_data]
        ),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = 9221223673929037989702739452259u128, ap++;
                [ap + -1] = [[fp + 2] + 0];
                [fp + 1] = [[fp + 2] + 1];
                [fp + 3] = [[fp + 2] + 2];
                [fp + 5] = [[fp + 2] + 3];
                [fp + 6] = [[fp + 2] + 4];
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
                // Success branch - return (gas builtin, system, result_array)
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 2] + 5]),
                        ref_expr!([fp + 2] + 9),
                        ref_expr!([[fp + 2] + 7], [[fp + 2] + 8]),
                    ],
                    ap_change: ApChange::Known(2)
                },
                // Failure branch - return (gas builtin, system, revert_reason, result_array)
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 2] + 5]),
                        ref_expr!([fp + 2] + 9),
                        ref_expr!([ap - 1]),
                        ref_expr!([[fp + 2] + 7], [[fp + 2] + 8]),
                    ],
                    ap_change: ApChange::Known(2)
                }
            ]
        }
    );
}
