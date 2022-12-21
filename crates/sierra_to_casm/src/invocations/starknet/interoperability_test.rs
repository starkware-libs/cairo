use casm::ap_change::ApChange;
use casm::{casm, deref, res};
use itertools::Itertools;
use pretty_assertions::assert_eq;
use sierra::program::StatementIdx;

use crate::invocations::test_utils::{
    compile_libfunc, ReducedBranchChanges, ReducedCompiledInvocation,
};
use crate::ref_expr;
use crate::references::{CellExpression, ReferenceExpression};
use crate::relocations::{Relocation, RelocationEntry};

#[test]
fn test_call_contract() {
    let gas_builtin = deref!([fp + 1]);
    let system = deref!([fp + 2]);
    let contract_address = deref!([fp + 3]);
    let call_data_start = deref!([fp + 5]);
    let call_data_end = deref!([fp + 6]);

    let call_data = ReferenceExpression {
        cells: vec![CellExpression::Deref(call_data_start), CellExpression::Deref(call_data_end)],
    };

    let mut args = [gas_builtin, system, contract_address]
        .into_iter()
        .map(|cell_expr| ReferenceExpression::from_cell(CellExpression::Deref(cell_expr)))
        .collect_vec();
    args.push(call_data);

    assert_eq!(
        compile_libfunc("call_contract_syscall", args),
        ReducedCompiledInvocation {
            instructions: casm! {
                [ap + 0] = 9221223673929037989702739452259u128, ap++;
                [ap + -1] = [[fp + 2] + 0];
                [&gas_builtin] = [[&system] + 1];
                [&contract_address] = [[&system] + 2];
                [&call_data_start] = [[&system] + 3];
                [&call_data_end] = [[&system] + 4];
                %{ syscall_handler.syscall(segments=segments, syscall_ptr=memory[fp + 2]) %}
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
                        ReferenceExpression {
                            cells: vec![
                                CellExpression::from_res_operand(res!([[fp + 2] + 7])),
                                CellExpression::from_res_operand(res!([[fp + 2] + 8]))
                            ]
                        }
                    ],
                    ap_change: ApChange::Known(2)
                },
                // Failure branch - return (gas builtin, system, revert_reason, result_array)
                ReducedBranchChanges {
                    refs: vec![
                        ref_expr!([[fp + 2] + 5]),
                        ref_expr!([fp + 2] + 9),
                        ref_expr!([ap - 1]),
                        ReferenceExpression {
                            cells: vec![
                                CellExpression::from_res_operand(res!([[fp + 2] + 7])),
                                CellExpression::from_res_operand(res!([[fp + 2] + 8]))
                            ]
                        },
                    ],
                    ap_change: ApChange::Known(2)
                }
            ]
        }
    );
}
