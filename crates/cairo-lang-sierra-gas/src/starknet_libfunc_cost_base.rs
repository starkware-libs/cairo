use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;

use crate::core_libfunc_cost_base::{ConstCost, CostOperations};

const SYSTEM_CALL_STEPS: i32 = 100;
pub const SYSTEM_CALL_COST: i32 =
    ConstCost { steps: SYSTEM_CALL_STEPS, holes: 0, range_checks: 0 }.cost();

/// Returns some cost value for a StarkNet libfunc - a helper function to implement costing both for
/// creating gas equations and getting actual gas usage after having a solution.
pub fn starknet_libfunc_cost_base<Ops: CostOperations>(
    ops: &mut Ops,
    libfunc: &StarkNetConcreteLibfunc,
) -> Vec<Ops::CostType> {
    match libfunc {
        StarkNetConcreteLibfunc::CallContract(_) => syscall_cost(ops, 8, 8),
        StarkNetConcreteLibfunc::ContractAddressConst(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::ContractAddressTryFromFelt(_) => {
            vec![
                ops.const_cost(ConstCost { steps: 7, holes: 0, range_checks: 3 }),
                ops.const_cost(ConstCost { steps: 9, holes: 0, range_checks: 3 }),
            ]
        }
        StarkNetConcreteLibfunc::ContractAddressToFelt(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::StorageRead(_) => syscall_cost(ops, 7, 7),
        StarkNetConcreteLibfunc::StorageWrite(_) => syscall_cost(ops, 8, 8),
        StarkNetConcreteLibfunc::StorageBaseAddressConst(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::StorageBaseAddressFromFelt(_) => {
            vec![ops.const_cost(ConstCost { steps: 10, holes: 0, range_checks: 3 })]
        }
        StarkNetConcreteLibfunc::StorageAddressFromBase(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![ops.steps(0)],
        StarkNetConcreteLibfunc::EmitEvent(_) => syscall_cost(ops, 9, 9),
        StarkNetConcreteLibfunc::GetCallerAddress(_)
        | StarkNetConcreteLibfunc::GetContractAddress(_)
        | StarkNetConcreteLibfunc::GetSequencerAddress(_)
        | StarkNetConcreteLibfunc::GetBlockNumber(_)
        | StarkNetConcreteLibfunc::GetBlockTimestamp(_) => syscall_cost(ops, 5, 5),
    }
}

/// Returns the costs for system calls.
fn syscall_cost<Ops: CostOperations>(
    ops: &mut Ops,
    success: i32,
    failure: i32,
) -> Vec<Ops::CostType> {
    [success, failure]
        .map(|steps| {
            ops.const_cost(ConstCost {
                steps: SYSTEM_CALL_STEPS + steps,
                holes: 0,
                range_checks: 0,
            })
        })
        .to_vec()
}
