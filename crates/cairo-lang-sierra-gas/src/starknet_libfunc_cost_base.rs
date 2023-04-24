use cairo_lang_sierra::extensions::starknet::secp256k1::Secp256K1EcConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::StarkNetConcreteLibfunc;

use crate::objects::ConstCost;

const SYSTEM_CALL_STEPS: i32 = 100;
pub const SYSTEM_CALL_COST: i32 =
    ConstCost { steps: SYSTEM_CALL_STEPS, holes: 0, range_checks: 0 }.cost();

/// Returns some cost value for a StarkNet libfunc - a helper function to implement costing both for
/// creating gas equations and getting actual gas cost after having a solution.
pub fn starknet_libfunc_cost_base(libfunc: &StarkNetConcreteLibfunc) -> Vec<ConstCost> {
    let steps = |value| ConstCost { steps: value, ..Default::default() };
    match libfunc {
        StarkNetConcreteLibfunc::CallContract(_) => syscall_cost(9, 9),
        StarkNetConcreteLibfunc::ClassHashConst(_)
        | StarkNetConcreteLibfunc::ContractAddressConst(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::ClassHashTryFromFelt252(_)
        | StarkNetConcreteLibfunc::ContractAddressTryFromFelt252(_)
        | StarkNetConcreteLibfunc::StorageAddressTryFromFelt252(_) => {
            vec![
                ConstCost { steps: 7, holes: 0, range_checks: 3 },
                ConstCost { steps: 9, holes: 0, range_checks: 3 },
            ]
        }
        StarkNetConcreteLibfunc::ClassHashToFelt252(_)
        | StarkNetConcreteLibfunc::ContractAddressToFelt252(_)
        | StarkNetConcreteLibfunc::StorageAddressToFelt252(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::StorageRead(_) => syscall_cost(7, 7),
        StarkNetConcreteLibfunc::StorageWrite(_) => syscall_cost(8, 8),
        StarkNetConcreteLibfunc::StorageBaseAddressConst(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::StorageBaseAddressFromFelt252(_) => {
            vec![ConstCost { steps: 10, holes: 0, range_checks: 3 }]
        }
        StarkNetConcreteLibfunc::StorageAddressFromBase(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::EmitEvent(_) => syscall_cost(9, 9),
        StarkNetConcreteLibfunc::GetExecutionInfo(_) => syscall_cost(5, 5),
        StarkNetConcreteLibfunc::Deploy(_) => syscall_cost(10, 10),
        StarkNetConcreteLibfunc::Keccak(_) => syscall_cost(7, 7),
        StarkNetConcreteLibfunc::LibraryCall(_) => syscall_cost(9, 9),
        StarkNetConcreteLibfunc::ReplaceClass(_) => syscall_cost(6, 6),
        StarkNetConcreteLibfunc::SendMessageToL1(_) => syscall_cost(8, 8),
        StarkNetConcreteLibfunc::Testing(_) => vec![steps(1)],
        StarkNetConcreteLibfunc::Secp256K1(libfunc) => match libfunc {
            Secp256K1EcConcreteLibfunc::Add(_) => syscall_cost(7, 7),
            Secp256K1EcConcreteLibfunc::Mul(_) | Secp256K1EcConcreteLibfunc::GetPointFromX(_) => {
                syscall_cost(8, 8)
            }
        },
    }
}

/// Returns the costs for system calls.
fn syscall_cost(success: i32, failure: i32) -> Vec<ConstCost> {
    [success, failure]
        .map(|steps| ConstCost { steps: SYSTEM_CALL_STEPS + steps, holes: 0, range_checks: 0 })
        .to_vec()
}
