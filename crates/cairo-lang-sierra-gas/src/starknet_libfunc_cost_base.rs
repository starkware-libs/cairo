use std::vec;

use cairo_lang_sierra::extensions::starknet::secp256::{
    Secp256EcConcreteLibfunc, Secp256EcOpConcreteLibfunc,
};
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
        StarkNetConcreteLibfunc::CallContract(_) => syscall_cost(4),
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
        StarkNetConcreteLibfunc::StorageRead(_) => syscall_cost(2),
        StarkNetConcreteLibfunc::StorageWrite(_) => syscall_cost(3),
        StarkNetConcreteLibfunc::StorageBaseAddressConst(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::StorageBaseAddressFromFelt252(_) => {
            vec![ConstCost { steps: 10, holes: 0, range_checks: 3 }]
        }
        StarkNetConcreteLibfunc::StorageAddressFromBase(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![steps(0)],
        StarkNetConcreteLibfunc::EmitEvent(_) => syscall_cost(4),
        StarkNetConcreteLibfunc::GetBlockHash(_) => syscall_cost(1),
        StarkNetConcreteLibfunc::GetExecutionInfo(_) => syscall_cost(0),
        StarkNetConcreteLibfunc::Deploy(_) => syscall_cost(5),
        StarkNetConcreteLibfunc::Keccak(_) => syscall_cost(2),
        StarkNetConcreteLibfunc::LibraryCall(_) => syscall_cost(4),
        StarkNetConcreteLibfunc::ReplaceClass(_) => syscall_cost(1),
        StarkNetConcreteLibfunc::SendMessageToL1(_) => syscall_cost(3),
        StarkNetConcreteLibfunc::Testing(_) => vec![steps(1)],
        StarkNetConcreteLibfunc::Secp256(libfunc) => match libfunc {
            Secp256EcConcreteLibfunc::K1(libfunc) => match libfunc {
                Secp256EcOpConcreteLibfunc::New(_) => syscall_cost(4),
                Secp256EcOpConcreteLibfunc::Add(_) => syscall_cost(2),
                Secp256EcOpConcreteLibfunc::Mul(_)
                | Secp256EcOpConcreteLibfunc::GetPointFromX(_) => syscall_cost(3),
                Secp256EcOpConcreteLibfunc::GetCoordinates(_) => syscall_cost(1),
            },
            Secp256EcConcreteLibfunc::R1(libfunc) => match libfunc {
                Secp256EcOpConcreteLibfunc::New(_) => syscall_cost(4),
                Secp256EcOpConcreteLibfunc::Add(_) => syscall_cost(2),
                Secp256EcOpConcreteLibfunc::Mul(_)
                | Secp256EcOpConcreteLibfunc::GetPointFromX(_) => syscall_cost(3),
                Secp256EcOpConcreteLibfunc::GetCoordinates(_) => syscall_cost(1),
            },
        },
    }
}

/// Returns the costs for system calls.
fn syscall_cost(arg_count: i32) -> Vec<ConstCost> {
    let cost = ConstCost { steps: SYSTEM_CALL_STEPS + 5 + arg_count, holes: 0, range_checks: 0 };
    vec![cost.clone(), cost]
}
