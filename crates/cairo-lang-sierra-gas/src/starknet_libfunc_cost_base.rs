use std::vec;

use cairo_lang_sierra::extensions::starknet::StarknetConcreteLibfunc;
use cairo_lang_sierra::extensions::starknet::secp256::{
    Secp256ConcreteLibfunc, Secp256OpConcreteLibfunc,
};
use cairo_lang_sierra::extensions::starknet::testing::TestingConcreteLibfunc;

use crate::objects::ConstCost;

const SYSTEM_CALL_STEPS: i32 = 100;
pub const SYSTEM_CALL_COST: i32 =
    ConstCost { steps: SYSTEM_CALL_STEPS, holes: 0, range_checks: 0, range_checks96: 0 }.cost();

/// Returns some cost value for a Starknet libfunc - a helper function to implement costing both for
/// creating gas equations and getting actual gas cost after having a solution.
pub fn starknet_libfunc_cost_base(libfunc: &StarknetConcreteLibfunc) -> Vec<ConstCost> {
    let steps = |value| ConstCost { steps: value, ..Default::default() };
    match libfunc {
        StarknetConcreteLibfunc::CallContract(_) => syscall_cost(4),
        StarknetConcreteLibfunc::ClassHashConst(_)
        | StarknetConcreteLibfunc::ContractAddressConst(_) => vec![steps(0)],
        StarknetConcreteLibfunc::ClassHashTryFromFelt252(_)
        | StarknetConcreteLibfunc::ContractAddressTryFromFelt252(_)
        | StarknetConcreteLibfunc::StorageAddressTryFromFelt252(_) => {
            vec![
                ConstCost { steps: 7, holes: 0, range_checks: 3, range_checks96: 0 },
                ConstCost { steps: 9, holes: 0, range_checks: 3, range_checks96: 0 },
            ]
        }
        StarknetConcreteLibfunc::ClassHashToFelt252(_)
        | StarknetConcreteLibfunc::ContractAddressToFelt252(_)
        | StarknetConcreteLibfunc::StorageAddressToFelt252(_) => vec![steps(0)],
        StarknetConcreteLibfunc::StorageRead(_) => syscall_cost(2),
        StarknetConcreteLibfunc::StorageWrite(_) => syscall_cost(3),
        StarknetConcreteLibfunc::StorageBaseAddressConst(_) => vec![steps(0)],
        StarknetConcreteLibfunc::StorageBaseAddressFromFelt252(_) => {
            vec![ConstCost { steps: 10, holes: 0, range_checks: 3, range_checks96: 0 }]
        }
        StarknetConcreteLibfunc::StorageAddressFromBase(_) => vec![steps(0)],
        StarknetConcreteLibfunc::StorageAddressFromBaseAndOffset(_) => vec![steps(0)],
        StarknetConcreteLibfunc::EmitEvent(_) => syscall_cost(4),
        StarknetConcreteLibfunc::GetBlockHash(_) => syscall_cost(1),
        StarknetConcreteLibfunc::GetExecutionInfo(_)
        | StarknetConcreteLibfunc::GetExecutionInfoV2(_) => syscall_cost(0),
        StarknetConcreteLibfunc::Deploy(_) => syscall_cost(5),
        StarknetConcreteLibfunc::Keccak(_) => syscall_cost(2),
        StarknetConcreteLibfunc::Sha256ProcessBlock(_) => syscall_cost(2),
        StarknetConcreteLibfunc::Sha256StateHandleInit(_) => vec![steps(0)],
        StarknetConcreteLibfunc::Sha256StateHandleDigest(_) => vec![steps(0)],
        StarknetConcreteLibfunc::LibraryCall(_) => syscall_cost(4),
        StarknetConcreteLibfunc::ReplaceClass(_) => syscall_cost(1),
        StarknetConcreteLibfunc::SendMessageToL1(_) => syscall_cost(3),
        StarknetConcreteLibfunc::Testing(libfunc) => match libfunc {
            TestingConcreteLibfunc::Cheatcode(_) => vec![steps(1)],
        },
        StarknetConcreteLibfunc::Secp256(libfunc) => {
            match libfunc {
                Secp256ConcreteLibfunc::K1(libfunc) => match libfunc {
                    Secp256OpConcreteLibfunc::New(_) => syscall_cost(4),
                    Secp256OpConcreteLibfunc::Add(_) => syscall_cost(2),
                    Secp256OpConcreteLibfunc::Mul(_)
                    | Secp256OpConcreteLibfunc::GetPointFromX(_) => syscall_cost(3),
                    Secp256OpConcreteLibfunc::GetXy(_) => syscall_cost(1),
                },
                Secp256ConcreteLibfunc::R1(libfunc) => match libfunc {
                    Secp256OpConcreteLibfunc::New(_) => syscall_cost(4),
                    Secp256OpConcreteLibfunc::Add(_) => syscall_cost(2),
                    Secp256OpConcreteLibfunc::Mul(_)
                    | Secp256OpConcreteLibfunc::GetPointFromX(_) => syscall_cost(3),
                    Secp256OpConcreteLibfunc::GetXy(_) => syscall_cost(1),
                },
            }
        }
        StarknetConcreteLibfunc::GetClassHashAt(_) => syscall_cost(1),
        StarknetConcreteLibfunc::MetaTxV0(_) => syscall_cost(6),
    }
}

/// Returns the costs for system calls.
fn syscall_cost(arg_size: i32) -> Vec<ConstCost> {
    let cost = ConstCost {
        steps: SYSTEM_CALL_STEPS + 5 + arg_size,
        holes: 0,
        range_checks: 0,
        range_checks96: 0,
    };
    vec![cost, cost]
}
