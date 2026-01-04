// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/utils.cairo)

pub mod secp256_point;
pub mod signature;

pub use signature::{is_valid_eth_signature, is_valid_p256_signature, is_valid_stark_signature};
use starknet::SyscallResultTrait;
use starknet::account::Call;

pub const MIN_TRANSACTION_VERSION: u256 = 1;
pub const QUERY_OFFSET: u256 = 0x100000000000000000000000000000000;
// QUERY_OFFSET + TRANSACTION_VERSION
pub const QUERY_VERSION: u256 = 0x100000000000000000000000000000001;

/// Executes a list of calls and returns the return values.
pub fn execute_calls(calls: Span<Call>) -> Array<Span<felt252>> {
    let mut res = array![];
    for call in calls {
        res.append(execute_single_call(call));
    }
    res
}

/// Executes a single call and returns the return value.
pub fn execute_single_call(call: @Call) -> Span<felt252> {
    let Call { to, selector, calldata } = *call;
    starknet::syscalls::call_contract_syscall(to, selector, calldata).unwrap_syscall()
}

/// If the transaction is a simulation (version >= `QUERY_OFFSET`), it must be
/// greater than or equal to `QUERY_OFFSET` + `MIN_TRANSACTION_VERSION` to be considered valid.
/// Otherwise, it must be greater than or equal to `MIN_TRANSACTION_VERSION`.
pub fn is_tx_version_valid() -> bool {
    let tx_info = starknet::get_tx_info().unbox();
    let tx_version = tx_info.version.into();
    if tx_version >= QUERY_OFFSET {
        QUERY_OFFSET + MIN_TRANSACTION_VERSION <= tx_version
    } else {
        MIN_TRANSACTION_VERSION <= tx_version
    }
}
