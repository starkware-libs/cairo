use option::OptionTrait;
use starknet::TxInfo;

// Interoperability.
extern fn call_contract_syscall(
    address: ContractAddress, entry_point_selector: felt, calldata: Array::<felt>
) -> SyscallResult::<Array::<felt>> implicits(GasBuiltin, System) nopanic;

// Events.
extern fn emit_event_syscall(
    keys: Array::<felt>, data: Array::<felt>
) -> SyscallResult::<()> implicits(GasBuiltin, System) nopanic;

// Getters.
extern fn get_caller_address_syscall() -> SyscallResult::<ContractAddress> implicits(
    GasBuiltin, System
) nopanic;

fn get_caller_address() -> ContractAddress {
    get_caller_address_syscall().unwrap_syscall()
}

extern fn get_contract_address_syscall() -> SyscallResult::<ContractAddress> implicits(
    GasBuiltin, System
) nopanic;

fn get_contract_address() -> ContractAddress {
    get_contract_address_syscall().unwrap_syscall()
}

extern fn get_sequencer_address_syscall() -> SyscallResult::<ContractAddress> implicits(
    GasBuiltin, System
) nopanic;

fn get_sequencer_address() -> ContractAddress {
    get_sequencer_address_syscall().unwrap_syscall()
}

extern fn get_block_number_syscall() -> SyscallResult::<u64> implicits(GasBuiltin, System) nopanic;

fn get_block_number() -> u64 {
    get_block_number_syscall().unwrap_syscall()
}

extern fn get_tx_info_syscall() -> SyscallResult::<Box::<TxInfo>> implicits(
    GasBuiltin, System
) nopanic;

fn get_tx_info() -> Box::<TxInfo> {
    get_tx_info_syscall().unwrap_syscall()
}

extern fn get_block_timestamp_syscall() -> SyscallResult::<u64> implicits(
    GasBuiltin, System
) nopanic;

// TODO(ilya): Consider Adding a type for timestamps.
fn get_block_timestamp() -> u64 {
    get_block_timestamp_syscall().unwrap_syscall()
}

/// The result type for a syscall.
type SyscallResult<T> = Result::<T, Array::<felt>>;

trait SyscallResultTrait<T> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with the revert reason.
    fn unwrap_syscall(self: SyscallResult::<T>) -> T;
}
impl SyscallResultTraitImpl<T> of SyscallResultTrait::<T> {
    fn unwrap_syscall(self: SyscallResult::<T>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(revert_reason) => {
                panic(revert_reason)
            },
        }
    }
}
