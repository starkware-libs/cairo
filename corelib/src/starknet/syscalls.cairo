use starknet::SyscallResult;
use starknet::storage_access::StorageAddress;

// Calls a given contract.
// `address` - The address of the called contract.
// `entry_point_selector` - A selector for a function within that contract.
// `calldata` - Call arguments.
extern fn call_contract_syscall(
    address: ContractAddress, entry_point_selector: felt, calldata: Array<felt>
) -> SyscallResult<Array<felt>> implicits(GasBuiltin, System) nopanic;

// Emits an event.
// `keys` - The keys of the event.
// `data` - The data of the event.
extern fn emit_event_syscall(
    keys: Array<felt>, data: Array<felt>
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

// Gets information about the current execution.
extern fn get_execution_info_syscall() -> SyscallResult<Box<starknet::info::ExecutionInfo>> implicits(
    GasBuiltin, System
) nopanic;

// Gets the value of a key in the storage of the calling contract.
// `address_domain` - The domain of the address. Only address_domain 0 is currently supported,
//     in the future it will enable access to address spaces with different data availability
//     guarantees.
// `address` - The address of the storage key to read.
extern fn storage_read_syscall(
    address_domain: felt, address: StorageAddress, 
) -> SyscallResult<felt> implicits(GasBuiltin, System) nopanic;

// Sets the value of a key in the storage of the calling contract.
// `address_domain` - The domain of the address. Only address_domain 0 is currently supported,
//     in the future it will enable access to address spaces with different data availability
//     guarantees.
// `address` - The address of the storage key to write.
// `value` - The value to write to the key.
extern fn storage_write_syscall(
    address_domain: felt, address: StorageAddress, value: felt
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;
