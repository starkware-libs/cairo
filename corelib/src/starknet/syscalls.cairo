use starknet::SyscallResult;
use starknet::storage_access::StorageAddress;
use starknet::class_hash::ClassHash;

// Calls a given contract.
// `address` - The address of the called contract.
// `entry_point_selector` - A selector for a function within that contract.
// `calldata` - Call arguments.
extern fn call_contract_syscall(
    address: ContractAddress, entry_point_selector: felt252, calldata: Array<felt252>
) -> SyscallResult<Array<felt252>> implicits(GasBuiltin, System) nopanic;

// Deploys a new instance of a previously declared class.
// `class_hash` - The class hash of the contract to be deployed.
// `contract_address_salt` - The salt, an arbitrary value provided by the sender, used in the
//     computation of the contract's address.
// `calldata` - Call arguments for the constructor.
extern fn deploy_syscall(
    class_hash: ClassHash, contract_address_salt: felt252, calldata: Array<felt252>
) -> SyscallResult<ContractAddress> implicits(GasBuiltin, System) nopanic;

// Emits an event.
// `keys` - The keys of the event.
// `data` - The data of the event.
extern fn emit_event_syscall(
    keys: Array<felt252>, data: Array<felt252>
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

// Gets information about the current execution.
extern fn get_execution_info_syscall() -> SyscallResult<Box<starknet::info::ExecutionInfo>> implicits(
    GasBuiltin, System
) nopanic;

// Calls the requested function in any previously declared class.
// `class_hash` - The hash of the class you want to use.
// `function_selector` - A selector for a function within that class.
// `calldata` - Call arguments.
extern fn library_call_syscall(
    class_hash: ClassHash, function_selector: felt252, calldata: Array<felt252>
) -> SyscallResult<Array<felt252>> implicits(GasBuiltin, System) nopanic;

// Calls the requested L1 handler in any previously declared class.
// `class_hash` - The hash of the class you want to use.
// `function_selector` - A selector for an L1 handler function within that class.
// `calldata` - Call arguments.
extern fn library_call_l1_handler_syscall(
    class_hash: ClassHash, function_selector: felt252, calldata: Array<felt252>
) -> SyscallResult<Array<felt252>> implicits(GasBuiltin, System) nopanic;

// TODO(Ilya): Decide if we limit the type of `to_address`.
// Sends a message to L1.
// `to_address` - The recipient's L1 address.
// `payload` - The content of the message.
extern fn send_message_to_l1_syscall(
    to_address: felt252, payload: Array<felt252>
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

// Gets the value of a key in the storage of the calling contract.
// `address_domain` - The domain of the address. Only address_domain 0 is currently supported,
//     in the future it will enable access to address spaces with different data availability
//     guarantees.
// `address` - The address of the storage key to read.
extern fn storage_read_syscall(
    address_domain: felt252, address: StorageAddress, 
) -> SyscallResult<felt252> implicits(GasBuiltin, System) nopanic;

// Sets the value of a key in the storage of the calling contract.
// `address_domain` - The domain of the address. Only address_domain 0 is currently supported,
//     in the future it will enable access to address spaces with different data availability
//     guarantees.
// `address` - The address of the storage key to write.
// `value` - The value to write to the key.
extern fn storage_write_syscall(
    address_domain: felt252, address: StorageAddress, value: felt252
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;


// Replaces the class hash of the current contract.
// `class_hash` - The class hash that should replace the current one.
extern fn replace_class_syscall(
    class_hash: ClassHash
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;
