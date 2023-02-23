use option::OptionTrait;

extern type System;

// A Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {}

// Re-imports
// StorageAccess
mod storage_access;
use storage_access::StorageAccess;
use storage_access::StorageAddress;
use storage_access::StorageBaseAddress;
use storage_access::storage_base_address_const;
use storage_access::storage_base_address_from_felt;
use storage_access::storage_read_syscall;
use storage_access::storage_write_syscall;
use storage_access::storage_address_from_base;
use storage_access::storage_address_from_base_and_offset;

// ContractAddress
mod contract_address;
use contract_address::ContractAddress;
use contract_address::ContractAddressIntoFelt;
use contract_address::FeltTryIntoContractAddress;
use contract_address::contract_address_const;
use contract_address::contract_address_to_felt;
use contract_address::contract_address_try_from_felt;
use contract_address::ContractAddressZeroable;

// Syscalls
mod syscalls;
use syscalls::call_contract_syscall;
use syscalls::emit_event_syscall;
use syscalls::get_caller_address_syscall;
use syscalls::get_caller_address;
use syscalls::get_contract_address_syscall;
use syscalls::get_contract_address;
use syscalls::get_sequencer_address_syscall;
use syscalls::get_sequencer_address;
use syscalls::get_block_number_syscall;
use syscalls::get_block_number;
use syscalls::get_tx_info_syscall;
use syscalls::get_tx_info;
use syscalls::get_block_timestamp_syscall;
use syscalls::get_block_timestamp;
use syscalls::SyscallResult;
use syscalls::SyscallResultTrait;
use syscalls::SyscallResultTraitImpl;


#[derive(Copy, Drop)]
struct TxInfo {
    // The version of the transaction. It is fixed (currently, 1) in the OS, and should be
    // signed by the account contract.
    // This field allows invalidating old transactions, whenever the meaning of the other
    // transaction fields is changed (in the OS).
    version: felt,
    // The account contract from which this transaction originates.
    account_contract_address: ContractAddress,
    // The max_fee field of the transaction.
    max_fee: u128,
    // The signature of the transaction.
    signature: Span::<felt>,
    // The hash of the transaction.
    transaction_hash: felt,
    // The identifier of the chain.
    // This field can be used to prevent replay of testnet transactions on mainnet.
    chain_id: felt,
    // The transaction's nonce.
    nonce: felt,
}
