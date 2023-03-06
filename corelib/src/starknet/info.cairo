use starknet::SyscallResultTrait;
use starknet::SyscallResult;
use starknet::syscalls::get_execution_info_syscall;

#[derive(Copy, Drop)]
struct ExecutionInfo {
    block_info: Box<BlockInfo>,
    tx_info: Box<TxInfo>,
    caller_address: ContractAddress,
    contract_address: ContractAddress,
    entry_point_selector: felt,
}

#[derive(Copy, Drop)]
struct BlockInfo {
    block_number: u64,
    block_timestamp: u64,
    sequencer_address: ContractAddress,
}
impl BlockInfoBoxCopy of Copy::<Box<BlockInfo>>;
impl BlockInfoBoxDrop of Drop::<Box<BlockInfo>>;

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
    signature: Span<felt>,
    // The hash of the transaction.
    transaction_hash: felt,
    // The identifier of the chain.
    // This field can be used to prevent replay of testnet transactions on mainnet.
    chain_id: felt,
    // The transaction's nonce.
    nonce: felt,
}
impl TxInfoBoxCopy of Copy::<Box<TxInfo>>;
impl TxInfoBoxDrop of Drop::<Box<TxInfo>>;

fn get_execution_info() -> Box<ExecutionInfo> {
    get_execution_info_syscall().unwrap_syscall()
}

fn get_caller_address() -> ContractAddress {
    unbox(get_execution_info()).caller_address
}

fn get_contract_address() -> ContractAddress {
    unbox(get_execution_info()).contract_address
}

fn get_block_info() -> Box<BlockInfo> {
    unbox(get_execution_info()).block_info
}

fn get_tx_info() -> Box<TxInfo> {
    unbox(get_execution_info()).tx_info
}
