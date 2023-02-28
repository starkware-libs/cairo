use starknet::SyscallResultTrait;
use starknet::SyscallResult;

#[derive(Copy, Drop)]
struct ExecutionInfo {
    block_info: Box::<BlockInfo>,
    tx_info: Box::<TxInfo>,
    caller_address: ContractAddress,
    contract_address: ContractAddress,
}

#[derive(Copy, Drop)]
struct BlockInfo {
    block_number: u64,
    block_timestamp: u64,
    sequencer_address: ContractAddress,
}
impl BlockInfoBoxCopy of Copy::<Box::<BlockInfo>>;
impl BlockInfoBoxDrop of Drop::<Box::<BlockInfo>>;

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
impl TxInfoBoxCopy of Copy::<Box::<TxInfo>>;
impl TxInfoBoxDrop of Drop::<Box::<TxInfo>>;
