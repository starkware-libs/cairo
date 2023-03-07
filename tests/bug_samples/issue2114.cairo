use starknet::contract_address::ContractAddressSerde;

#[abi]
trait IProxy {
    fn initialize(world_address: starknet::ContractAddress);
}
