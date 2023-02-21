use starknet_serde::ContractAddressSerde;

#[abi]
trait IProxy {
    fn initialize(world_address: starknet::ContractAddress);
}
