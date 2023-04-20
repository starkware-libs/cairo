#[abi]
trait IProxy {
    fn initialize(world_address: starknet::ContractAddress);
}
