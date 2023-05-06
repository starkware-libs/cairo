#[starknet::interface]
trait IProxy<T> {
    fn initialize(ref self: T, world_address: starknet::ContractAddress);
}
