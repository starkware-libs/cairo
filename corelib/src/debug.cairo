use array::ArrayTrait;
use starknet::ContractAddress;
use traits::Into;
use starknet::ContractAddressIntoFelt;

extern fn print(message: Array::<felt>) nopanic;

fn print_felt(message: felt) {
    let mut arr = ArrayTrait::new();
    arr.append(message);
    print(arr);
}

fn print_contract_address(message: ContractAddress) {
    print_felt(message.into())
}
