use traits::Into;
use starknet::ContractAddressIntoFelt252;
use starknet::ContractAddress;

extern type OutputBuiltin;

extern fn output_felt252(a: felt252) implicits(OutputBuiltin) nopanic;
