use traits::Into;
use starknet::ContractAddressIntoFelt252;

fn foo(contract_address: starknet::ContractAddress) {
    assert(contract_address.into() == contract_address.into(), 'Some message');
}
