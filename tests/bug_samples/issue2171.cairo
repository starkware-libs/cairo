use traits::Into;
use starknet::ContractAddressIntoFelt;

fn foo(contract_address: ContractAddress) {
    assert(contract_address.into() == contract_address.into(), 'Some message');
}
