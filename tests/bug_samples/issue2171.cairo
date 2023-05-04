use traits::Into;
use starknet::ContractAddress;

fn foo(contract_address: ContractAddress) {
    assert(
        Into::<ContractAddress,
        felt252>::into(
            contract_address
        ) == Into::<ContractAddress, felt252>::into(contract_address),
        'Some message'
    );
}
