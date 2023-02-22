#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[account_contract]
mod Account {
    use array::SpanTrait;
    use ecdsa::check_ecdsa_signature;
    use starknet::ContractAddress;

    struct Storage {
        public_key: felt
    }

    fn validate_transaction() -> felt {
        let tx_info = unbox(starknet::get_tx_info());
        let signature = tx_info.signature;
        assert(signature.len() == 2_u32, 'bad signature length');
        assert(
            check_ecdsa_signature(
                message_hash: tx_info.transaction_hash,
                public_key: public_key::read(),
                signature_r: *signature.at(0_u32),
                signature_s: *signature.at(1_u32),
            ),
            'invalid signature',
        );

        'VALIDATED'
    }

    #[external]
    fn __validate__(
        constract_address: ContractAddress, selector: felt, calldata: Array::<felt>
    ) -> felt {
        validate_transaction()
    }

    // TODO(ilya): Add __execute__.

    #[external]
    fn __validate_declare__(class_hash: felt) -> felt {
        validate_transaction()
    }

    #[external]
    fn __validate_deploy__(
        class_hash: felt, contract_address_salt: felt, _public_key: felt
    ) -> felt {
        validate_transaction()
    }
}
