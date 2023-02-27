#[account_contract]
mod Account {
    use array::SpanTrait;
    use ecdsa::check_ecdsa_signature;
    use starknet::ContractAddress;

    struct Storage {
        public_key: felt
    }

    #[constructor]
    fn constructor(public_key_: felt) {
        public_key::write(public_key_);
    }

    fn validate_transaction_ex(public_key_: felt) -> felt {
        let tx_info = unbox(starknet::get_tx_info());
        let signature = tx_info.signature;
        assert(signature.len() == 2_u32, 'INVALID_SIGNATURE_LENGTH');
        assert(
            check_ecdsa_signature(
                message_hash: tx_info.transaction_hash,
                public_key: public_key_,
                signature_r: *signature.at(0_u32),
                signature_s: *signature.at(1_u32),
            ),
            'invalid signature',
        );

        'VALIDATED'
    }

    fn validate_transaction() -> felt {
        validate_transaction_ex(public_key::read())
    }

    #[external]
    fn __validate_deploy__(
        class_hash: felt, contract_address_salt: felt, public_key_: felt
    ) -> felt {
        // Note that the storage var is not set at this point, so we need to take the public
        // key from the arguments.
        validate_transaction_ex(public_key_)
    }

    #[external]
    fn __validate_declare__(class_hash: felt) -> felt {
        validate_transaction()
    }

    #[external]
    fn __validate__(
        contract_address: ContractAddress, entry_point_selector: felt, calldata: Array::<felt>
    ) -> felt {
        validate_transaction()
    }

    #[external]
    #[raw_output]
    fn __execute__(
        contract_address: ContractAddress, entry_point_selector: felt, calldata: Array::<felt>
    ) -> Array::<felt> {
        starknet::call_contract_syscall(
            contract_address, entry_point_selector, calldata
        ).unwrap_syscall()
    }
}
