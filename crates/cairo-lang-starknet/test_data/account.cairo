#[account_contract]
mod Account {
    use array::SpanTrait;
    use ecdsa::check_ecdsa_signature;
    use starknet::ContractAddress;
    use starknet::ContractAddressZeroable;
    use zeroable::Zeroable;

    struct Storage {
        public_key: felt
    }

    #[constructor]
    fn constructor(public_key_: felt) {
        public_key::write(public_key_);
    }

    fn validate_transaction() -> felt {
        let tx_info = unbox(starknet::get_tx_info());
        let signature = tx_info.signature;
        assert(signature.len() == 2_u32, 'INVALID_SIGNATURE_LENGTH');
        assert(
            check_ecdsa_signature(
                message_hash: tx_info.transaction_hash,
                public_key: public_key::read(),
                signature_r: *signature.at(0_u32),
                signature_s: *signature.at(1_u32),
            ),
            'INVALID_SIGNATURE',
        );

        starknet::VALIDATED
    }


    #[external]
    fn __validate_deploy__(
        class_hash: felt, contract_address_salt: felt, public_key_: felt
    ) -> felt {
        validate_transaction()
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
        // Validate caller.
        assert(starknet::get_caller_address().is_zero(), 'INVALID_CALLER');

        // Check the tx version here, since version 0 transaction skip the __validate__ function.
        let tx_info = unbox(starknet::get_tx_info());
        assert(tx_info.version != 0, 'INVALID_TX_VERSION');

        starknet::call_contract_syscall(
            contract_address, entry_point_selector, calldata
        ).unwrap_syscall()
    }
}
