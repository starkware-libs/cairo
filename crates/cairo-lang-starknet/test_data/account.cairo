#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[account_contract]
mod Account {
    use array::SpanTrait;
    use ecdsa::check_ecdsa_signature;

    struct Storage {
        public_key: felt
    }

    fn validate_transaction() {
        let tx_info = unbox(starknet::get_tx_info());

        check_ecdsa_signature(message_hash: tx_info.transaction_hash, public_key: public_key::read(), signature_r: *tx_info.signature.at(0_u32), signature_s: *tx_info.signature.at(0_u32) );
    }

    #[external]
    fn __validate__(calldata: Array::<felt>) {
        validate_transaction()
    }

    // TODO(ilya): Add __execute__.

    #[external]
    fn __validate_declare__(class_hash: felt) {
        validate_transaction()
    }

    #[external]
    fn __validate_deploy__(class_hash: felt, contract_address_salt: felt, _public_key: felt) {
        validate_transaction()
    }
}
