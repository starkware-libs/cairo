use serde::Serde;
use starknet::ContractAddress;
use starknet::contract_address::ContractAddressSerde;
use array::ArrayTrait;
use array::SpanTrait;

#[account_contract]
mod Account {
    use array::ArrayTrait;
    use array::SpanTrait;
    use ecdsa::check_ecdsa_signature;
    use option::OptionTrait;
    use super::Call;
    use super::ArrayCallSerde;
    use super::ArrayCallDrop;
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
    fn __execute__(mut calls: Array<Call>) -> Array::<felt> {
        // Validate caller.
        assert(starknet::get_caller_address().is_zero(), 'INVALID_CALLER');

        // Check the tx version here, since version 0 transaction skip the __validate__ function.
        let tx_info = unbox(starknet::get_tx_info());
        assert(tx_info.version != 0, 'INVALID_TX_VERSION');

        // TODO(ilya): Implement multi call.
        assert(calls.len() == 1_u32, 'MULTI_CALL_NOT_SUPPORTED');
        let Call{to, selector, calldata } = calls.pop_front().unwrap();

        starknet::call_contract_syscall(
            address: to, entry_point_selector: selector, :calldata
        ).unwrap_syscall()
    }
}


struct Call {
    to: ContractAddress,
    selector: felt,
    calldata: Array<felt>
}

impl ArrayCallDrop of Drop::<Array<Call>>;

impl CallSerde of Serde::<Call> {
    fn serialize(ref serialized: Array<felt>, input: Call) {
        let Call{to, selector, calldata } = input;
        Serde::serialize(ref serialized, to);
        Serde::serialize(ref serialized, selector);
        Serde::serialize(ref serialized, calldata);
    }
    fn deserialize(ref serialized: Span<felt>) -> Option<Call> {
        let to = Serde::<ContractAddress>::deserialize(ref serialized)?;
        let selector = Serde::<felt>::deserialize(ref serialized)?;
        let calldata = Serde::<Array::<felt>>::deserialize(ref serialized)?;
        Option::Some(Call { to, selector, calldata })
    }
}

impl ArrayCallSerde of Serde::<Array<Call>> {
    fn serialize(ref output: Array<felt>, mut input: Array<Call>) {
        Serde::<usize>::serialize(ref output, input.len());
        serialize_array_call_helper(ref output, input);
    }

    fn deserialize(ref serialized: Span<felt>) -> Option<Array<Call>> {
        let length = *serialized.pop_front()?;
        let mut arr = ArrayTrait::new();
        deserialize_array_call_helper(ref serialized, arr, length)
    }
}

fn serialize_array_call_helper(ref serialized: Array<felt>, mut input: Array<Call>) {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match gas::get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }
    match input.pop_front() {
        Option::Some(value) => {
            Serde::<Call>::serialize(ref serialized, value);
            serialize_array_call_helper(ref serialized, input);
        },
        Option::None(_) => {},
    }
}

fn deserialize_array_call_helper(
    ref serialized: Span<felt>, mut curr_output: Array<Call>, remaining: felt
) -> Option<Array<Call>> {
    if remaining == 0 {
        return Option::Some(curr_output);
    }

    // TODO(orizi): Replace with simple call once inlining is supported.
    match gas::get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }

    curr_output.append(Serde::<Call>::deserialize(ref serialized)?);
    deserialize_array_call_helper(ref serialized, curr_output, remaining - 1)
}
