use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin_account::AccountComponent;
use openzeppelin_account::AccountComponent::{OwnerAdded, OwnerRemoved};
use openzeppelin_testing::constants::TRANSACTION_HASH;
use openzeppelin_testing::signing::StarkKeyPair;
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy};
use snforge_std::signature::stark_curve::StarkCurveSignerImpl;
use starknet::ContractAddress;

#[derive(Drop)]
pub struct SignedTransactionData {
    pub tx_hash: felt252,
    pub r: felt252,
    pub s: felt252,
}

pub fn SIGNED_TX_DATA(key_pair: StarkKeyPair) -> SignedTransactionData {
    let tx_hash = TRANSACTION_HASH;
    let (r, s) = key_pair.sign(tx_hash).unwrap();
    SignedTransactionData { tx_hash, r, s }
}

pub fn get_accept_ownership_signature(
    account_address: ContractAddress, current_public_key: felt252, new_key_pair: StarkKeyPair,
) -> Span<felt252> {
    let msg_hash = PoseidonTrait::new()
        .update_with('StarkNet Message')
        .update_with('accept_ownership')
        .update_with(account_address)
        .update_with(current_public_key)
        .finalize();
    let (sig_r, sig_s) = new_key_pair.sign(msg_hash).unwrap();
    array![sig_r, sig_s].span()
}

#[generate_trait]
pub impl AccountSpyHelpersImpl of AccountSpyHelpers {
    fn assert_event_owner_removed(
        ref self: EventSpy, contract: ContractAddress, removed_owner_guid: felt252,
    ) {
        let expected = AccountComponent::Event::OwnerRemoved(OwnerRemoved { removed_owner_guid });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_event_owner_added(
        ref self: EventSpy, contract: ContractAddress, new_owner_guid: felt252,
    ) {
        let expected = AccountComponent::Event::OwnerAdded(OwnerAdded { new_owner_guid });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_owner_added(
        ref self: EventSpy, contract: ContractAddress, new_owner_guid: felt252,
    ) {
        self.assert_event_owner_added(contract, new_owner_guid);
        self.assert_no_events_left_from(contract);
    }
}
