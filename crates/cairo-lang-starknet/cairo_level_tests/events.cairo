use core::debug::PrintTrait;
use core::test::test_utils::{assert_eq, assert_ne};
use starknet::syscalls::{deploy_syscall, get_block_hash_syscall};
use starknet::SyscallResultTrait;

#[starknet::interface]
trait IContractWithEvent<T> {
    fn emit_event(ref self: T, incremental: bool);
    fn emit_flat_event(ref self: T);
}

#[starknet::contract]
mod contract_with_event {
    use starknet::info::get_contract_address;
    #[storage]
    struct Storage {
        value: u128,
    }

    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    pub struct IncrementalEvent {
        pub value: u128,
    }

    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    pub struct StaticEvent {}

    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    pub enum FlatEvent {
        FlatEvent: StaticEvent,
    }

    #[event]
    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    pub enum Event {
        IncrementalEvent: IncrementalEvent,
        StaticEvent: StaticEvent,
        #[flat]
        FlatEvent: FlatEvent,
    }

    #[constructor]
    fn constructor(ref self: ContractState) {
        self.value.write(0);
    }

    #[external(v0)]
    fn emit_event(ref self: ContractState, incremental: bool) {
        if incremental {
            self.emit(Event::IncrementalEvent(IncrementalEvent { value: self.value.read() }));
            self.value.write(self.value.read() + 1);
        } else {
            self.emit(Event::StaticEvent(StaticEvent {}));
        }
    }

    #[external(v0)]
    fn emit_flat_event(ref self: ContractState) {
        self.emit(FlatEvent::FlatEvent(StaticEvent {}));
    }
}

use contract_with_event::{Event, IncrementalEvent, StaticEvent, FlatEvent};

#[test]
fn test_events() {
    core::internal::revoke_ap_tracking();
    // Set up.
    let (contract_address, _) = deploy_syscall(
        contract_with_event::TEST_CLASS_HASH.try_into().unwrap(),
        0,
        Default::default().span(),
        false
    )
        .unwrap();
    let mut contract = IContractWithEventDispatcher { contract_address };
    contract.emit_event(true);
    contract.emit_event(true);
    contract.emit_event(false);
    contract.emit_event(false);
    contract.emit_event(true);
    contract.emit_event(false);
    contract.emit_event(true);
    contract.emit_flat_event();
    contract.emit_flat_event();

    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::IncrementalEvent(IncrementalEvent { value: 0 }),
        'event == IncrementalEvent(0)'
    );

    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::IncrementalEvent(IncrementalEvent { value: 1 }),
        'event == IncrementalEvent(1)'
    );
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::StaticEvent(StaticEvent {}),
        'event == StaticEvent'
    );
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::StaticEvent(StaticEvent {}),
        'event == StaticEvent'
    );
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::IncrementalEvent(IncrementalEvent { value: 2 }),
        'event == IncrementalEvent(2)'
    );
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::StaticEvent(StaticEvent {}),
        'event == StaticEvent'
    );
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::IncrementalEvent(IncrementalEvent { value: 3 }),
        'event == IncrementalEvent(3)'
    );
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @Event::FlatEvent(FlatEvent::FlatEvent(StaticEvent {})),
        'event == FlatEvent'
    );
    // Check that `FlatEvent` is flattened and can be deserialized directly.
    assert_eq(
        @starknet::testing::pop_log(contract_address).unwrap(),
        @FlatEvent::FlatEvent(StaticEvent {}),
        'event == FlatEvent'
    );
    assert!(starknet::testing::pop_log_raw(contract_address).is_none());
}

#[test]
fn test_pop_log() {
    let contract_address = starknet::contract_address_const::<0x1234>();
    starknet::testing::set_contract_address(contract_address);
    let mut keys = array![];
    let mut data = array![];
    keys.append(1234);
    data.append(2345);
    starknet::emit_event_syscall(keys.span(), data.span()).unwrap_syscall();
    starknet::emit_event_syscall(keys.span(), data.span()).unwrap_syscall();

    let (keys, data) = starknet::testing::pop_log_raw(contract_address).unwrap();
    assert_eq!(keys.len(), 1);
    assert_eq!(data.len(), 1);
    assert_eq(keys.at(0), @1234, 'unexpected key');
    assert_eq(data.at(0), @2345, 'unexpected data');

    let (keys, data) = starknet::testing::pop_log_raw(contract_address).unwrap();
    assert_eq!(keys.len(), 1);
    assert_eq!(data.len(), 1);
    assert_eq(keys.at(0), @1234, 'unexpected key');
    assert_eq(data.at(0), @2345, 'unexpected data');
}
