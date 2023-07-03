use core::debug::PrintTrait;
use core::traits::Into;
use core::result::ResultTrait;
use test::test_utils::{assert_eq, assert_ne};
use starknet::syscalls::{deploy_syscall, get_block_hash_syscall};
use traits::TryInto;
use option::OptionTrait;
use starknet::SyscallResultTrait;
use starknet::class_hash::Felt252TryIntoClassHash;
use array::ArrayTrait;
use array::SpanTrait;

#[starknet::interface]
trait IContractWithEvent<T> {
    fn emit_event(ref self: T, incremental: bool);
}

#[starknet::contract]
mod contract_with_event {
    use traits::Into;
    use starknet::info::get_contract_address;
    #[storage]
    struct Storage {
        value: u128, 
    }

    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    struct IncrementalEvent {
        value: u128, 
    }

    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    struct StaticEvent {}

    #[event]
    #[derive(Copy, Drop, PartialEq, starknet::Event)]
    enum Event {
        IncrementalEvent: IncrementalEvent,
        StaticEvent: StaticEvent,
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
}

use contract_with_event::{Event, IncrementalEvent, StaticEvent};

#[test]
#[available_gas(30000000)]
fn test_events() {
    internal::revoke_ap_tracking();
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
    assert(starknet::testing::pop_log_raw(contract_address).is_none(), 'no more events');
}

#[test]
#[available_gas(300000)]
fn test_pop_log() {
    let contract_address = starknet::contract_address_const::<0x1234>();
    starknet::testing::set_contract_address(contract_address);
    let mut keys = Default::default();
    let mut data = Default::default();
    keys.append(1234);
    data.append(2345);
    starknet::emit_event_syscall(keys.span(), data.span());
    starknet::emit_event_syscall(keys.span(), data.span());

    let (keys, data) = starknet::testing::pop_log_raw(contract_address).unwrap();
    assert_eq(@keys.len(), @1, 'unexpected keys size');
    assert_eq(@data.len(), @1, 'unexpected data size');
    assert_eq(keys.at(0), @1234, 'unexpected key');
    assert_eq(data.at(0), @2345, 'unexpected data');

    let (keys, data) = starknet::testing::pop_log_raw(contract_address).unwrap();
    assert_eq(@keys.len(), @1, 'unexpected keys size');
    assert_eq(@data.len(), @1, 'unexpected data size');
    assert_eq(keys.at(0), @1234, 'unexpected key');
    assert_eq(data.at(0), @2345, 'unexpected data');
}
