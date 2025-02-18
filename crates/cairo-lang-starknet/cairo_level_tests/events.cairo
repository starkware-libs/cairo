use starknet::SyscallResultTrait;
use starknet::syscalls::deploy_syscall;

#[starknet::interface]
trait IContractWithEvent<T> {
    fn emit_event(ref self: T, incremental: bool);
    fn emit_flat_event(ref self: T);
}

#[starknet::contract]
mod contract_with_event {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    struct Storage {
        value: u128,
    }

    #[derive(Copy, Drop, Debug, PartialEq, starknet::Event)]
    pub struct IncrementalEvent {
        pub value: u128,
    }

    #[derive(Copy, Drop, Debug, PartialEq, starknet::Event)]
    pub struct StaticEvent {}

    #[derive(Copy, Drop, Debug, PartialEq, starknet::Event)]
    pub enum FlatEvent {
        FlatEvent: StaticEvent,
    }

    #[event]
    #[derive(Copy, Drop, Debug, PartialEq, starknet::Event)]
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
use contract_with_event::{Event, FlatEvent, IncrementalEvent, StaticEvent};

#[test]
fn test_events() {
    core::internal::revoke_ap_tracking();
    // Set up.
    let (contract_address, _) = deploy_syscall(
        contract_with_event::TEST_CLASS_HASH.try_into().unwrap(), 0, [].span(), false,
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

    assert_eq!(
        starknet::testing::pop_log(contract_address),
        Some(Event::IncrementalEvent(IncrementalEvent { value: 0 })),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address),
        Some(Event::IncrementalEvent(IncrementalEvent { value: 1 })),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address), Some(Event::StaticEvent(StaticEvent {})),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address), Some(Event::StaticEvent(StaticEvent {})),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address),
        Some(Event::IncrementalEvent(IncrementalEvent { value: 2 })),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address), Some(Event::StaticEvent(StaticEvent {})),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address),
        Some(Event::IncrementalEvent(IncrementalEvent { value: 3 })),
    );
    assert_eq!(
        starknet::testing::pop_log(contract_address),
        Some(Event::FlatEvent(FlatEvent::FlatEvent(StaticEvent {}))),
    );
    // Check that `FlatEvent` is flattened and can be deserialized directly.
    assert_eq!(
        starknet::testing::pop_log(contract_address), Some(FlatEvent::FlatEvent(StaticEvent {})),
    );
    assert!(starknet::testing::pop_log_raw(contract_address).is_none());
}

#[test]
fn test_pop_log() {
    const CONTRACT_ADDRESS: starknet::ContractAddress = 0x1234_felt252.try_into().unwrap();
    starknet::testing::set_contract_address(CONTRACT_ADDRESS);
    let (keys, data) = ([1234].span(), [2345].span());
    starknet::syscalls::emit_event_syscall(keys, data).unwrap_syscall();
    starknet::syscalls::emit_event_syscall(keys, data).unwrap_syscall();

    assert_eq!(starknet::testing::pop_log_raw(CONTRACT_ADDRESS), Some((keys, data)));
    assert_eq!(starknet::testing::pop_log_raw(CONTRACT_ADDRESS), Some((keys, data)));
}
