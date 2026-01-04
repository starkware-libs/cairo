use snforge_std::cheatcodes::events::Events;
use snforge_std::{EventSpy, EventSpyTrait, EventsFilterTrait};
use starknet::ContractAddress;

/// A wrapper around the `EventSpy` structure to allow treating the events as a queue.
#[derive(Drop, Serde)]
pub struct EventSpyQueue {
    event_offset: usize,
    event_spy: EventSpy,
}

/// Creates a new `EventSpyQueue` instance.
pub fn spy_events() -> EventSpyQueue {
    let event_spy = snforge_std::spy_events();
    EventSpyQueue { event_offset: 0, event_spy }
}

#[generate_trait]
pub impl EventSpyQueueImpl of EventSpyExt {
    fn get_events(ref self: EventSpyQueue) -> Events {
        let mut events = self.event_spy.get_events().events;

        // Remove events that have already been consumed
        for _ in 0..self.event_offset {
            let _ = events.pop_front();
        }

        Events { events }
    }

    /// Ensures that `from_address` has emitted only the `expected_event` and no additional events.
    fn assert_only_event<T, +starknet::Event<T>, +Drop<T>>(
        ref self: EventSpyQueue, from_address: ContractAddress, expected_event: T,
    ) {
        self.assert_emitted_single(from_address, expected_event);
        self.assert_no_events_left_from(from_address);
    }

    /// Ensures that `from_address` has emitted the `expected_event`.
    /// This assertion increments the event offset which essentially
    /// consumes the event in the first position of the offset. This means
    /// that events must be asserted in the order that they're emitted.
    fn assert_emitted_single<T, +starknet::Event<T>, +Drop<T>>(
        ref self: EventSpyQueue, from_address: ContractAddress, expected_event: T,
    ) {
        self.assert_emitted(@array![(from_address, expected_event)]);
        self.event_offset += 1;
    }

    /// Removes a single event from the queue. If the queue is empty, the function will panic.
    fn drop_event(ref self: EventSpyQueue) {
        self.drop_n_events(1);
    }

    /// Removes `number_to_drop` events from the queue. If the queue is empty, the function will
    /// panic.
    fn drop_n_events(ref self: EventSpyQueue, number_to_drop: u32) {
        let events = self.get_events().events;
        let len = events.len();
        assert!(
            len >= number_to_drop,
            "Not enough events to drop. ${len} events, ${number_to_drop} to drop",
        );
        self.event_offset += number_to_drop;
    }

    /// Removes all events remaining on the queue. If the queue is empty already, the function will
    /// do nothing.
    fn drop_all_events(ref self: EventSpyQueue) {
        let events = self.get_events().events;
        self.event_offset += events.len();
    }

    /// Ensures that there are no events remaining on the queue.
    fn assert_no_events_left(ref self: EventSpyQueue) {
        let events = self.get_events().events;
        assert!(events.len() == 0, "Events remaining on queue");
    }

    /// Ensures that there are no events emitted from the given address remaining on the queue.
    fn assert_no_events_left_from(ref self: EventSpyQueue, from_address: ContractAddress) {
        assert!(self.count_events_from(from_address) == 0, "Events remaining on queue");
    }

    /// Counts the number of remaining events emitted from the given address.
    fn count_events_from(ref self: EventSpyQueue, from_address: ContractAddress) -> u32 {
        let events = self.get_events().emitted_by(from_address).events;
        events.len()
    }
}

/// Allows to assert the expected events emission (or lack thereof),
/// in the scope of [`EventSpyQueue`] structure.
#[generate_trait]
impl EventSpyQueueAssertionsTraitImpl<
    T, impl TEvent: starknet::Event<T>, impl TDrop: Drop<T>,
> of EventSpyQueueAssertionsTrait<T> {
    fn assert_emitted(ref self: EventSpyQueue, events: @Array<(ContractAddress, T)>) {
        let mut i = 0;
        let received_events = self.get_events();

        while i != events.len() {
            let (from, event) = events.at(i);
            let emitted = is_emitted(@received_events, from, event);

            if !emitted {
                let from: felt252 = (*from).into();
                panic!("Event with matching data and keys was not emitted from {}", from);
            }

            i += 1;
        };
    }

    fn assert_not_emitted(ref self: EventSpyQueue, events: @Array<(ContractAddress, T)>) {
        let mut i = 0;
        let received_events = self.get_events();

        while i != events.len() {
            let (from, event) = events.at(i);
            let emitted = is_emitted(@received_events, from, event);

            if emitted {
                let from: felt252 = (*from).into();
                panic!("Event with matching data and keys was emitted from {}", from);
            }

            i += 1;
        };
    }
}

fn is_emitted<T, impl TEvent: starknet::Event<T>, impl TDrop: Drop<T>>(
    self: @Events, expected_emitted_by: @ContractAddress, expected_event: @T,
) -> bool {
    let mut expected_keys = array![];
    let mut expected_data = array![];
    expected_event.append_keys_and_data(ref expected_keys, ref expected_data);

    let mut i = 0;
    let mut is_emitted = false;
    while i != self.events.len() {
        let (from, event) = self.events.at(i);

        if from == expected_emitted_by
            && event.keys == @expected_keys
            && event.data == @expected_data {
            is_emitted = true;
            break;
        }

        i += 1;
    }
    return is_emitted;
}
