use std::thread;

use super::*;

#[test]
fn test_trigger() {
    let (trigger, actuator) = trigger();

    trigger.activate(42);
    assert_eq!(actuator.wait(), Some(42));

    trigger.activate(43);
    assert_eq!(actuator.wait(), Some(43));

    drop(trigger);
    assert_eq!(actuator.wait(), None);
}

#[test]
fn test_trigger_threaded() {
    let (trigger, actuator) = trigger();
    thread::scope(|s| {
        s.spawn(move || {
            for i in 0..10_000 {
                trigger.activate(i);
            }
        });

        s.spawn(move || {
            let past_i = -1;
            while let Some(i) = actuator.wait() {
                assert!(i > past_i);
            }
        });
    });
}

#[test]
fn test_drop_actuator() {
    let (trigger, actuator) = trigger();
    drop(actuator);
    // This line should just do nothing.
    trigger.activate(42);
}
