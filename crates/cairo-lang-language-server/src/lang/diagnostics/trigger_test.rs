use std::thread;

use super::*;

#[test]
fn test_sync() {
    let (sender, receiver) = trigger();

    sender.activate(42);
    assert_eq!(receiver.wait(), Some(42));

    sender.activate(43);
    assert_eq!(receiver.wait(), Some(43));

    drop(sender);
    assert_eq!(receiver.wait(), None);
}

#[test]
fn test_threaded() {
    let (sender, receiver) = trigger();
    thread::scope(|s| {
        s.spawn(move || {
            for i in 0..10_000 {
                sender.activate(i);
            }
        });

        s.spawn(move || {
            let past_i = -1;
            while let Some(i) = receiver.wait() {
                assert!(i > past_i);
            }
        });
    });
}

#[test]
fn test_drop_receiver() {
    let (sender, receiver) = trigger();
    drop(receiver);
    // This line should just do nothing.
    sender.activate(42);
}
