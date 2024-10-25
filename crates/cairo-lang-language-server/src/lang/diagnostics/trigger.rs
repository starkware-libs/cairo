use std::mem;
use std::sync::{Arc, Condvar, Mutex};

#[cfg(test)]
#[path = "trigger_test.rs"]
mod test;

const POISON_PANIC: &str = "invariant error: trigger mutex should never become poisoned";

/// The writer side of a trigger sender-receiver pair.
///
/// See [`trigger`] for more information.
pub struct Sender<T>(Arc<Inner<T>>);

/// The reader side of a trigger sender-receiver pair.
///
/// See [`trigger`] for more information.
pub struct Receiver<T>(Arc<Inner<T>>);

/// Creates a new trigger, returning the sender/receiver halves.
///
/// Trigger is an in-house synchronisation primitive that behaves like a 1-bounded synchronous
/// single-producer single-consumer channel, but with a twist, that sending a new message
/// overwrites the buffered one.
///
/// Triggers allow sending a single piece of work within a pair of controller and worker threads.
/// In case multiple pieces of work are sent (_activated the trigger_) before worker thread manages
/// to process them, only the last one will be processed.
///
/// ## Disconnecting
/// Dropping either the sender or the receiver will bring the trigger into a _disconnected_ state.
/// In this state, activating the trigger will be a no-op, while waiting on the receiver will
/// immediately return `None`, signalling that the worker thread should disconnect immediately.
///
/// ## Poisoning
/// This primitive uses a mutex internally, but care has been taken to never panic while holding
/// that mutex, so it should never become poisoned.
pub fn trigger<T>() -> (Sender<T>, Receiver<T>) {
    let inner =
        Arc::new(Inner { state_mutex: Mutex::new(State::Pending), condvar: Condvar::new() });

    (Sender(Arc::clone(&inner)), Receiver(inner))
}

impl<T> Sender<T> {
    /// Attempts to activate the trigger with the given value.
    ///
    /// In case the trigger is already activated but has not been yet picked up by the receiver,
    /// the value will overwrite the one buffered.
    ///
    /// This method will be a no-op if the trigger has been disconnected.
    pub fn activate(&self, value: T) {
        self.0.set_state(State::Activated(value));
    }
}

impl<T> Drop for Sender<T> {
    fn drop(&mut self) {
        self.0.set_state(State::Disconnected);
    }
}

impl<T> Receiver<T> {
    /// Attempts to wait for a value on this receiver,
    /// returning `None` if the trigger has been disconnected.
    ///
    /// This function will always block the current thread if the trigger is not activated,
    /// and it's possible for it to be activated in the future.
    pub fn wait(&self) -> Option<T> {
        self.0.wait()
    }
}

impl<T> Drop for Receiver<T> {
    fn drop(&mut self) {
        self.0.set_state(State::Disconnected);
    }
}

enum State<T> {
    Pending,
    Activated(T),
    Disconnected,
}

struct Inner<T> {
    state_mutex: Mutex<State<T>>,
    condvar: Condvar,
}

impl<T> Inner<T> {
    fn set_state(&self, state: State<T>) {
        let &Inner { state_mutex, condvar } = &self;

        let mut state_guard = state_mutex.lock().expect(POISON_PANIC);

        if let State::Disconnected = *state_guard {
            // Cannot go back from disconnected state.
            return;
        }

        *state_guard = state;

        // Wake up a thread blocked by waiting on the corresponding receiver.
        condvar.notify_one();
    }

    fn wait(&self) -> Option<T> {
        let &Inner { state_mutex, condvar } = &self;

        // This loop is a regular wait-with-condition pattern,
        // but written in a type and panic-safe manner.
        let mut state_guard = state_mutex.lock().expect(POISON_PANIC);
        loop {
            let idle = match &*state_guard {
                State::Pending => State::Pending,
                State::Activated(_) => State::Pending,
                State::Disconnected => State::Disconnected,
            };
            match mem::replace(&mut *state_guard, idle) {
                State::Pending => {
                    // Continue waiting.
                    state_guard = condvar.wait(state_guard).expect(POISON_PANIC);
                }
                State::Activated(value) => return Some(value),
                State::Disconnected => return None,
            }
        }
    }
}
