use std::mem;
use std::sync::{Arc, Condvar, Mutex};

#[cfg(test)]
#[path = "trigger_test.rs"]
mod test;

const POISON_PANIC: &str = "invariant error: trigger mutex should never become poisoned";

/// The writer side of a trigger-actuator pair.
///
/// See [`trigger`] for more information.
pub struct Trigger<T>(Arc<Inner<T>>);

/// The reader side of a trigger-actuator pair.
///
/// See [`trigger`] for more information.
pub struct Actuator<T>(Arc<Inner<T>>);

/// Creates a new trigger, returning the trigger/actuator halves.
///
/// Trigger is an in-house synchronisation primitive that allows signalling a single piece of work
/// to do within a pair of controller and worker threads.
/// In case multiple pieces of work are sent (_activated the trigger_) before worker thread manages
/// to process them, only the last one will be processed.
/// So, this primitive behaves like a 1-bounded synchronous single-producer single-consumer channel,
/// but with a twist, that sending a new message overwrites the buffered one.
///
/// ## Disconnecting
/// Dropping either the trigger or the actuator will bring the trigger into a _disconnected_ state.
/// In this state, activating the trigger will be a no-op, while waiting on the actuator will
/// immediately return `None`, signalling that the worker thread should disconnect immediately.
///
/// ## Poisoning
/// This primitive uses a mutex internally, but care has been taken to never panic while holding
/// that mutex, so it should never become poisoned.
pub fn trigger<T>() -> (Trigger<T>, Actuator<T>) {
    let inner = Arc::new(Inner { mutex: Mutex::new(State::Pending), condvar: Condvar::new() });
    (Trigger(Arc::clone(&inner)), Actuator(inner))
}

impl<T> Trigger<T> {
    /// Attempts to activate the trigger with the given value.
    ///
    /// In case the trigger is already activated but has not been yet picked up by the actuator,
    /// the value will overwrite the one buffered.
    ///
    /// This method will be a no-op if the trigger has been disconnected.
    pub fn activate(&self, value: T) {
        self.0.set_state(State::Activate(value));
    }
}

impl<T> Drop for Trigger<T> {
    fn drop(&mut self) {
        self.0.set_state(State::Disconnect);
    }
}

impl<T> Actuator<T> {
    /// Attempts to wait for a value on this actuator,
    /// returning `None` if the trigger has been disconnected.
    ///
    /// This function will always block the current thread if the trigger is not activated,
    /// and it's possible for it to be activated in the future.
    pub fn wait(&self) -> Option<T> {
        self.0.wait()
    }
}

impl<T> Drop for Actuator<T> {
    fn drop(&mut self) {
        self.0.set_state(State::Disconnect);
    }
}

enum State<T> {
    Pending,
    Activate(T),
    Disconnect,
}

struct Inner<T> {
    mutex: Mutex<State<T>>,
    condvar: Condvar,
}

impl<T> Inner<T> {
    fn set_state(&self, state: State<T>) {
        let &Inner { mutex, condvar } = &self;

        let mut guard = mutex.lock().expect(POISON_PANIC);

        if let State::Disconnect = *guard {
            // Cannot go back from disconnected state.
            return;
        }

        *guard = state;

        // Wake up a thread blocked by waiting on the corresponding actuator.
        condvar.notify_one();
    }

    fn wait(&self) -> Option<T> {
        let &Inner { mutex, condvar } = &self;

        // This loop is a regular wait-with-condition pattern,
        // but written in a type and panic-safe manner.
        let mut guard = mutex.lock().expect(POISON_PANIC);
        loop {
            let idle = match &*guard {
                State::Pending => State::Pending,
                State::Activate(_) => State::Pending,
                State::Disconnect => State::Disconnect,
            };
            match mem::replace(&mut *guard, idle) {
                State::Pending => {
                    // Continue waiting.
                    guard = condvar.wait(guard).expect(POISON_PANIC);
                }
                State::Activate(value) => break Some(value),
                State::Disconnect => break None,
            }
        }
    }
}
