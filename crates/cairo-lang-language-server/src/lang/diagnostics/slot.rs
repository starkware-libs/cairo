use std::sync::{Arc, Condvar, Mutex};

/// Single element buffer that allows force pushing into it.

pub struct SlotReader<T>(Arc<Slot<T>>);

impl<T> SlotReader<T> {
    /// Read from buffer leaving it empty. If it is currently empty waits for push first.
    pub fn pop(&self) -> T {
        self.0.pop()
    }
}

pub struct SlotWriter<T>(Arc<Slot<T>>);

impl<T> SlotWriter<T> {
    /// Forcefully pushes into buffer.
    pub fn set(&self, value: T) {
        self.0.set(value)
    }
}

pub fn slot<T>(default: Option<T>) -> (SlotReader<T>, SlotWriter<T>) {
    let slot = Arc::new(Slot { inner: Mutex::new(default), condvar: Condvar::new() });

    (SlotReader(slot.clone()), SlotWriter(slot))
}

struct Slot<T> {
    inner: Mutex<Option<T>>,
    condvar: Condvar,
}

impl<T> Slot<T> {
    fn set(&self, value: T) {
        let mut data = self.inner.lock().unwrap();

        *data = Some(value);

        // Notify one waiting thread, if there is one.
        self.condvar.notify_one();
    }

    fn pop(&self) -> T {
        let mut data = self.inner.lock().unwrap();

        while data.is_none() {
            // Wait until there is some data available to consume.
            data = self.condvar.wait(data).unwrap();
        }

        data.take().unwrap()
    }
}
