use std::sync::{Arc, Condvar, Mutex};

/// Single element buffer that allows force pushing into it.
pub struct ForceWriteBuffer<T>(Arc<ForceWriteBufferInner<T>>);

impl<T> Clone for ForceWriteBuffer<T> {
    fn clone(&self) -> Self {
        Self(Clone::clone(&self.0))
    }
}

impl<T> ForceWriteBuffer<T> {
    /// Initializes empty buffer.
    pub fn new() -> Self {
        ForceWriteBuffer(Arc::new(ForceWriteBufferInner {
            inner: Mutex::new(None),
            condvar: Condvar::new(),
        }))
    }

    /// Forcefully pushes into buffer.
    pub fn force_push(&self, value: T) {
        self.0.force_push(value)
    }

    /// Read from buffer leaving it empty. If it is currently empty waits for push first.
    pub fn pop(&self) -> T {
        self.0.pop()
    }
}

struct ForceWriteBufferInner<T> {
    inner: Mutex<Option<T>>,
    condvar: Condvar,
}

impl<T> ForceWriteBufferInner<T> {
    fn force_push(&self, value: T) {
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
