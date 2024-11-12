use std::sync::atomic::{AtomicU64, Ordering};

use scarb_proc_macro_server_types::jsonrpc::RequestId;

/// Atomic Id generator.
#[derive(Debug, Default)]
pub struct IdGenerator {
    counter: AtomicU64,
}

impl IdGenerator {
    /// Every call to this method returns different ID, but order is not guaranteed.
    pub fn unique_id(&self) -> RequestId {
        self.counter.fetch_add(1, Ordering::Relaxed)
    }
}
