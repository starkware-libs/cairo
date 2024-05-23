use std::ops::{Deref, DerefMut};
use std::time::Duration;

use tokio::runtime::Runtime;
use tokio::task::AbortHandle;

/// A wrapper over a multithreaded [`Runtime`] that ensures it is properly shut down when dropped.
pub struct GuardedRuntime(Option<Runtime>);

impl GuardedRuntime {
    /// Starts a new multithreaded [`Runtime`].
    pub fn start() -> Self {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to start runtime");
        Self(Some(runtime))
    }
}

impl Drop for GuardedRuntime {
    fn drop(&mut self) {
        if let Some(runtime) = self.0.take() {
            runtime.shutdown_timeout(Duration::from_millis(300));
        }
    }
}

impl Deref for GuardedRuntime {
    type Target = Runtime;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("use after drop")
    }
}

impl DerefMut for GuardedRuntime {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().expect("use after drop")
    }
}

/// A guard object which aborts a linked task when dropped.
pub struct AbortOnDrop(AbortHandle);

impl Drop for AbortOnDrop {
    fn drop(&mut self) {
        self.0.abort();
    }
}

impl From<AbortHandle> for AbortOnDrop {
    fn from(handle: AbortHandle) -> Self {
        Self(handle)
    }
}
