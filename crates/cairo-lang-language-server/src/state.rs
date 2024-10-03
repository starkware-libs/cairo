use std::collections::HashSet;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use lsp_types::{ClientCapabilities, Url};
use salsa::ParallelDatabase;

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;
use crate::toolchain::scarb::ScarbToolchain;
use crate::{env_config, Tricks};

/// State of Language server.
pub struct State {
    pub db: AnalysisDatabase,
    pub open_files: Owned<HashSet<Url>>,
    pub config: Owned<Config>,
    pub client_capabilities: Owned<ClientCapabilities>,
    pub scarb_toolchain: ScarbToolchain,
    pub last_replace: SystemTime,
    pub db_replace_interval: Duration,
    pub tricks: Owned<Tricks>,
}

impl State {
    pub fn new(
        db: AnalysisDatabase,
        client_capabilities: ClientCapabilities,
        scarb_toolchain: ScarbToolchain,
        tricks: Tricks,
    ) -> Self {
        Self {
            db,
            open_files: Default::default(),
            config: Default::default(),
            client_capabilities: Owned::new(client_capabilities.into()),
            tricks: Owned::new(tricks.into()),
            scarb_toolchain,
            last_replace: SystemTime::now(),
            db_replace_interval: env_config::db_replace_interval(),
        }
    }

    pub fn snapshot(&self) -> StateSnapshot {
        StateSnapshot { db: self.db.snapshot() }
    }
}

/// Readonly snapshot of Language server state.
pub struct StateSnapshot {
    pub db: salsa::Snapshot<AnalysisDatabase>,
}

impl std::panic::UnwindSafe for StateSnapshot {}

/// Represents owned value that can be mutated.
/// Allows creating snapshot from self.
#[derive(Debug, Default)]
pub struct Owned<T: ?Sized>(Arc<T>);

/// Readonly snapshot of [`Owned`] value.
#[derive(Debug, Default, Clone)]
pub struct Snapshot<T: ?Sized>(Arc<T>);

impl<T: ?Sized> Owned<T> {
    pub fn new(inner: Arc<T>) -> Self {
        Self(inner)
    }

    pub fn snapshot(&self) -> Snapshot<T> {
        Snapshot(self.0.clone())
    }
}

impl<T: ?Sized> Deref for Owned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Clone> DerefMut for Owned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Arc::make_mut(&mut self.0)
    }
}

impl<T: ?Sized> Deref for Snapshot<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Clone> Snapshot<T> {
    pub fn owned(self) -> T {
        Arc::unwrap_or_clone(self.0)
    }
}
