use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_semantic::SemanticDiagnostic;
use salsa::ParallelDatabase;
use tower_lsp::lsp_types::{ClientCapabilities, Url};

use crate::config::Config;
use crate::lang::db::AnalysisDatabase;

/// State of Language server.
pub struct State {
    pub db: AnalysisDatabase,
    pub open_files: Owned<HashSet<Url>>,
    pub file_diagnostics: Owned<HashMap<Url, FileDiagnostics>>,
    pub config: Owned<Config>,
    pub client_capabilities: Owned<ClientCapabilities>,
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct FileDiagnostics {
    pub parser: Diagnostics<ParserDiagnostic>,
    pub semantic: Diagnostics<SemanticDiagnostic>,
    pub lowering: Diagnostics<LoweringDiagnostic>,
}

impl FileDiagnostics {
    pub fn is_empty(&self) -> bool {
        self.semantic.is_empty() && self.lowering.is_empty() && self.parser.is_empty()
    }
}
impl std::panic::UnwindSafe for FileDiagnostics {}

impl State {
    pub fn new(db: AnalysisDatabase) -> Self {
        Self {
            db,
            open_files: Default::default(),
            file_diagnostics: Default::default(),
            config: Default::default(),
            client_capabilities: Default::default(),
        }
    }

    pub fn snapshot(&self) -> StateSnapshot {
        StateSnapshot {
            db: self.db.snapshot(),
            open_files: self.open_files.snapshot(),
            config: self.config.snapshot(),
            client_capabilities: self.client_capabilities.snapshot(),
        }
    }
}

/// Readonly snapshot of Language server state.
pub struct StateSnapshot {
    pub db: salsa::Snapshot<AnalysisDatabase>,
    pub open_files: Snapshot<HashSet<Url>>,
    pub config: Snapshot<Config>,
    pub client_capabilities: Snapshot<ClientCapabilities>,
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
