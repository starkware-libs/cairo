use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_semantic::SemanticDiagnostic;
use lsp_types::{ClientCapabilities, Url};
use salsa::ParallelDatabase;

use crate::Tricks;
use crate::config::Config;
use crate::lang::db::{AnalysisDatabase, AnalysisDatabaseSwapper};
use crate::toolchain::scarb::ScarbToolchain;

/// State of Language server.
pub struct State {
    pub db: AnalysisDatabase,
    pub open_files: Owned<HashSet<Url>>,
    pub file_diagnostics: Owned<HashMap<Url, FileDiagnostics>>,
    pub config: Owned<Config>,
    pub client_capabilities: Owned<ClientCapabilities>,
    pub scarb_toolchain: ScarbToolchain,
    pub db_swapper: AnalysisDatabaseSwapper,
    pub tricks: Owned<Tricks>,
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
    pub fn new(
        db: AnalysisDatabase,
        client_capabilities: ClientCapabilities,
        scarb_toolchain: ScarbToolchain,
        tricks: Tricks,
    ) -> Self {
        Self {
            db,
            open_files: Default::default(),
            file_diagnostics: Default::default(),
            config: Default::default(),
            client_capabilities: Owned::new(client_capabilities.into()),
            scarb_toolchain,
            db_swapper: AnalysisDatabaseSwapper::new(),
            tricks: Owned::new(tricks.into()),
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
