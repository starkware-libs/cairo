use std::collections::{HashMap, HashSet};
use std::mem;
use std::sync::{Arc, RwLock};

use itertools::{Either, Itertools};
use lsp_types::Url;
use tracing::error;

use crate::lang::diagnostics::file_diagnostics::FileDiagnostics;

/// Takes the result of locking and on error logs it and returns the default result value.
macro_rules! try_lock {
    ($lock:expr) => {
        match $lock {
            Ok(lock) => lock,
            Err(err) => {
                error!("project diagnostics locks are poisoned: {err:?}");
                return Default::default();
            }
        }
    };
}

/// Global storage of diagnostics for the entire analysed codebase(s).
///
/// This object can be shared between threads and accessed concurrently.
///
/// ## Identifying files
///
/// Globally, we have to always identify files by [`Url`] instead of [`FileId`],
/// as the diagnostics state is independent of analysis database swaps that invalidate interned IDs.
#[derive(Clone)]
pub struct ProjectDiagnostics {
    file_diagnostics: Arc<RwLock<HashMap<Url, FileDiagnostics>>>,
}

impl ProjectDiagnostics {
    /// Creates new project diagnostics instance.
    pub fn new() -> Self {
        Self { file_diagnostics: Default::default() }
    }

    /// Inserts new diagnostics for a file if they update the existing diagnostics.
    ///
    /// Returns `true` if stored diagnostics were updated; otherwise, returns `false`.
    pub fn insert(&self, file_url: &Url, new_file_diagnostics: FileDiagnostics) -> bool {
        if let Some(old_file_diagnostics) = try_lock!(self.file_diagnostics.read()).get(file_url) {
            if *old_file_diagnostics == new_file_diagnostics {
                return false;
            }
        };

        try_lock!(self.file_diagnostics.write()).insert(file_url.clone(), new_file_diagnostics);
        true
    }

    /// Removes diagnostics for files not present in the given set and returns a list of actually
    /// removed entries.
    pub fn clear_old(&self, files_to_retain: &HashSet<Url>) -> Vec<FileDiagnostics> {
        let mut file_diagnostics = try_lock!(self.file_diagnostics.write());

        let (clean, removed) =
            mem::take(&mut *file_diagnostics).into_iter().partition_map(|(file_url, diags)| {
                if files_to_retain.contains(&file_url) {
                    Either::Left((file_url, diags))
                } else {
                    Either::Right(diags)
                }
            });

        *file_diagnostics = clean;
        removed
    }
}
