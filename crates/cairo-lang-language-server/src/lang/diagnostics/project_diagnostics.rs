use std::collections::{HashMap, HashSet};
use std::mem;
use std::sync::{Arc, RwLock};

use itertools::{Either, Itertools};
use lsp_types::Url;

use crate::lang::diagnostics::file_diagnostics::FileDiagnostics;

/// Global storage of diagnostics for the entire analysed codebase(s).
///
/// This object can be shared between threads and accessed concurrently.
///
/// ## Identifying files
///
/// Globally, we have to always identify files by [`Url`] instead of [`FileId`],
/// as the diagnostics state is independent of analysis database swaps that invalidate interned IDs.
///
/// [`FileId`]: cairo_lang_filesystem::ids::FileId
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
    pub fn insert(&self, diags: FileDiagnostics) -> bool {
        if let Some(old_diags) = self
            .file_diagnostics
            .read()
            .expect("file diagnostics are poisoned, bailing out")
            .get(&diags.url)
        {
            if *old_diags == diags {
                return false;
            }
        };

        self.file_diagnostics
            .write()
            .expect("file diagnostics are poisoned, bailing out")
            .insert(diags.url.clone(), diags);
        true
    }

    /// Removes diagnostics for files not present in the given set and returns a list of actually
    /// removed entries.
    pub fn clear_old(&self, files_to_retain: &HashSet<Url>) -> Vec<FileDiagnostics> {
        let mut file_diagnostics =
            self.file_diagnostics.write().expect("file diagnostics are poisoned, bailing out");

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
