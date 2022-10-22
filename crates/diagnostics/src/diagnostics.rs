#[cfg(test)]
#[path = "diagnostics_test.rs"]
mod test;

use std::fmt::Write;
use std::sync::Arc;

use db_utils::Upcast;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use filesystem::span::TextSpan;
use itertools::Itertools;

use crate::location_marks::get_location_marks;

/// A trait for diagnostics (i.e., errors and warnings) across the compiler.
/// Meant to be implemented by each module that may produce diagnostics.
pub trait DiagnosticEntry: Clone + std::fmt::Debug + Eq + std::hash::Hash {
    type DbType: Upcast<dyn FilesGroup> + ?Sized;
    fn format(&self, db: &Self::DbType) -> String;
    fn location(&self, db: &Self::DbType) -> DiagnosticLocation;
    // TODO(spapini): Add a way to inspect the diagnostic programmatically, e.g, downcast.
}
pub struct DiagnosticLocation {
    pub file_id: FileId,
    pub span: TextSpan,
}

/// A builder for Diagnostics, accumulating multiple diagnostic entries.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DiagnosticsBuilder<TEntry: DiagnosticEntry> {
    pub leaves: Vec<TEntry>,
    pub subtrees: Vec<Diagnostics<TEntry>>,
}
impl<TEntry: DiagnosticEntry> DiagnosticsBuilder<TEntry> {
    pub fn new() -> Self {
        Self { leaves: Default::default(), subtrees: Default::default() }
    }
    pub fn add(&mut self, diagnostic: TEntry) {
        self.leaves.push(diagnostic);
    }
    pub fn extend(&mut self, diagnostics: Diagnostics<TEntry>) {
        self.subtrees.push(diagnostics);
    }
    pub fn build(self) -> Diagnostics<TEntry> {
        Diagnostics(Arc::new(self))
    }
}

impl<TEntry: DiagnosticEntry> Default for DiagnosticsBuilder<TEntry> {
    fn default() -> Self {
        Self::new()
    }
}

/// A set of diagnostic entries that arose during a computation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Diagnostics<TEntry: DiagnosticEntry>(pub Arc<DiagnosticsBuilder<TEntry>>);
impl<TEntry: DiagnosticEntry> Diagnostics<TEntry> {
    pub fn new() -> Self {
        Self(DiagnosticsBuilder::default().into())
    }

    pub fn format(&self, db: &TEntry::DbType) -> String {
        let mut res = String::new();
        // Format leaves.
        for entry in &self.0.leaves {
            let location = entry.location(db);
            let filename = location.file_id.file_name(db.upcast());
            let marks = get_location_marks(db.upcast(), &location);
            let pos = match location.span.start.position_in_file(db.upcast(), location.file_id) {
                Some(pos) => format!("{}:{}", pos.line + 1, pos.col + 1),
                None => "?".into(),
            };
            let message = entry.format(db);
            writeln!(res, "error: {message}\n --> {filename}:{pos}\n{marks}\n").unwrap();
        }
        // Format subtrees.
        res += &self.0.subtrees.iter().map(|subtree| subtree.format(db)).join("");
        res
    }

    /// Asserts that no diagnostic has occurred, panicking with an error message on failure.
    pub fn expect(&self, error_message: &str) {
        assert!(self.0.leaves.is_empty(), "{}\n{:?}", error_message, self);
        for subtree in &self.0.subtrees {
            subtree.expect(error_message);
        }
    }

    /// Same as [Self::expect], except that the diagnostics are formatted.
    pub fn expect_with_db(&self, db: &TEntry::DbType, error_message: &str) {
        assert!(self.0.leaves.is_empty(), "{}\n{}", error_message, self.format(db));
        for subtree in &self.0.subtrees {
            subtree.expect_with_db(db, error_message);
        }
    }

    // TODO(spapini): This is temporary. Remove once the logic in language server doesn't use this.
    pub fn get_all(&self) -> Vec<TEntry> {
        let mut res = self.0.leaves.clone();
        for subtree in &self.0.subtrees {
            res.extend(subtree.get_all())
        }
        res
    }
}
impl<TEntry: DiagnosticEntry> Default for Diagnostics<TEntry> {
    fn default() -> Self {
        Self::new()
    }
}
