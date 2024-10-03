use std::collections::HashMap;
use std::ops::{Add, AddAssign};

use lsp_types::Url;

use super::FileDiagnosticsChange;

pub struct StateDiff {
    changes: HashMap<Url, FileDiagnosticsChange>,
    calculated_all: bool,
}

impl StateDiff {
    pub fn new(files: impl IntoIterator<Item = Url>) -> Self {
        Self {
            changes: files.into_iter().map(|url| (url, FileDiagnosticsChange::Unchanged)).collect(),
            calculated_all: true,
        }
    }

    pub fn update_for(&mut self, url: &Url, change: FileDiagnosticsChange) {
        *self.changes.get_mut(url).unwrap() = change;
    }

    pub fn consume(&mut self, url: &Url) -> Option<FileDiagnosticsChange> {
        self.changes.remove(url)
    }

    pub fn calculated_all(&self) -> bool {
        self.calculated_all
    }

    pub fn calculating_all_failed(&mut self) {
        self.calculated_all = false;
    }
}

impl IntoIterator for StateDiff {
    type Item = (Url, FileDiagnosticsChange);
    type IntoIter = std::collections::hash_map::IntoIter<Url, FileDiagnosticsChange>;

    fn into_iter(self) -> Self::IntoIter {
        self.changes.into_iter()
    }
}

impl AddAssign for StateDiff {
    fn add_assign(&mut self, rhs: Self) {
        self.calculated_all = self.calculated_all && rhs.calculated_all;

        self.changes.extend(rhs.changes);
    }
}

impl Add for StateDiff {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;

        self
    }
}
