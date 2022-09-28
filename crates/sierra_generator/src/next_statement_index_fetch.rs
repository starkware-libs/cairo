use sierra::program::GenBranchTarget;
use utils::unordered_hash_map::UnorderedHashMap;

use crate::pre_sierra::{LabelId, Statement};

/// Helper to fetch the next statement index from a branch target, and get the statement indices
/// for labels.
pub struct NextStatementIndexFetch {
    label_to_statement: UnorderedHashMap<LabelId, usize>,
}
impl NextStatementIndexFetch {
    /// Creates the mapping to fetch statement indices, ignores label statements according to the
    /// value of `include_label_indices`.
    pub fn new(statements: &[Statement], include_label_indices: bool) -> Self {
        let mut scanned_labels = 0;
        Self {
            label_to_statement: statements
                .iter()
                .enumerate()
                .filter_map(|(i, s)| match s {
                    Statement::Sierra(_) => None,
                    Statement::Label(label) => {
                        let entry = Some((
                            label.id,
                            if include_label_indices { i } else { i - scanned_labels },
                        ));
                        scanned_labels += 1;
                        entry
                    }
                    Statement::PushValues(_) => panic!(
                        "Unexpected pre_sierra::Statement::PushValues in \
                         NextStatementIndexFetch::new()."
                    ),
                })
                .collect(),
        }
    }
    /// Returns the next index matching a branches target.
    pub fn get(&self, index: usize, target: &GenBranchTarget<LabelId>) -> usize {
        match target {
            GenBranchTarget::Fallthrough => index + 1,
            GenBranchTarget::Statement(label) => self.resolve_label(label),
        }
    }
    /// Returns the index of a statement matching a lable.
    pub fn resolve_label(&self, label: &LabelId) -> usize {
        *self.label_to_statement.get(label).unwrap()
    }
}
