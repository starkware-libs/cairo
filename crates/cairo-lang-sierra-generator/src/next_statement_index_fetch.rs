use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::pre_sierra::{LabelId, Statement, StatementWithLocation};

/// Helper to fetch the next statement index from a branch target, and get the statement indices
/// for labels.
pub struct NextStatementIndexFetch<'db> {
    label_to_statement: UnorderedHashMap<LabelId<'db>, usize>,
}
impl<'db> NextStatementIndexFetch<'db> {
    /// Creates the mapping to fetch statement indices.
    ///
    /// If `include_label_indices` is `true`, indices will include label statements.
    /// Otherwise, those statements will be skipped.
    pub fn new(statements: &[StatementWithLocation<'db>], include_label_indices: bool) -> Self {
        let mut index = 0;
        let mut label_to_statement = UnorderedHashMap::default();
        for statement in statements {
            match &statement.statement {
                Statement::Sierra(_) => {
                    index += 1;
                }
                Statement::Label(label) => {
                    if label_to_statement.insert(label.id, index).is_some() {
                        panic!("Label was already declared.");
                    }
                    if include_label_indices {
                        index += 1;
                    }
                }
                Statement::PushValues(_) => panic!(
                    "Unexpected pre_sierra::Statement::PushValues in \
                     NextStatementIndexFetch::new()."
                ),
            }
        }
        Self { label_to_statement }
    }

    /// Returns the index of a statement pointed by the given label.
    pub fn resolve_label(&self, label: &LabelId<'db>) -> usize {
        // TODO(lior): handle missing labels.
        *self.label_to_statement.get(label).unwrap_or_else(|| {
            panic!("Missing label in NextStatementIndexFetch::resolve_label: {label:?}")
        })
    }
}
