#[cfg(test)]
#[path = "resolve_labels_test.rs"]
mod test;

use std::collections::{hash_map, HashMap};

use sierra::program;

use crate::pre_sierra;

/// Replaces labels with their corresponding StatementIdx.
pub fn resolve_labels(
    statements: Vec<pre_sierra::Statement>,
    label_replacer: &LabelReplacer,
) -> Vec<program::Statement> {
    statements
        .into_iter()
        .filter_map(|statement| match statement {
            pre_sierra::Statement::Sierra(sierra_statement) => {
                Some(label_replacer.handle_statement(sierra_statement))
            }
            pre_sierra::Statement::Label(_) => None,
        })
        .collect()
}

/// Returns a map from LabelId to the index of the next Sierra statement.
fn get_label_id_to_index(
    statements: &[pre_sierra::Statement],
) -> HashMap<pre_sierra::LabelId, usize> {
    let mut label_id_to_index = HashMap::<pre_sierra::LabelId, usize>::new();
    let mut index = 0;
    for statement in statements {
        match &statement {
            pre_sierra::Statement::Sierra(_) => {
                index += 1;
            }
            pre_sierra::Statement::Label(label) => {
                if let hash_map::Entry::Vacant(entry) = label_id_to_index.entry(label.id) {
                    entry.insert(index);
                } else {
                    panic!("Label {} was already declared.", label.id)
                }
            }
        }
    }
    label_id_to_index
}

/// Helper struct for resolve_labels.
pub struct LabelReplacer {
    label_id_to_index: HashMap<pre_sierra::LabelId, usize>,
}
impl LabelReplacer {
    fn new(label_id_to_index: HashMap<pre_sierra::LabelId, usize>) -> Self {
        Self { label_id_to_index }
    }

    pub fn from_statements(statements: &[pre_sierra::Statement]) -> LabelReplacer {
        Self::new(get_label_id_to_index(statements))
    }

    /// Replaces the pre-sierra labels in the given statement, and returns [program::Statement].
    fn handle_statement(
        &self,
        statement: program::GenStatement<pre_sierra::LabelId>,
    ) -> program::Statement {
        match statement {
            program::GenStatement::Invocation(invocation) => {
                program::Statement::Invocation(self.handle_invocation(invocation))
            }
            program::GenStatement::Return(statement) => program::Statement::Return(statement),
        }
    }

    /// Replaces the pre-sierra labels in the given invocation, and returns [program::Invocation].
    fn handle_invocation(
        &self,
        invocation: program::GenInvocation<pre_sierra::LabelId>,
    ) -> program::Invocation {
        program::Invocation {
            libfunc_id: invocation.libfunc_id,
            args: invocation.args,
            branches: invocation
                .branches
                .into_iter()
                .map(|branch_info| self.handle_branch_info(branch_info))
                .collect(),
        }
    }

    /// Replaces the pre-sierra labels in the given branch info, and returns [program::BranchInfo].
    fn handle_branch_info(
        &self,
        branch_info: program::GenBranchInfo<pre_sierra::LabelId>,
    ) -> program::BranchInfo {
        program::BranchInfo {
            target: self.handle_branch_target(branch_info.target),
            results: branch_info.results,
        }
    }

    /// Replaces the pre-sierra labels in the given branch target, and returns
    /// [program::BranchTarget].
    fn handle_branch_target(
        &self,
        branch_target: program::GenBranchTarget<pre_sierra::LabelId>,
    ) -> program::BranchTarget {
        match branch_target {
            program::GenBranchTarget::Fallthrough => program::GenBranchTarget::Fallthrough,
            program::GenBranchTarget::Statement(label_id) => {
                program::BranchTarget::Statement(self.handle_label_id(label_id))
            }
        }
    }

    /// Resolves the given pre-sierra label, and returns [program::StatementIdx].
    pub fn handle_label_id(&self, label_id: pre_sierra::LabelId) -> program::StatementIdx {
        // TODO(lior): handle missing labels.
        program::StatementIdx(self.label_id_to_index[&label_id])
    }
}
