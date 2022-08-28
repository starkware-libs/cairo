#[cfg(test)]
#[path = "resolve_labels_test.rs"]
mod tests;

use std::collections::{hash_map, HashMap};

use sierra::program;

use crate::pre_sierra;

/// Replaces labels with their corresponding StatementIdx.
pub fn resolve_labels(statements: &Vec<pre_sierra::Statement>) -> Vec<program::Statement> {
    let label_replacer = LabelReplacer::new(get_label_id_to_index(statements));

    let mut result: Vec<program::Statement> = vec![];
    for statement in statements {
        match statement {
            pre_sierra::Statement::SierraStatement(sierra_statement) => {
                result.push(label_replacer.handle_statement(sierra_statement))
            }
            pre_sierra::Statement::Label(_) => {}
        }
    }

    result
}

/// Returns a map from LabelId to the index of the next Sierra statement.
fn get_label_id_to_index(
    statements: &Vec<pre_sierra::Statement>,
) -> HashMap<pre_sierra::LabelId, usize> {
    let mut label_id_to_index = HashMap::<pre_sierra::LabelId, usize>::new();
    let mut idx = 0;
    for statement in statements {
        match &statement {
            pre_sierra::Statement::SierraStatement(_) => {
                idx += 1;
            }
            pre_sierra::Statement::Label(label) => {
                if let hash_map::Entry::Vacant(entry) = label_id_to_index.entry(label.id.clone()) {
                    entry.insert(idx);
                } else {
                    panic!()
                }
            }
        }
    }
    label_id_to_index
}

/// Helper struct for resolve_labels.
struct LabelReplacer {
    label_id_to_index: HashMap<pre_sierra::LabelId, usize>,
}
impl LabelReplacer {
    fn new(label_id_to_index: HashMap<pre_sierra::LabelId, usize>) -> Self {
        LabelReplacer { label_id_to_index }
    }

    fn handle_statement(
        &self,
        statement: &program::GenStatement<pre_sierra::LabelId>,
    ) -> program::Statement {
        match statement {
            program::GenStatement::Invocation(invocation) => {
                program::Statement::Invocation(self.handle_invocation(invocation))
            }
            program::GenStatement::Return(statement) => {
                program::Statement::Return(statement.clone())
            }
        }
    }

    fn handle_invocation(
        &self,
        invocation: &program::GenInvocation<pre_sierra::LabelId>,
    ) -> program::Invocation {
        program::Invocation {
            libfunc_id: invocation.libfunc_id.clone(),
            args: invocation.args.clone(),
            branches: invocation
                .branches
                .iter()
                .map(|branch_info| self.handle_branch_info(branch_info))
                .collect(),
        }
    }

    fn handle_branch_info(
        &self,
        branch_info: &program::GenBranchInfo<pre_sierra::LabelId>,
    ) -> program::BranchInfo {
        program::BranchInfo {
            target: self.handle_branch_target(&branch_info.target),
            results: branch_info.results.clone(),
        }
    }

    fn handle_branch_target(
        &self,
        branch_target: &program::GenBranchTarget<pre_sierra::LabelId>,
    ) -> program::BranchTarget {
        match branch_target {
            program::GenBranchTarget::Fallthrough => program::GenBranchTarget::Fallthrough,
            program::GenBranchTarget::Statement(label_id) => {
                program::BranchTarget::Statement(self.handle_label_id(label_id.clone()))
            }
        }
    }

    fn handle_label_id(&self, label_id: pre_sierra::LabelId) -> program::StatementIdx {
        // TODO(lior): handle missing labels.
        program::StatementIdx(self.label_id_to_index[&label_id])
    }
}
