use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::program::{
    BranchInfo, BranchTarget, GenBranchInfo, GenBranchTarget, GenInvocation, GenStatement,
    Invocation, Statement, StatementIdx,
};

pub enum PreStatement {
    Label(String),
    Statement(GenStatement<StatementId>),
}

#[derive(Debug)]
pub enum StatementId {
    Label(String),
    Idx(StatementIdx),
}

pub fn finalize_prestatements(statements: Vec<PreStatement>) -> Vec<Statement> {
    let mut statement_count = 0;
    let mut label_to_statement: UnorderedHashMap<String, StatementIdx> = Default::default();
    for statement in &statements {
        if let PreStatement::Label(label) = statement {
            assert!(
                label_to_statement.insert(label.clone(), StatementIdx(statement_count)).is_none(),
                "Duplicate label: {label}."
            );
        } else {
            statement_count += 1;
        }
    }
    statements
        .into_iter()
        .filter_map(|p| try_extract_matches!(p, PreStatement::Statement))
        .map(|s| match s {
            GenStatement::Invocation(GenInvocation { libfunc_id, args, branches }) => {
                Statement::Invocation(Invocation {
                    libfunc_id,
                    args,
                    branches: branches
                        .into_iter()
                        .map(|GenBranchInfo { results, target }| BranchInfo {
                            results,
                            target: match target {
                                GenBranchTarget::Fallthrough => BranchTarget::Fallthrough,
                                GenBranchTarget::Statement(statement_id) => {
                                    BranchTarget::Statement(match statement_id {
                                        StatementId::Label(label) => *label_to_statement
                                            .get(&label)
                                            .unwrap_or_else(|| panic!("Unknown label: {label}.")),
                                        StatementId::Idx(idx) => idx,
                                    })
                                }
                            },
                        })
                        .collect(),
                })
            }
            GenStatement::Return(vars) => Statement::Return(vars),
        })
        .collect()
}
