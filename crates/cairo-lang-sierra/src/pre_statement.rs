use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;

use crate::program::{
    BranchInfo, BranchTarget, Function, GenBranchInfo, GenBranchTarget, GenFunction, GenInvocation,
    GenStatement, Invocation, Statement, StatementIdx,
};

/// A statement that is not yet fully resolved.
/// Used for building the program during parsing with labels.
pub enum PreStatement {
    Label(String),
    Statement(GenStatement<StatementId>),
}

/// Statement identifier that is not yet fully resolved.
/// Used for building the program during parsing with labels.
#[derive(Debug)]
pub enum StatementId {
    Label(String),
    Idx(StatementIdx),
}

/// Finalize the pre-statements by resolving the labels, and generating the final statements and
/// functions.
pub fn finalize_prestatements(
    statements: Vec<PreStatement>,
    funcs: Vec<GenFunction<StatementId>>,
) -> (Vec<Statement>, Vec<Function>) {
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
    let map_stmt_id = |statement_id| match statement_id {
        StatementId::Label(label) => {
            *label_to_statement.get(&label).unwrap_or_else(|| panic!("Unknown label: {label}."))
        }
        StatementId::Idx(idx) => idx,
    };
    (
        statements
            .into_iter()
            .filter_map(|pre_statement| {
                try_extract_matches!(pre_statement, PreStatement::Statement)
            })
            .map(|statement| match statement {
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
                                        BranchTarget::Statement(map_stmt_id(statement_id))
                                    }
                                },
                            })
                            .collect(),
                    })
                }
                GenStatement::Return(vars) => Statement::Return(vars),
            })
            .collect(),
        funcs
            .into_iter()
            .map(|func| Function {
                id: func.id,
                signature: func.signature,
                params: func.params,
                entry_point: map_stmt_id(func.entry_point),
            })
            .collect(),
    )
}
