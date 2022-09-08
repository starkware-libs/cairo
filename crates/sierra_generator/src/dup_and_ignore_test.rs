use salsa::InternKey;
use sierra::ids::VarId;
use sierra::program::{self, Param};
use utils::ordered_hash_set::OrderedHashSet;

use super::calculate_statement_dups_and_ignores;
use crate::dup_and_ignore::VarsDupsAndIgnores;
use crate::pre_sierra::{Label, LabelId, Statement};
use crate::utils::{return_statement, simple_statement};

fn params<const COUNT: usize>(ids: [&str; COUNT]) -> Vec<Param> {
    ids.into_iter().map(|id| Param { id: id.into(), ty: "NotUsed".into() }).collect()
}

fn as_ids<const COUNT: usize>(ids: [&str; COUNT]) -> Vec<VarId> {
    ids.into_iter().map(|id| id.into()).collect()
}

fn map_stmt<const COUNT1: usize, const COUNT2: usize>(
    inputs: [&str; COUNT1],
    outputs: [&str; COUNT2],
) -> Statement {
    simple_statement("mock".into(), &as_ids(inputs), &as_ids(outputs))
}

fn ret_stmt<const COUNT: usize>(ids: [&str; COUNT]) -> Statement {
    return_statement(as_ids(ids))
}

fn as_set<const COUNT: usize>(ids: [&str; COUNT]) -> OrderedHashSet<VarId> {
    ids.into_iter().map(|id| id.into()).collect()
}

#[test]
fn all_used_no_ignore() {
    assert_eq!(
        calculate_statement_dups_and_ignores(
            &params(["arg"]),
            &[map_stmt(["arg"], ["res"]), ret_stmt(["res"])]
        ),
        vec![
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set([]) },
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set([]) }
        ],
    );
}

#[test]
fn ignore_unused() {
    assert_eq!(
        calculate_statement_dups_and_ignores(
            &params(["arg1", "arg2"]),
            &[map_stmt(["arg1"], ["res1", "res2"]), ret_stmt(["res1"])]
        ),
        vec![
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set(["arg2"]) },
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set(["res2"]) }
        ],
    );
}

#[test]
fn dup_reused() {
    assert_eq!(
        calculate_statement_dups_and_ignores(
            &params(["arg"]),
            &[map_stmt(["arg"], ["res"]), ret_stmt(["arg", "res"])]
        ),
        vec![
            VarsDupsAndIgnores { dups: as_set(["arg"]), ignores: as_set([]) },
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set([]) }
        ],
    );
}

#[test]
fn branch_merging() {
    let label_id = LabelId::from_intern_id(salsa::InternId::from(1u32));
    // All vars are the concatanation of the statement indices that uses them here.
    assert_eq!(
        calculate_statement_dups_and_ignores(
            &params(["none", "0", "1", "3", "0_1", "0_3", "1_3", "0_1_3",]),
            &[
                Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
                    libfunc_id: "mock".into(),
                    args: as_ids(["0", "0_1", "0_3", "0_1_3"]),
                    branches: vec![
                        program::GenBranchInfo {
                            target: program::GenBranchTarget::Statement(label_id),
                            results: vec![],
                        },
                        program::GenBranchInfo {
                            target: program::GenBranchTarget::Fallthrough,
                            results: vec![],
                        }
                    ],
                })),
                ret_stmt(["1", "0_1", "1_3", "0_1_3"]),
                Statement::Label(Label { id: label_id }),
                ret_stmt(["3", "0_3", "1_3", "0_1_3"])
            ]
        ),
        vec![
            VarsDupsAndIgnores { dups: as_set(["0_1", "0_3", "0_1_3"]), ignores: as_set(["none"]) },
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set(["3", "0_3"]) },
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set([]) },
            VarsDupsAndIgnores { dups: as_set([]), ignores: as_set(["1", "0_1"]) },
        ],
    );
}
