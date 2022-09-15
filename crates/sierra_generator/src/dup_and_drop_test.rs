use salsa::InternKey;
use sierra::ids::VarId;
use sierra::program::{self, Param};
use utils::ordered_hash_set::OrderedHashSet;

use super::{calculate_statement_dups_and_drops, VarsDupsAndDrops};
use crate::pre_sierra::{Label, LabelId, Statement};
use crate::utils::{return_statement, simple_statement};

/// Returns a vector of params based on inputs mapped into variable ids.
/// The type of the params is ignored.
fn params<const COUNT: usize>(ids: [&str; COUNT]) -> Vec<Param> {
    ids.into_iter().map(|id| Param { id: id.into(), ty: "NotUsed".into() }).collect()
}

/// Returns a vector of variable ids based on the inputs mapped into varaible ids.
fn as_var_id_vec<const COUNT: usize>(ids: [&str; COUNT]) -> Vec<VarId> {
    ids.into_iter().map(|id| id.into()).collect()
}

/// Returns a simple non-branch libfunc invocation mapping from a set of input var ids to a set of
/// output var ids.
fn mock_non_branch_statement<const COUNT1: usize, const COUNT2: usize>(
    inputs: [&str; COUNT1],
    outputs: [&str; COUNT2],
) -> Statement {
    simple_statement("mock".into(), &as_var_id_vec(inputs), &as_var_id_vec(outputs))
}

/// Returns a return statement returning the given var ids.
fn as_return_statement<const COUNT: usize>(ids: [&str; COUNT]) -> Statement {
    return_statement(as_var_id_vec(ids))
}

/// Returns a set of var ids generated from the inputs as var ids.
fn as_var_id_set<const COUNT: usize>(ids: [&str; COUNT]) -> OrderedHashSet<VarId> {
    ids.into_iter().map(|id| id.into()).collect()
}

#[test]
fn all_used_no_d() {
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(["arg1", "arg2"]),
            &[
                mock_non_branch_statement(["arg1"], ["mid1"]),
                mock_non_branch_statement(["arg2"], ["mid2"]),
                mock_non_branch_statement(["mid1", "mid2"], ["res"]),
                as_return_statement(["res"])
            ]
        ),
        vec![
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set([]) },
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set([]) },
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set([]) },
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set([]) }
        ],
    );
}

#[test]
fn ignore_unused() {
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(["arg1", "arg2"]),
            &[mock_non_branch_statement(["arg1"], ["res1", "res2"]), as_return_statement(["res1"])]
        ),
        vec![
            // arg2 is no longer used and can be dropped before the first statement.
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set(["arg2"]) },
            // res2 is no longer used and can be dropped before the second statement.
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set(["res2"]) }
        ],
    );
}

#[test]
fn dup_reused() {
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(["arg"]),
            &[mock_non_branch_statement(["arg"], ["res"]), as_return_statement(["arg", "res"])]
        ),
        vec![
            // arg is used by both statements and therefore should be duplicated.
            VarsDupsAndDrops { dups: as_var_id_set(["arg"]), drops: as_var_id_set([]) },
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set([]) }
        ],
    );
}

#[test]
fn branch() {
    let label_id = LabelId::from_intern_id(salsa::InternId::from(1u32));
    // All variable names are the concatenation of the indices of the statements that use them.
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(["none", "0", "1", "3", "0_1", "0_3", "1_3", "0_1_3",]),
            &[
                Statement::Sierra(program::GenStatement::Invocation(program::GenInvocation {
                    libfunc_id: "mock".into(),
                    args: as_var_id_vec(["0", "0_1", "0_3", "0_1_3"]),
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
                as_return_statement(["1", "0_1", "1_3", "0_1_3"]),
                Statement::Label(Label { id: label_id }),
                as_return_statement(["3", "0_3", "1_3", "0_1_3"])
            ]
        ),
        vec![
            // Duplicated all variables used both in 0 and in 1 or 3.
            VarsDupsAndDrops {
                dups: as_var_id_set(["0_1", "0_3", "0_1_3"]),
                drops: as_var_id_set(["none"])
            },
            // Ignoring all variables that are available only due to being used in 3.
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set(["3", "0_3"]) },
            // A label line - should not cause any dups or drops.
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set([]) },
            // Ignoring all variables that are available only due to being used in 1.
            VarsDupsAndDrops { dups: as_var_id_set([]), drops: as_var_id_set(["1", "0_1"]) },
        ],
    );
}
