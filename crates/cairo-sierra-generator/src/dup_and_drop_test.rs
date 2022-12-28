use sierra::ids::VarId;
use sierra::program;
use test_log::test;
use utils::ordered_hash_set::OrderedHashSet;

use super::{calculate_statement_dups_and_drops, VarsDupsAndDrops};
use crate::pre_sierra;
use crate::test_utils::{
    as_var_id_vec, dummy_label, dummy_simple_branch, SierraGenDatabaseForTesting,
};
use crate::utils::{return_statement, simple_statement};

/// Returns a vector of params based on inputs mapped into variable ids.
/// The type of the params is ignored.
fn params(ids: &[&str]) -> Vec<program::Param> {
    ids.iter().map(|id| program::Param { id: (*id).into(), ty: "NotUsed".into() }).collect()
}

/// Returns a simple non-branch libfunc invocation mapping from a set of input var ids to a set of
/// output var ids.
fn mock_non_branch_statement(inputs: &[&str], outputs: &[&str]) -> pre_sierra::Statement {
    simple_statement("mock".into(), &as_var_id_vec(inputs), &as_var_id_vec(outputs))
}

/// Returns a return statement returning the given var ids.
fn as_return_statement(ids: &[&str]) -> pre_sierra::Statement {
    return_statement(as_var_id_vec(ids))
}

/// Returns a set of var ids generated from the inputs as var ids.
fn as_var_id_set(ids: &[&str]) -> OrderedHashSet<VarId> {
    ids.iter().map(|id| (*id).into()).collect()
}

#[test]
fn all_used_no_d() {
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(&["arg1", "arg2"]),
            &[
                mock_non_branch_statement(&["arg1"], &["mid1"]),
                mock_non_branch_statement(&["arg2"], &["mid2"]),
                mock_non_branch_statement(&["mid1", "mid2"], &["res"]),
                as_return_statement(&["res"])
            ]
        ),
        vec![
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&[]) },
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&[]) },
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&[]) },
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&[]) }
        ],
    );
}

#[test]
fn ignore_unused() {
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(&["arg1", "arg2"]),
            &[
                mock_non_branch_statement(&["arg1"], &["res1", "res2"]),
                as_return_statement(&["res1"])
            ]
        ),
        vec![
            // arg2 is no longer used and can be dropped before the first statement.
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&["arg2"]) },
            // res2 is no longer used and can be dropped before the second statement.
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&["res2"]) }
        ],
    );
}

#[test]
fn dup_reused() {
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(&["arg"]),
            &[mock_non_branch_statement(&["arg"], &["res"]), as_return_statement(&["arg", "res"])]
        ),
        vec![
            // arg is used by both statements and therefore should be duplicated.
            VarsDupsAndDrops { dups: as_var_id_set(&["arg"]), drops: as_var_id_set(&[]) },
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&[]) }
        ],
    );
}

#[test]
fn branch() {
    let db = SierraGenDatabaseForTesting::default();
    // All variable names are the concatenation of the indices of the statements that use them.
    assert_eq!(
        calculate_statement_dups_and_drops(
            &params(&["none", "0", "1", "3", "0_1", "0_3", "1_3", "0_1_3",]),
            &[
                dummy_simple_branch(&db, "mock", &["0", "0_1", "0_3", "0_1_3"], 1),
                as_return_statement(&["1", "0_1", "1_3", "0_1_3"]),
                dummy_label(1),
                as_return_statement(&["3", "0_3", "1_3", "0_1_3"])
            ]
        ),
        vec![
            // Duplicated all variables used both in 0 and in 1 or 3.
            VarsDupsAndDrops {
                dups: as_var_id_set(&["0_1", "0_3", "0_1_3"]),
                drops: as_var_id_set(&["none"])
            },
            // Ignoring all variables that are available only due to being used in 3.
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&["3", "0_3"]) },
            // A label line - should not cause any dups or drops.
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&[]) },
            // Ignoring all variables that are available only due to being used in 1.
            VarsDupsAndDrops { dups: as_var_id_set(&[]), drops: as_var_id_set(&["1", "0_1"]) },
        ],
    );
}
