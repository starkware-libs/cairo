use serde::{Deserialize, Serialize};

use crate::db::FilesGroup;
use crate::ids::{FlagId, FlagLongId};

/// A compilation flag.
#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize, Hash, salsa::Update)]
pub enum Flag {
    /// Whether to automatically add `withdraw_gas` calls in code cycles.
    /// Default is true - automatically add.
    ///
    /// Additionally controls addition of `redeposit_gas` which happens on default.
    AddWithdrawGas(bool),
    NumericMatchOptimizationMinArmsThreshold(usize),
    /// Whether to add panic backtrace handling to the generated code.
    ///
    /// Default is false - do not add, as it won't be used in production.
    PanicBacktrace(bool),
    /// Whether to use unsafe_panic in the generated code.
    ///
    /// Default is false as it makes panic unprovable.
    UnsafePanic(bool),
    /// Whether to enable the local_into_box optimization.
    ///
    /// Default is true.
    LocalIntoBoxOptimization(bool),
}

/// Returns the value of the `unsafe_panic` flag, or `false` if the flag is not set.
pub fn flag_unsafe_panic(db: &dyn salsa::Database) -> bool {
    let flag = FlagId::new(db, FlagLongId("unsafe_panic".into()));
    if let Some(flag) = db.get_flag(flag) { *flag == Flag::UnsafePanic(true) } else { false }
}

/// Returns whether the local_into_box optimization is enabled (default: true).
pub fn flag_local_into_box_optimization(db: &dyn salsa::Database) -> bool {
    let flag = FlagId::new(db, FlagLongId("local_into_box_optimization".into()));
    db.get_flag(flag).is_none_or(|f| *f != Flag::LocalIntoBoxOptimization(false))
}
