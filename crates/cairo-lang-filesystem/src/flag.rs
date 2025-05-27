use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::db::FilesGroup;
use crate::ids::FlagLongId;

/// A compilation flag.
#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize, Hash, salsa::Update)]
pub enum Flag {
    /// Whether automatically add `withdraw_gas` calls in code cycles.
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
    /// Default is false as it make panic unprovable.
    UnsafePanic(bool),
}

/// Returns the value of the `unsafe_panic` flag, or `false` if the flag is not set.
pub fn flag_unsafe_panic(db: &dyn FilesGroup) -> bool {
    let flag = db.intern_flag(FlagLongId(SmolStr::from("unsafe_panic")));
    if let Some(flag) = db.get_flag(flag) { *flag == Flag::UnsafePanic(true) } else { false }
}
