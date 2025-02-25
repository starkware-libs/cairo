use serde::{Deserialize, Serialize};

/// A compilation flag.
#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub enum Flag {
    /// Whether automatically add `withdraw_gas` calls in code cycles.
    /// Default is true - automatically add.
    ///
    /// Additionally controls addition of `redeposit_gas` which happens on default.
    AddWithdrawGas(bool),
    NumericMatchOptimizationMinArmsThreshold(usize),
}
