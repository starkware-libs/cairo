/// A compilation flag.
#[derive(PartialEq, Eq, Debug)]
pub enum Flag {
    /// Whether automatically add `withdraw_gas` calls in code cycles.
    /// Default is true - automatically add.
    AddWithdrawGas(bool),
    NumericMatchOptimizationMinArmsThreshold(usize),
}
