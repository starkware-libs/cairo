/// A compilation flag.
#[derive(PartialEq, Eq, Debug)]
pub enum Flag {
    /// Whether automatically add `withdraw_gas` calls in code cycles.
    /// Default is true - automatically add.
    AddWithdrawGas(bool),
    /// Whether automatically add `redeposit_gas` calls on stores of the gas builtin.
    /// Default is false - automatically add.
    AddRedepositGas(bool),
    NumericMatchOptimizationMinArmsThreshold(usize),
}
