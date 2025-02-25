/// A compilation flag.
#[derive(PartialEq, Eq, Debug)]
pub enum Flag {
    /// Whether automatically add `withdraw_gas` calls in code cycles.
    /// Default is true - automatically add.
    ///
    /// Additionally controls addition of `redeposit_gas` which happens on default.
    AddWithdrawGas(bool),
    NumericMatchOptimizationMinArmsThreshold(usize),
    /// Whether to add panic back trace handling to the generated code.
    ///
    /// Default is false - do not add, as it won't be used in production.
    PanicBacktrace(bool),
}
