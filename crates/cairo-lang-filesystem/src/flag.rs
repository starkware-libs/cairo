/// A compilation flag.
#[derive(PartialEq, Eq, Debug)]
pub enum Flag {
    /// Whether to skip automatically adding `withdraw_gas` calls in code cycles.
    /// Default is to automatically add.
    SkipAddWithdrawGas(bool),
}
