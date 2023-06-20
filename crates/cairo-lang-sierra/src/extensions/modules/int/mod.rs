pub mod unsigned;
pub mod unsigned128;
pub mod unsigned256;
pub mod unsigned512;

/// Operators for integers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntOperator {
    OverflowingAdd,
    OverflowingSub,
}
