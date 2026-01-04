use crate::constants::TAB;

/// Generates a string with `n` tabs.
pub fn tabs(n: usize) -> String {
    TAB.repeat(n)
}
