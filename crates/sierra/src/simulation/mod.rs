use thiserror::Error;

pub mod core;
pub mod mem_cell;
#[cfg(test)]
mod tests;

/// Error occurring while testing extension inputs.
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ExtensionSimulationError {
    #[error("Expected different number of arguments")]
    WrongNumberOfArgs,
    #[error("Expected a different memory layout")]
    MemoryLayoutMismatch,
}
