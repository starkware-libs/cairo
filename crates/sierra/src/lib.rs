//! Sierra is an intermediate representation between high level Cairo and compilation targets,
//! such as CASM. Sierra code is guaranteed to be "safe"* by construction.
//! Sierra has a primitive, yet rich typing system to express all high level code while guaranteeing
//! safety and allowing for efficient compilation down to the target.
//!
//! Safety - this means a few things:
//! 1. There are no "panics" / "runtime errors". Every function is guaranteed to return.
//! 2. There are no infinite loops. Moreover, every program "counts" its own steps, and returns when
//!    the limit is reached.
//! 3. Builtin library functions are always used correctly.

use lalrpop_util::lalrpop_mod;

pub mod edit_state;
pub mod extensions;
pub mod fmt;
pub mod ids;
pub mod program;
pub mod program_registry;
pub mod serialization;
pub mod simulation;
#[cfg(test)]
mod test_utils;

lalrpop_mod!(
    #[allow(clippy::all, unused_extern_crates)]
    parser
);

pub type ProgramParser = parser::ProgramParser;
pub type ConcreteLibFuncLongIdParser = parser::ConcreteLibFuncLongIdParser;
pub type ConcreteTypeLongIdParser = parser::ConcreteTypeLongIdParser;
