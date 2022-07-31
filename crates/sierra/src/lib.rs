use lalrpop_util::*;

mod edit_state;
mod extensions;
pub mod fmt;
pub mod program;
pub mod simulation;

lalrpop_mod!(
    #[allow(clippy::all, unused_extern_crates)]
    parser
);

pub type ProgramParser = parser::ProgramParser;

#[macro_use]
extern crate itertools;
