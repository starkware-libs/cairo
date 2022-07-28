use lalrpop_util::*;

pub mod edit_state;
pub mod extensions;
pub mod program;
pub mod simulation;

lalrpop_mod!(parser);

pub type ProgramParser = parser::ProgramParser;

#[macro_use]
extern crate itertools;
