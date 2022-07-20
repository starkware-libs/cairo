use lalrpop_util::*;

mod context;
mod edit_state;
pub mod error;
mod extensions;
pub mod graph;
mod ref_value;
mod side_effects;
pub mod simulation;
pub mod soundness;
pub mod utils;

lalrpop_mod!(parser);

pub type ProgramParser = parser::ProgramParser;

#[macro_use]
extern crate itertools;
