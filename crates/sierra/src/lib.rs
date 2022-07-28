use lalrpop_util::*;

pub mod program;
pub mod edit_state;

lalrpop_mod!(parser);

pub type ProgramParser = parser::ProgramParser;