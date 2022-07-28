use lalrpop_util::*;

pub mod program;

lalrpop_mod!(parser);

pub type ProgramParser = parser::ProgramParser;