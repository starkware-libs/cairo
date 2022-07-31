use lalrpop_util::*;

pub mod fmt;
pub mod program;

lalrpop_mod!(
    #[allow(clippy::all, unused_extern_crates)]
    parser
);

pub type ProgramParser = parser::ProgramParser;
