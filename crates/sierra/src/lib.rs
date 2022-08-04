use lalrpop_util::lalrpop_mod;

pub mod edit_state;
pub mod fmt;
pub mod program;

lalrpop_mod!(
    #[allow(clippy::all, unused_extern_crates)]
    parser
);

pub type ProgramParser = parser::ProgramParser;
