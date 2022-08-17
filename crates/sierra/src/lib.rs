use lalrpop_util::lalrpop_mod;

pub mod edit_state;
pub mod extensions;
pub mod fmt;
pub mod ids;
pub mod mem_cell;
pub mod program;
pub mod program_registry;

lalrpop_mod!(
    #[allow(clippy::all, unused_extern_crates)]
    parser
);

pub type ProgramParser = parser::ProgramParser;
