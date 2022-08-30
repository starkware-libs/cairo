use sierra::program::StatementIdx;

#[derive(Debug, Eq, PartialEq)]
pub enum Relocation {
    // Adds program_offset(StatementId) and subtracts the program offset of the instruction
    // that is being relocated.
    RelativeStatementID(StatementIdx),
}

#[derive(Debug, Eq, PartialEq)]
pub struct RelocationEntry {
    // The index of the instruction that needs to be relocated.
    pub instruction_idx: usize,
    // The relocation the needs to be applied.
    pub relocation: Relocation,
}
