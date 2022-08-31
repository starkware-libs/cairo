use casm::instructions::{CallInstruction, Instruction, InstructionBody};
use casm::operand::{DerefOrImmediate, ImmediateOperand};
use sierra::program::StatementIdx;

type CodeOffset = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Relocation {
    // Adds program_offset(StatementId) and subtracts the program offset of the instruction
    // that is being relocated.
    RelativeStatementID(StatementIdx),
}

impl Relocation {
    pub fn apply(
        &self,
        instruction_offset: CodeOffset,
        statement_offsets: &[CodeOffset],
        instruction: &mut Instruction,
    ) {
        match self {
            Relocation::RelativeStatementID(statement_id) => match instruction {
                Instruction {
                    body:
                        InstructionBody::Call(CallInstruction {
                            target: DerefOrImmediate::Immediate(ImmediateOperand { value }),
                            relative: true,
                        }),
                    inc_ap: false,
                } => {
                    *value +=
                        statement_offsets[statement_id.0] as i128 - instruction_offset as i128;
                }
                _ => panic!("Bad relocation."),
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RelocationEntry {
    // The index of the instruction that needs to be relocated.
    pub instruction_idx: CodeOffset,
    // The relocation the needs to be applied.
    pub relocation: Relocation,
}

/// Applies 'relocations' to 'instructions'.
///
/// This is currently O(instruction.len()) rather then O(relocations.len()),
/// But it can be does as single pass that generates the bytecode and applies the relocations.
pub fn relocate_instructions(
    relocations: Vec<RelocationEntry>,
    statement_offsets: Vec<usize>,
    instructions: &mut [Instruction],
) {
    let mut program_offset = 0;
    for (instruction_idx, instruction) in instructions.iter_mut().enumerate() {
        match relocations.get(0) {
            Some(RelocationEntry { instruction_idx: relocation_idx, relocation })
                if *relocation_idx == instruction_idx =>
            {
                relocation.apply(program_offset, &statement_offsets, instruction)
            }

            _ => (),
        };

        program_offset += instruction.body.op_size();
    }
}
