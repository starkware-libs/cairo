use cairo_lang_casm::instructions::{
    AssertEqInstruction, CallInstruction, Instruction, InstructionBody, JnzInstruction,
    JumpInstruction,
};
use cairo_lang_casm::operand::{BinOpOperand, DerefOrImmediate, ResOperand};
use cairo_lang_sierra::program::StatementIdx;

type CodeOffset = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Relocation {
    /// Adds program_offset(StatementIdx) and subtracts the program offset of the casm instruction
    /// that is being relocated.
    RelativeStatementId(StatementIdx),
    /// Adds the offset between the current statement index and the end of the program code
    /// segment.
    EndOfProgram,
}

impl Relocation {
    pub fn apply(
        &self,
        instruction_offset: CodeOffset,
        statement_offsets: &[CodeOffset],
        instruction: &mut Instruction,
    ) {
        let target_pc = match self {
            Relocation::RelativeStatementId(statement_idx) => statement_offsets[statement_idx.0],
            Relocation::EndOfProgram => *statement_offsets.last().unwrap(),
        };

        match instruction {
            Instruction {
                body:
                    InstructionBody::Call(CallInstruction {
                        target: DerefOrImmediate::Immediate(value),
                        relative: true,
                    }),
                inc_ap: false,
                ..
            }
            | Instruction {
                body:
                    InstructionBody::Jnz(JnzInstruction {
                        jump_offset: DerefOrImmediate::Immediate(value),
                        condition: _,
                    }),
                ..
            }
            | Instruction {
                body:
                    InstructionBody::Jump(JumpInstruction {
                        target: DerefOrImmediate::Immediate(value),
                        relative: true,
                    }),
                inc_ap: false,
                ..
            }
            | Instruction {
                body:
                    InstructionBody::AssertEq(AssertEqInstruction {
                        b:
                            ResOperand::BinOp(BinOpOperand {
                                b: DerefOrImmediate::Immediate(value),
                                ..
                            }),
                        ..
                    }),
                ..
            } => {
                *value += target_pc as i128 - instruction_offset as i128;
            }
            _ => panic!("Bad relocation."),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RelocationEntry {
    /// The index of the casm instruction that needs to be relocated.
    pub instruction_idx: CodeOffset,
    /// The relocation the needs to be applied.
    pub relocation: Relocation,
}

/// Applies 'relocations' to 'instructions'.
///
/// This is currently O(instruction.len()) rather then O(relocations.len()),
/// But another pass is required anyhow to generate the bytecode and the relocations
/// can be applied during that pass.
pub fn relocate_instructions(
    relocations: &[RelocationEntry],
    statement_offsets: &[usize],
    instructions: &mut [Instruction],
) {
    let mut program_offset = 0;
    let mut relocations_iter = relocations.iter();
    let mut relocation_entry = relocations_iter.next();
    for (instruction_idx, instruction) in instructions.iter_mut().enumerate() {
        match relocation_entry {
            Some(RelocationEntry { instruction_idx: relocation_idx, relocation })
                if *relocation_idx == instruction_idx =>
            {
                relocation.apply(program_offset, statement_offsets, instruction);
                relocation_entry = relocations_iter.next();
            }
            _ => (),
        };

        program_offset += instruction.body.op_size();
    }
}
