use cairo_lang_casm::instructions::{
    AssertEqInstruction, CallInstruction, Instruction, InstructionBody, JnzInstruction,
    JumpInstruction,
};
use cairo_lang_casm::operand::{BinOpOperand, DerefOrImmediate, ResOperand};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_sierra_gas::objects::ConstCost;

use crate::compiler::ConstSegmentInfo;

pub type CodeOffset = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Relocation {
    /// Adds program_offset(StatementIdx) and subtracts the program offset of the casm instruction
    /// that is being relocated.
    RelativeStatementId(StatementIdx),
    /// Adds the offset between the current statement index and the end of the program code
    /// segment.
    EndOfProgram,
    /// Adds the offset of the const type in the const segment, assuming the const segment is at
    /// the end of the program.
    Const(ConcreteTypeId),
}

impl Relocation {
    pub fn apply(
        &self,
        instruction_offset: CodeOffset,
        statement_offsets: &[CodeOffset],
        const_segment_info: &ConstSegmentInfo,
        instruction: &mut Instruction,
    ) {
        let target_pc = match self {
            Relocation::RelativeStatementId(statement_idx) => statement_offsets[statement_idx.0],
            Relocation::EndOfProgram => {
                *statement_offsets.last().unwrap() + const_segment_info.const_segment_size
            }
            Relocation::Const(ty) => {
                *statement_offsets.last().unwrap()
                    + const_segment_info
                        .const_allocations
                        .get(ty)
                        .expect("Const type not found in the const segment.")
                        .offset
            }
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
                value.value += target_pc as i128 - instruction_offset as i128;
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
    const_segment_info: &ConstSegmentInfo,
    instructions: &mut [Instruction],
) {
    let mut program_offset = 0;
    let mut relocations_iter = relocations.iter();
    let mut relocation_entry = relocations_iter.next();
    for (instruction_idx, instruction) in instructions.iter_mut().enumerate() {
        if let Some(RelocationEntry { instruction_idx: relocation_idx, relocation }) =
            relocation_entry
        {
            if *relocation_idx == instruction_idx {
                relocation.apply(
                    program_offset,
                    statement_offsets,
                    const_segment_info,
                    instruction,
                );
                relocation_entry = relocations_iter.next();
            } else {
                assert!(
                    *relocation_idx > instruction_idx,
                    "Relocation for already handled instruction #{relocation_idx} - currently \
                     handling #{instruction_idx}."
                );
            }
        }

        program_offset += instruction.body.op_size();
    }
    assert!(
        relocation_entry.is_none(),
        "No relocations should be left when done with all instructions."
    );
}

/// Represents a list of instructions with their relocations.
#[derive(Default)]
pub struct InstructionsWithRelocations {
    pub instructions: Vec<Instruction>,
    pub relocations: Vec<RelocationEntry>,
    /// The gas cost of executing the instructions.
    pub cost: ConstCost,
}
