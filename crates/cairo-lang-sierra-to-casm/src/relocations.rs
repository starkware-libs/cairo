use cairo_lang_casm::instructions::{
    AssertEqInstruction, CallInstruction, Instruction, InstructionBody, JnzInstruction,
    JumpInstruction,
};
use cairo_lang_casm::operand::{BinOpOperand, DerefOrImmediate, ResOperand};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_sierra_gas::objects::ConstCost;

use crate::compiler::ConstsInfo;

pub type CodeOffset = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Relocation {
    /// Adds program_offset(StatementIdx) and subtracts the program offset of the casm instruction
    /// that is being relocated.
    RelativeStatementId(StatementIdx),
    /// Adds the offset of the constant segment with this id.
    SegmentStart(u32),
    /// Adds the offset of the constant value in the const segments.
    ConstStart(u32, ConcreteTypeId),
    /// Adds the offset between the current statement index and the end of the program code
    /// segment (which includes the const segment at its end).
    EndOfProgram,
}

impl Relocation {
    pub fn apply(
        &self,
        instruction_offset: CodeOffset,
        statement_offsets: &[CodeOffset],
        consts_info: &ConstsInfo,
        instruction: &mut Instruction,
    ) {
        let target_pc = match self {
            Relocation::RelativeStatementId(statement_idx) => statement_offsets[statement_idx.0],
            Relocation::SegmentStart(segment_index) => {
                let segment = consts_info.segments.get(segment_index).expect("Segment not found.");
                *statement_offsets.last().unwrap() + segment.segment_offset
            }
            Relocation::ConstStart(segment_index, ty) => {
                let segment = consts_info.segments.get(segment_index).expect("Segment not found.");
                *statement_offsets.last().unwrap()
                    + segment.segment_offset
                    + segment.const_offset.get(ty).expect("Const type not found in const segments.")
            }
            Relocation::EndOfProgram => {
                *statement_offsets.last().unwrap() + consts_info.total_segments_size
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
    consts_info: &ConstsInfo,
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
                relocation.apply(program_offset, statement_offsets, consts_info, instruction);
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
