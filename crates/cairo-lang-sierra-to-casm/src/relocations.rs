use cairo_lang_casm::instructions::{
    AssertEqInstruction, CallInstruction, Instruction, InstructionBody, JnzInstruction,
    JumpInstruction,
};
use cairo_lang_casm::operand::{BinOpOperand, DerefOrImmediate, ResOperand};
use cairo_lang_sierra::ids::ConcreteTypeId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_sierra_gas::objects::ConstCost;

use crate::compiler::{ConstSegment, ConstsInfo, SierraStatementDebugInfo};

pub type CodeOffset = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Relocation {
    /// Adds program_offset(StatementIdx) and subtracts the program offset of the CASM instruction
    /// that is being relocated.
    RelativeStatementId(StatementIdx),
    /// Adds the offset of the constant segment with this id.
    SegmentStart(u32),
    /// Adds the offset of the constant value in the const segments.
    ConstStart(u32, ConcreteTypeId),
    /// Adds the offset of the circuit with the given type.
    CircuitStart(ConcreteTypeId),
    /// Adds the offset between the current statement index and the end of the program code
    /// segment (which includes the const segment at its end).
    EndOfProgram,
}

impl Relocation {
    pub fn apply(
        &self,
        instruction_offset: CodeOffset,
        sierra_statement_info: &[SierraStatementDebugInfo],
        consts_info: &ConstsInfo,
        instruction: &mut Instruction,
    ) {
        // The offset of the beginning of the const segments.
        let const_segments_offset = || sierra_statement_info.last().unwrap().end_offset;
        // The offset of the beginning of the given segment.
        let segment_start_offset =
            |segment: &ConstSegment| const_segments_offset() + segment.segment_offset;
        // The offset of given const ty within the given segment.
        let const_offset = |segment_idx: &u32, ty| {
            let segment = consts_info.segments.get(segment_idx).expect("Segment not found.");
            segment_start_offset(segment)
                + segment.const_offset.get(ty).expect("Const type not found in const segments.")
        };
        let target_pc = match self {
            Relocation::RelativeStatementId(idx) => sierra_statement_info[idx.0].start_offset,
            Relocation::SegmentStart(segment_idx) => segment_start_offset(
                consts_info.segments.get(segment_idx).expect("Segment not found."),
            ),
            Relocation::ConstStart(segment_idx, ty) => const_offset(segment_idx, ty),
            Relocation::EndOfProgram => const_segments_offset() + consts_info.total_segments_size,
            Relocation::CircuitStart(circ_ty) => const_offset(
                consts_info.circuit_segments.get(circ_ty).expect("Circuit not found"),
                circ_ty,
            ),
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
    /// The index of the CASM instruction that needs to be relocated.
    pub instruction_idx: CodeOffset,
    /// The relocation that needs to be applied.
    pub relocation: Relocation,
}

/// Applies 'relocations' to 'instructions'.
///
/// This is currently O(instruction.len()) rather than O(relocations.len()),
/// But another pass is required anyhow to generate the bytecode and the relocations
/// can be applied during that pass.
pub fn relocate_instructions(
    relocations: &[RelocationEntry],
    sierra_statement_info: &[SierraStatementDebugInfo],
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
                relocation.apply(program_offset, sierra_statement_info, consts_info, instruction);
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
    assert_eq!(
        relocation_entry, None,
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
