use std::fmt::Display;

use crate::operand::{DerefOperand, ImmediateOperand, Register, ResOperand};

#[cfg(test)]
#[path = "instructions_test.rs"]
mod instructions_test;

// An enum of Cairo instructions.
enum Instruction {
    JumpInstruction(JumpInstruction),
    AssertEqInstruction(AssertEqInstruction),
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::JumpInstruction(insn) => write!(f, "{}", insn),
            Instruction::AssertEqInstruction(insn) => write!(f, "{}", insn),
        }
    }
}

// Represents the instruction "jmp rel/abs".
pub struct JumpInstruction {
    target: ResOperand,
    relative: bool,
}
impl Display for JumpInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "jmp {} {}", if self.relative { "rel" } else { "abs" }, self.target,)
    }
}

// Represents the instruction "a = b" for two operands a, b.
pub struct AssertEqInstruction {
    a: DerefOperand,
    b: ResOperand,
}
impl Display for AssertEqInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.a, self.b)
    }
}
