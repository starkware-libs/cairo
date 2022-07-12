use std::fmt::Display;

use crate::operand::{DerefOperand, ImmediateOperand, Operand, Register};

#[cfg(test)]
#[path = "instructions_test.rs"]
mod instructions_test;

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
    target: Operand,
    relative: bool,
}
impl Display for JumpInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "jmp {} {}",
            if self.relative { "rel" } else { "abs" },
            self.target,
        )
    }
}

pub struct AssertEqInstruction {
    a: Operand,
    b: Operand,
}
impl Display for AssertEqInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{} = {}", self.a, self.b);
    }
}
