use std::fmt::Display;

use crate::operand::{DerefOperand, DerefOrImmediate, ImmediateOperand, Register, ResOperand};

#[cfg(test)]
#[path = "instructions_test.rs"]
mod instructions_test;

// An enum of Cairo instructions.
enum Instruction {
    AssertEq(AssertEqInstruction),
    Jnz(JnzInstruction),
    Jump(JumpInstruction),
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::AssertEq(insn) => write!(f, "{}", insn),
            Instruction::Jnz(insn) => write!(f, "{}", insn),
            Instruction::Jump(insn) => write!(f, "{}", insn),
        }
    }
}

// Represents the instruction "jmp rel/abs".
pub struct JumpInstruction {
    target: DerefOrImmediate,
    relative: bool,
}
impl Display for JumpInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "jmp {} {}", if self.relative { "rel" } else { "abs" }, self.target,)
    }
}

// Represents the instruction "jmp rel <jump_offset> if condition != 0".
pub struct JnzInstruction {
    jump_offset: DerefOrImmediate,
    condition: DerefOperand,
}
impl Display for JnzInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "jmp rel {} if {} != 0", self.jump_offset, self.condition)
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
