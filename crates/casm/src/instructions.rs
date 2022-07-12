use crate::operand::{DerefOperand, ImmediateOperand, Operand, Register};

#[cfg(test)]
#[path = "instructions_test.rs"]
mod instructions_test;

enum Instruction {
    JumpInstruction(JumpInstruction),
    AssertEqInstruction(AssertEqInstruction),
}

impl Instruction {
    fn format(&self) -> String {
        match self {
            Instruction::JumpInstruction(insn) => insn.format(),
            Instruction::AssertEqInstruction(insn) => insn.format(),
        }
    }
}

// Represents the instruction "jmp rel/abs".
pub struct JumpInstruction {
    target: Operand,
    relative: bool,
}

impl JumpInstruction {
    fn format(&self) -> String {
        format!(
            "jmp {} {}",
            if self.relative { "rel" } else { "abs" },
            self.target.format()
        )
    }
}

pub struct AssertEqInstruction {
    a: Operand,
    b: Operand,
}

impl AssertEqInstruction {
    fn format(&self) -> String {
        return format!("{} = {}", self.a.format(), self.b.format());
    }
}
