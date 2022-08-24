use std::fmt::Display;

use crate::operand::{DerefOperand, DerefOrImmediate, ImmediateOperand, Register, ResOperand};

#[cfg(test)]
#[path = "instructions_test.rs"]
mod test;

// An enum of Cairo instructions.
#[derive(Debug, Eq, PartialEq)]
pub enum InstructionBody {
    AssertEq(AssertEqInstruction),
    Call(CallInstruction),
    Jnz(JnzInstruction),
    Jump(JumpInstruction),
    Ret(RetInstruction),
}
impl InstructionBody {
    pub fn op_size(&self) -> usize {
        // TOOD(spapini): Make this correct.
        match self {
            InstructionBody::AssertEq(insn) => insn.op_size(),
            _ => 1,
        }
    }
}
impl Display for InstructionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionBody::AssertEq(insn) => write!(f, "{}", insn),
            InstructionBody::Call(insn) => write!(f, "{}", insn),
            InstructionBody::Jnz(insn) => write!(f, "{}", insn),
            InstructionBody::Jump(insn) => write!(f, "{}", insn),
            InstructionBody::Ret(insn) => write!(f, "{}", insn),
        }
    }
}

/// Represents an instruction, including the ap++ flag (inc_ap).
#[derive(Debug, Eq, PartialEq)]
pub struct Instruction {
    pub body: InstructionBody,
    pub inc_ap: bool,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.body)?;
        if self.inc_ap {
            write!(f, ", ap++")?
        };
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct CallInstruction {
    pub target: DerefOrImmediate,
    pub relative: bool,
}
impl Display for CallInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "call {} {}", if self.relative { "rel" } else { "abs" }, self.target,)
    }
}

// Represents the InstructionBody "jmp rel/abs".
#[derive(Debug, Eq, PartialEq)]
pub struct JumpInstruction {
    pub target: DerefOrImmediate,
    pub relative: bool,
}
impl Display for JumpInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "jmp {} {}", if self.relative { "rel" } else { "abs" }, self.target,)
    }
}

// Represents the InstructionBody "jmp rel <jump_offset> if condition != 0".
#[derive(Debug, Eq, PartialEq)]
pub struct JnzInstruction {
    pub jump_offset: DerefOrImmediate,
    pub condition: DerefOperand,
}
impl Display for JnzInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "jmp rel {} if {} != 0", self.jump_offset, self.condition)
    }
}

// Represents the InstructionBody "a = b" for two operands a, b.
#[derive(Debug, Eq, PartialEq)]
pub struct AssertEqInstruction {
    pub a: DerefOperand,
    pub b: ResOperand,
}
impl AssertEqInstruction {
    pub fn op_size(&self) -> usize {
        match &self.b {
            ResOperand::Deref(_) => 1,
            ResOperand::Immediate(_) => 2,
            ResOperand::BinOp(op) => match op.b {
                DerefOrImmediate::Immediate(_) => 2,
                DerefOrImmediate::Deref(_) => 1,
            },
        }
    }
}
impl Display for AssertEqInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.a, self.b)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RetInstruction {}
impl Display for RetInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ret")
    }
}
