use std::fmt::Display;

use num_bigint::BigInt;

#[cfg(test)]
#[path = "operand_test.rs"]
mod test;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Register {
    AP,
    FP,
}
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::AP => write!(f, "ap"),
            Register::FP => write!(f, "fp"),
        }
    }
}

// Represents the rhs operand of an assert equal InstructionBody.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ResOperand {
    Deref(CellRef),
    DoubleDeref(CellRef, i16),
    Immediate(BigInt),
    BinOp(BinOpOperand),
}
impl Display for ResOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResOperand::Deref(operand) => write!(f, "{}", operand),
            ResOperand::DoubleDeref(operand, offset) => write!(f, "[{} + {}]", operand, offset),
            ResOperand::Immediate(operand) => write!(f, "{}", operand),
            ResOperand::BinOp(operand) => write!(f, "{}", operand),
        }
    }
}
impl From<DerefOrImmediate> for ResOperand {
    fn from(x: DerefOrImmediate) -> Self {
        match x {
            DerefOrImmediate::Deref(deref) => ResOperand::Deref(deref),
            DerefOrImmediate::Immediate(imm) => ResOperand::Immediate(imm),
        }
    }
}
impl From<i128> for ResOperand {
    fn from(imm: i128) -> Self {
        ResOperand::Immediate(BigInt::from(imm))
    }
}
impl From<i32> for ResOperand {
    fn from(imm: i32) -> Self {
        ResOperand::Immediate(BigInt::from(imm))
    }
}
impl From<usize> for ResOperand {
    fn from(imm: usize) -> Self {
        ResOperand::Immediate(BigInt::from(imm))
    }
}

/// Represents an operand of the form [reg + offset].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CellRef {
    pub register: Register,
    pub offset: i16,
}
impl Display for CellRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} + {}]", self.register, self.offset)
    }
}

/// Returns an AP DerefOperand with the given offset.
pub fn ap_cell_ref(offset: i16) -> CellRef {
    CellRef { register: Register::AP, offset }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DerefOrImmediate {
    Deref(CellRef),
    Immediate(BigInt),
}
impl Display for DerefOrImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DerefOrImmediate::Deref(operand) => write!(f, "{}", operand),
            DerefOrImmediate::Immediate(operand) => write!(f, "{}", operand),
        }
    }
}
impl<T: Into<BigInt>> From<T> for DerefOrImmediate {
    fn from(x: T) -> Self {
        DerefOrImmediate::Immediate(x.into())
    }
}
impl From<CellRef> for DerefOrImmediate {
    fn from(x: CellRef) -> Self {
        DerefOrImmediate::Deref(x)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operation {
    Add,
    Mul,
}
impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Add => write!(f, "+"),
            Operation::Mul => write!(f, "*"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinOpOperand {
    pub op: Operation,
    pub a: CellRef,
    pub b: DerefOrImmediate,
}
impl Display for BinOpOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.a, self.op, self.b)
    }
}
