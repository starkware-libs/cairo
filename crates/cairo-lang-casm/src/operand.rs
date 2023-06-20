use std::fmt::Display;

use cairo_lang_utils::bigint::BigIntAsHex;
use parity_scale_codec_derive::{Decode, Encode};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[cfg(test)]
#[path = "operand_test.rs"]
mod test;

#[derive(
    Copy, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize, Encode, Decode, JsonSchema,
)]
pub enum Register {
    #[codec(index = 0)]
    AP,
    #[codec(index = 1)]
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
#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, Encode, Decode, JsonSchema)]
pub enum ResOperand {
    #[codec(index = 0)]
    Deref(CellRef),
    #[codec(index = 1)]
    DoubleDeref(CellRef, i16),
    #[codec(index = 2)]
    Immediate(BigIntAsHex),
    #[codec(index = 3)]
    BinOp(BinOpOperand),
}
impl Display for ResOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResOperand::Deref(operand) => write!(f, "{operand}"),
            ResOperand::DoubleDeref(operand, offset) => write!(f, "[{operand} + {offset}]"),
            ResOperand::Immediate(operand) => write!(f, "{}", operand.value),
            ResOperand::BinOp(operand) => write!(f, "{operand}"),
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

impl<T: Into<BigIntAsHex>> From<T> for ResOperand {
    fn from(imm: T) -> Self {
        ResOperand::Immediate(imm.into())
    }
}

/// Represents an operand of the form [reg + offset].
#[derive(Serialize, Deserialize, Copy, Clone, Debug, Eq, PartialEq, Encode, Decode, JsonSchema)]
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

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, Encode, Decode, JsonSchema)]
pub enum DerefOrImmediate {
    #[codec(index = 0)]
    Deref(CellRef),
    #[codec(index = 1)]
    Immediate(BigIntAsHex),
}
impl Display for DerefOrImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DerefOrImmediate::Deref(operand) => write!(f, "{operand}"),
            DerefOrImmediate::Immediate(operand) => write!(f, "{}", operand.value),
        }
    }
}
impl<T: Into<BigIntAsHex>> From<T> for DerefOrImmediate {
    fn from(x: T) -> Self {
        DerefOrImmediate::Immediate(x.into())
    }
}
impl From<CellRef> for DerefOrImmediate {
    fn from(x: CellRef) -> Self {
        DerefOrImmediate::Deref(x)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, Encode, Decode, JsonSchema)]
pub enum Operation {
    #[codec(index = 0)]
    Add,
    #[codec(index = 1)]
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

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, Encode, Decode, JsonSchema)]
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
