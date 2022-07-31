use std::fmt::Display;

#[cfg(test)]
#[path = "operand_test.rs"]
mod operand_test;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

// Represents the rhs operand of an assert equal instruction.
pub enum ResOperand {
    DerefOperand(DerefOperand),
    ImmediateOperand(ImmediateOperand),
    BinOpOperand(BinOpOperand),
}
impl Display for ResOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResOperand::DerefOperand(operand) => write!(f, "{}", operand),
            ResOperand::ImmediateOperand(operand) => write!(f, "{}", operand),
            ResOperand::BinOpOperand(operand) => write!(f, "{}", operand),
        }
    }
}

// Represents an operand of the form [reg + offset].
pub struct DerefOperand {
    pub register: Register,
    pub offset: i16,
}
impl Display for DerefOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} + {}]", self.register, self.offset)
    }
}

pub struct ImmediateOperand {
    // TODO(ilya, 10/10/2022): What type do we want to use here.
    pub value: i128,
}
impl Display for ImmediateOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub enum DerefOrImmediate {
    DerefOperand(DerefOperand),
    ImmediateOperand(ImmediateOperand),
}
impl Display for DerefOrImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DerefOrImmediate::DerefOperand(operand) => write!(f, "{}", operand),
            DerefOrImmediate::ImmediateOperand(operand) => write!(f, "{}", operand),
        }
    }
}

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

pub struct BinOpOperand {
    pub op: Operation,
    pub a: DerefOperand,
    pub b: DerefOrImmediate,
}
impl Display for BinOpOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.a, self.op, self.b)
    }
}
