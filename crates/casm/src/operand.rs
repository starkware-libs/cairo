#[cfg(test)]
#[path = "operand_test.rs"]
mod operand_test;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Register {
    AP,
    FP,
}
impl Register {
    pub fn format(&self) -> String {
        match self {
            Register::AP => "ap".to_string(),
            Register::FP => "fp".to_string(),
        }
    }
}

pub enum Operand {
    DerefOperand(DerefOperand),
    ImmediateOperand(ImmediateOperand),
    BinOpOperand(BinOpOperand),
}

impl Operand {
    pub fn format(&self) -> String {
        match self {
            Operand::DerefOperand(operand) => operand.format(),
            Operand::ImmediateOperand(operand) => operand.format(),
            Operand::BinOpOperand(operand) => operand.format(),
       }
    }
}

pub struct DerefOperand {
    pub register: Register,
    pub offset: i16,
}

impl DerefOperand {
    pub fn format(&self) -> String {
        format!("[{} + {}]", self.register.format(), self.offset)
    }
}

pub struct ImmediateOperand {
    // TODO(ilya, 10/10/2022): What type do we want to use here.
    pub value: i128,
}

impl ImmediateOperand {
    pub fn format(&self) -> String {
        self.value.to_string()
    }
}

pub enum SimpleOperand {
    DerefOperand(DerefOperand),
    ImmediateOperand(ImmediateOperand),
}
impl SimpleOperand {
    pub fn format(&self) -> String {
        match self {
            SimpleOperand::DerefOperand(operand) => operand.format(),
            SimpleOperand::ImmediateOperand(operand) => operand.format(),
        }
    }
}

pub enum Operation {
    Add,
    Mul,
}
impl Operation {
    pub fn format(&self) -> String {
        match self {
            Operation::Add => "+".to_string(),
            Operation::Mul => "*".to_string(),
        }
    }
}

pub struct BinOpOperand {
    pub op: Operation,
    pub a: DerefOperand,
    pub b: SimpleOperand,
}
impl BinOpOperand {
    pub fn format(&self) -> String {
        format!(
            "{} {} {}",
            self.a.format(),
            self.op.format(),
            self.b.format(),
        )
    }
}
