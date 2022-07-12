#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Register {
    AP,
    FP,
}

pub enum Operand {
    DerefOperand(DerefOperand),
    ImmediateOperand(ImmediateOperand),
}

#[allow(dead_code)]
impl Operand {
    pub fn format(&self) -> String {
        match self {
            Operand::DerefOperand(operand) => operand.format(),
            Operand::ImmediateOperand(operand) => operand.format(),
        }
    }
}

impl Register {
    pub fn format(&self) -> String {
        match self {
            Register::AP => "ap".to_string(),
            Register::FP => "fp".to_string(),
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

#[test]
fn test_deref_operand_format() {
    assert_eq!(
        DerefOperand {
            register: Register::AP,
            offset: 5
        }
        .format(),
        "[ap + 5]"
    );

    assert_eq!(
        DerefOperand {
            register: Register::FP,
            offset: -3
        }
        .format(),
        "[fp + -3]"
    );
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

#[test]
fn test_immediate_format() {
    assert_eq!(ImmediateOperand { value: 1400 }.format(), "1400");
}
