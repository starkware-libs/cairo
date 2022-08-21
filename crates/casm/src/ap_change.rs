use crate::operand::{
    BinOpOperand, DerefOperand, DerefOrImmediate, ImmediateOperand, Register, ResOperand,
};

#[cfg(test)]
#[path = "ap_change_test.rs"]
mod ap_change_test;

pub trait ApChange {
    fn apply_ap_change(self, ap_change: i16) -> Self;
}

impl ApChange for ResOperand {
    fn apply_ap_change(self, ap_change: i16) -> Self {
        match self {
            ResOperand::Deref(operand) => ResOperand::Deref(operand.apply_ap_change(ap_change)),
            ResOperand::Immediate(operand) => {
                ResOperand::Immediate(operand.apply_ap_change(ap_change))
            }
            ResOperand::BinOp(operand) => ResOperand::BinOp(operand.apply_ap_change(ap_change)),
        }
    }
}

impl ApChange for DerefOperand {
    fn apply_ap_change(self, ap_change: i16) -> Self {
        match self {
            DerefOperand { register: Register::AP, offset } => {
                DerefOperand { register: Register::AP, offset: offset - ap_change }
            }
            DerefOperand { register: Register::FP, offset: _ } => self,
        }
    }
}

impl ApChange for ImmediateOperand {
    fn apply_ap_change(self, _ap_change: i16) -> Self {
        return self;
    }
}

impl ApChange for DerefOrImmediate {
    fn apply_ap_change(self, ap_change: i16) -> Self {
        match self {
            DerefOrImmediate::Deref(operand) => {
                DerefOrImmediate::Deref(operand.apply_ap_change(ap_change))
            }
            DerefOrImmediate::Immediate(operand) => {
                DerefOrImmediate::Immediate(operand.apply_ap_change(ap_change))
            }
        }
    }
}

impl ApChange for BinOpOperand {
    fn apply_ap_change(self, ap_change: i16) -> Self {
        return BinOpOperand {
            op: self.op,
            a: self.a.apply_ap_change(ap_change),
            b: self.b.apply_ap_change(ap_change),
        };
    }
}
