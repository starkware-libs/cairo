use super::{BinOpOperand, DerefOrImmediate, Operation};
use crate::operand::{DerefOperand, ImmediateOperand, Register};

#[test]
fn test_deref_operand_format() {
    assert_eq!(format!("{}", DerefOperand { register: Register::AP, offset: 5 }), "[ap + 5]");

    assert_eq!(format!("{}", DerefOperand { register: Register::FP, offset: -3 }), "[fp + -3]");
}

#[test]
fn test_immediate_format() {
    assert_eq!(format!("{}", ImmediateOperand { value: 1400 }), "1400");
}

#[test]
fn test_bin_op_format() {
    let a = DerefOperand { register: Register::FP, offset: -3 };
    let b = ImmediateOperand { value: 1400 };

    let bin_op = BinOpOperand { op: Operation::Mul, a, b: DerefOrImmediate::ImmediateOperand(b) };
    assert_eq!(format!("{}", bin_op), "[fp + -3] * 1400")
}
