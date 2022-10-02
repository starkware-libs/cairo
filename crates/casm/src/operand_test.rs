use super::{BinOpOperand, DerefOrImmediate, DoubleDerefOperand, Operation};
use crate::operand::{DerefOperand, ImmediateOperand, Register};

#[test]
fn test_deref_operand_format() {
    assert_eq!(DerefOperand { register: Register::AP, offset: 5 }.to_string(), "[ap + 5]");

    assert_eq!(DerefOperand { register: Register::FP, offset: -3 }.to_string(), "[fp + -3]");
}

#[test]
fn test_double_deref_op_format() {
    assert_eq!(
        DoubleDerefOperand {
            inner_deref: DerefOperand { register: Register::AP, offset: 5 },
            offset: 0
        }
        .to_string(),
        "[[ap + 5] + 0]"
    );
}

#[test]
fn test_immediate_format() {
    assert_eq!(ImmediateOperand { value: 1400 }.to_string(), "1400");
}

#[test]
fn test_bin_op_format() {
    let a = DerefOperand { register: Register::FP, offset: -3 };
    let b = ImmediateOperand { value: 1400 };

    let bin_op = BinOpOperand { op: Operation::Mul, a, b: DerefOrImmediate::Immediate(b) };
    assert_eq!(bin_op.to_string(), "[fp + -3] * 1400")
}
