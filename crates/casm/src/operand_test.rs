use test_log::test;

use super::{BinOpOperand, DerefOrImmediate, Operation};
use crate::operand::{CellRef, Register, ResOperand};

#[test]
fn test_deref_operand_format() {
    assert_eq!(CellRef { register: Register::AP, offset: 5 }.to_string(), "[ap + 5]");

    assert_eq!(CellRef { register: Register::FP, offset: -3 }.to_string(), "[fp + -3]");
}

#[test]
fn test_double_deref_op_format() {
    assert_eq!(
        ResOperand::DoubleDeref(CellRef { register: Register::AP, offset: 5 }, 12).to_string(),
        "[[ap + 5] + 12]"
    );
}

#[test]
fn test_immediate_format() {
    assert_eq!(DerefOrImmediate::from(1400).to_string(), "1400");
}

#[test]
fn test_bin_op_format() {
    let bin_op = BinOpOperand {
        op: Operation::Mul,
        a: CellRef { register: Register::FP, offset: -3 },
        b: DerefOrImmediate::from(1400),
    };
    assert_eq!(bin_op.to_string(), "[fp + -3] * 1400")
}
