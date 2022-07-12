use crate::operand::{DerefOperand, ImmediateOperand, Register};

use super::{BinOpOperand, Operation, SimpleOperand};

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

#[test]
fn test_immediate_format() {
    assert_eq!(ImmediateOperand { value: 1400 }.format(), "1400");
}

#[test]
fn test_bin_op_format() {
    let a = DerefOperand {
        register: Register::FP,
        offset: -3,
    };
    let b = ImmediateOperand { value: 1400 };

    let bin_op = BinOpOperand {
        op: Operation::Mul,
        a,
        b: SimpleOperand::ImmediateOperand(b),
    };
    assert_eq!(bin_op.format(), "[fp + -3] * 1400")
}
