use super::{BinOpOperand, DerefOrImmediate};
use crate::ap_change::{ApChange, ApChangeError, ApplyApChange};
use crate::operand::{DerefOperand, Operation, Register, ResOperand};

#[test]
fn test_res_operand_ap_change() {
    let operand_a = DerefOperand { register: Register::FP, offset: -3 };
    let operand_b = DerefOperand { register: Register::AP, offset: 3 };

    let operand = ResOperand::BinOp(BinOpOperand {
        op: Operation::Mul,
        a: operand_a,
        b: DerefOrImmediate::Deref(operand_b),
    });

    assert_eq!(
        operand.clone().apply_ap_change(ApChange::Known(5)).unwrap().to_string(),
        "[fp + -3] * [ap + -2]"
    );

    assert_eq!(operand.apply_ap_change(ApChange::Unknown), Err(ApChangeError::UnknownApChange));

    assert_eq!(operand_a.apply_ap_change(ApChange::Unknown).unwrap(), operand_a);
}

#[test]
fn test_overflow() {
    let operand = DerefOperand { register: Register::AP, offset: i16::MIN };

    assert_eq!(operand.apply_ap_change(ApChange::Known(1)), Err(ApChangeError::OffsetOverflow));
}
