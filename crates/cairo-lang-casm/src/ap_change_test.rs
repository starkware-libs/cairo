use test_log::test;

use super::{BinOpOperand, DerefOrImmediate};
use crate::ap_change::{ApChange, ApChangeError, ApplyApChange};
use crate::operand::{CellRef, Operation, Register, ResOperand};

#[test]
fn test_res_operand_ap_change() {
    let fp_based_operand = CellRef { register: Register::FP, offset: -3 };
    let ap_based_operand = CellRef { register: Register::AP, offset: 3 };

    let operand = ResOperand::BinOp(BinOpOperand {
        op: Operation::Mul,
        a: fp_based_operand,
        b: DerefOrImmediate::Deref(ap_based_operand),
    });

    assert_eq!(
        operand.clone().apply_ap_change(ApChange::Known(5)).unwrap().to_string(),
        "[fp + -3] * [ap + -2]"
    );

    assert_eq!(operand.apply_ap_change(ApChange::Unknown), Err(ApChangeError::UnknownApChange));

    assert_eq!(fp_based_operand.apply_ap_change(ApChange::Unknown).unwrap(), fp_based_operand);
}

#[test]
fn test_overflow() {
    let ap_based_operand = CellRef { register: Register::AP, offset: i16::MIN };

    assert_eq!(
        ap_based_operand.apply_ap_change(ApChange::Known(1)),
        Err(ApChangeError::OffsetOverflow)
    );
}
