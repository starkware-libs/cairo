#[cfg(not(feature = "std"))]
use alloc::string::ToString;

use cairo_lang_test_utils::test;

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
        apply_ap_change(operand.clone(), ApChange::Known(5)).unwrap().to_string(),
        "[fp + -3] * [ap + -2]"
    );

    assert_eq!(apply_ap_change(operand, ApChange::Unknown), Err(ApChangeError::UnknownApChange));

    assert_eq!(apply_ap_change(fp_based_operand, ApChange::Unknown).unwrap(), fp_based_operand);
}

#[test]
fn test_overflow() {
    let ap_based_operand = CellRef { register: Register::AP, offset: i16::MIN };

    assert_eq!(
        apply_ap_change(ap_based_operand, ApChange::Known(1)),
        Err(ApChangeError::OffsetOverflow)
    );
}

/// Helper function to apply an AP change to a value.
fn apply_ap_change<T: ApplyApChange>(mut t: T, ap_change: ApChange) -> Result<T, ApChangeError> {
    t.apply_ap_change(ap_change)?;
    Ok(t)
}
