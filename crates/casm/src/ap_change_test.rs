use super::{BinOpOperand, DerefOrImmediate};
use crate::ap_change::ApChange;
use crate::operand::{DerefOperand, Operation, Register, ResOperand};

#[test]
fn test_res_operand_ap_change() {
    let a = DerefOperand { register: Register::FP, offset: -3 };
    let b = DerefOperand { register: Register::AP, offset: 3 };

    let operand =
        ResOperand::BinOp(BinOpOperand { op: Operation::Mul, a, b: DerefOrImmediate::Deref(b) });
    let ap_change: i16 = 5;

    assert_eq!(operand.apply_ap_change(ap_change).to_string(), "[fp + -3] * [ap + -2]");
}
