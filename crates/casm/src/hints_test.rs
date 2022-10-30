use crate::hints::Hint;
use crate::operand::{CellRef, DerefOrImmediate, Register};

#[test]
fn test_alloc_segment_format() {
    let dst = CellRef { register: Register::AP, offset: 5 };
    let hint = Hint::AllocSegment { dst };

    assert_eq!(hint.to_string(), "%{ memory[ap + 5] = segments.add() %}");
}

#[test]
fn test_less_than_format() {
    let ap_based = DerefOrImmediate::Deref(CellRef { register: Register::AP, offset: 6 });
    let fp_based = DerefOrImmediate::Deref(CellRef { register: Register::FP, offset: 4 });
    let immediate = DerefOrImmediate::from(3);

    assert_eq!(
        Hint::TestLessThan { lhs: ap_based.clone(), rhs: fp_based.clone() }.to_string(),
        "%{ memory[ap + 0] = memory[ap + 6] < memory[fp + 4] %}"
    );
    assert_eq!(
        Hint::TestLessThan { lhs: fp_based, rhs: immediate.clone() }.to_string(),
        "%{ memory[ap + 0] = memory[fp + 4] < 3 %}"
    );
    assert_eq!(
        Hint::TestLessThan { lhs: immediate, rhs: ap_based }.to_string(),
        "%{ memory[ap + 0] = 3 < memory[ap + 6] %}"
    );
}
