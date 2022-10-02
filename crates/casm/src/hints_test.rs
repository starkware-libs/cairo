use crate::hints::Hint;
use crate::operand::{DerefOperand, Register};

#[test]
fn test_alloc_segment_format() {
    let dst = DerefOperand { register: Register::AP, offset: 5 };
    let hint = Hint::AllocSegment { dst };

    assert_eq!(hint.to_string(), "%{ memory[ap + 5] = segments.add() %}");
}
