use indoc::indoc;
use test_log::test;

use crate::hints::{CoreHint, StarknetHint};
use crate::operand::{BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand};
use crate::res;

#[test]
fn test_alloc_segment_format() {
    let dst = CellRef { register: Register::AP, offset: 5 };
    let hint = CoreHint::AllocSegment { dst };

    assert_eq!(hint.to_string(), "memory[ap + 5] = segments.add()");
}

#[test]
fn test_less_than_format() {
    let ap_based = res!([ap + 6]);
    let fp_based = res!([fp + 4]);
    let immediate = res!(3);

    assert_eq!(
        CoreHint::TestLessThan {
            lhs: ap_based.clone(),
            rhs: fp_based.clone(),
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .to_string(),
        "memory[ap + 0] = memory[ap + 6] < memory[fp + 4]"
    );
    assert_eq!(
        CoreHint::TestLessThan {
            lhs: fp_based,
            rhs: immediate.clone(),
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .to_string(),
        "memory[ap + 0] = memory[fp + 4] < 3"
    );
    assert_eq!(
        CoreHint::TestLessThan {
            lhs: immediate,
            rhs: ap_based,
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .to_string(),
        "memory[ap + 0] = 3 < memory[ap + 6]"
    );
}

#[test]
fn test_less_than_or_equal_format() {
    let ap_based = res!([ap + 6]);
    let fp_based = res!([fp + 4]);
    let immediate = res!(3);

    assert_eq!(
        CoreHint::TestLessThanOrEqual {
            lhs: ap_based.clone(),
            rhs: fp_based.clone(),
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .to_string(),
        "memory[ap + 0] = memory[ap + 6] <= memory[fp + 4]"
    );
    assert_eq!(
        CoreHint::TestLessThanOrEqual {
            lhs: fp_based,
            rhs: immediate.clone(),
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .to_string(),
        "memory[ap + 0] = memory[fp + 4] <= 3"
    );
    assert_eq!(
        CoreHint::TestLessThanOrEqual {
            lhs: immediate,
            rhs: ap_based,
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .to_string(),
        "memory[ap + 0] = 3 <= memory[ap + 6]"
    );
}

#[test]
fn test_syscall_hint_format() {
    let system = ResOperand::BinOp(BinOpOperand {
        op: Operation::Add,
        a: CellRef { register: Register::FP, offset: -3 },
        b: DerefOrImmediate::from(3),
    });

    assert_eq!(
        StarknetHint::SystemCall { system }.to_string(),
        "syscall_handler.syscall(syscall_ptr=memory[fp + -3] + 3)"
    );
}

#[test]
fn test_debug_hint_format() {
    assert_eq!(
        CoreHint::DebugPrint { start: res!([ap + 6]), end: res!([fp - 8]) }.to_string(),
        indoc! {"

            start = memory[ap + 6]
            end = memory[fp + -8]
            for i in range(start, end):
                print(memory[i])
        "}
    );
}
