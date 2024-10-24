use indoc::indoc;
use test_log::test;

use crate::hints::{CoreHint, PythonicHint, StarknetHint};
use crate::operand::{BinOpOperand, CellRef, DerefOrImmediate, Operation, Register, ResOperand};
use crate::res;

#[test]
fn test_alloc_segment_format() {
    let dst = CellRef { register: Register::AP, offset: 5 };
    let hint = CoreHint::AllocSegment { dst };

    assert_eq!(hint.get_pythonic_hint(), "memory[ap + 5] = segments.add()");
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
        .get_pythonic_hint(),
        "memory[ap + 0] = memory[ap + 6] < memory[fp + 4]"
    );
    assert_eq!(
        CoreHint::TestLessThan {
            lhs: fp_based,
            rhs: immediate.clone(),
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .get_pythonic_hint(),
        "memory[ap + 0] = memory[fp + 4] < 3"
    );
    assert_eq!(
        CoreHint::TestLessThan {
            lhs: immediate,
            rhs: ap_based,
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .get_pythonic_hint(),
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
        .get_pythonic_hint(),
        "memory[ap + 0] = memory[ap + 6] <= memory[fp + 4]"
    );
    assert_eq!(
        CoreHint::TestLessThanOrEqual {
            lhs: fp_based,
            rhs: immediate.clone(),
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .get_pythonic_hint(),
        "memory[ap + 0] = memory[fp + 4] <= 3"
    );
    assert_eq!(
        CoreHint::TestLessThanOrEqual {
            lhs: immediate,
            rhs: ap_based,
            dst: CellRef { register: Register::AP, offset: 0 }
        }
        .get_pythonic_hint(),
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
        StarknetHint::SystemCall { system }.get_pythonic_hint(),
        "syscall_handler.syscall(syscall_ptr=memory[fp + -3] + 3)"
    );
}

#[test]
fn test_debug_hint_format() {
    assert_eq!(
        CoreHint::DebugPrint { start: res!([ap + 6]), end: res!([fp - 8]) }.get_pythonic_hint(),
        indoc! {"

            curr = memory[ap + 6]
            end = memory[fp + -8]
            while curr != end:
                print(hex(memory[curr]))
                curr += 1
        "}
    );
}

#[test]
#[cfg(feature = "parity-scale-codec")]
fn encode_hint() {
    use core::str::FromStr;

    use cairo_lang_utils::bigint::BigIntAsHex;
    use parity_scale_codec::{Decode, Encode};

    use crate::hints::{CoreHintBase, Hint};

    let hint = Hint::Core(CoreHintBase::Core(CoreHint::TestLessThan {
        lhs: ResOperand::Deref(CellRef { register: Register::FP, offset: -3 }),
        rhs: ResOperand::Immediate(BigIntAsHex {
            value: num_bigint::BigInt::from_str(
                "3618502788666131106986593281521497120414687020801267626233049500247285301248",
            )
            .unwrap(),
        }),
        dst: CellRef { register: Register::AP, offset: 4 },
    }));

    let encoding = hint.encode();
    let decoded = Hint::decode(&mut encoding.as_slice()).unwrap();
    assert_eq!(hint, decoded);
}
