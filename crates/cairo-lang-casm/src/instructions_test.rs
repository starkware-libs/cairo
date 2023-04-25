use indoc::indoc;
use test_log::test;

use crate::hints::CoreHint;
use crate::instructions::{
    AddApInstruction, AssertEqInstruction, CallInstruction, Instruction, InstructionBody,
    JnzInstruction, JumpInstruction, RetInstruction,
};
use crate::operand::{CellRef, DerefOrImmediate, Register, ResOperand};

#[test]
fn test_jump_format() {
    let abs_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::from(3),
            relative: false,
        }),
        false,
    );

    assert_eq!(abs_jmp_insn.to_string(), "jmp abs 3");

    let rel_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::from(-5),
            relative: true,
        }),
        true,
    );

    assert_eq!(rel_jmp_insn.to_string(), "jmp rel -5, ap++");
}

#[test]
fn test_call_format() {
    let abs_call_insn = CallInstruction { target: DerefOrImmediate::from(3), relative: false };

    assert_eq!(abs_call_insn.to_string(), "call abs 3");

    let rel_call_insn: InstructionBody = InstructionBody::Call(CallInstruction {
        target: DerefOrImmediate::from(-5),
        relative: true,
    });

    assert_eq!(rel_call_insn.to_string(), "call rel -5");
}

#[test]
fn test_jnz_format() {
    let jnz_insn = JnzInstruction {
        jump_offset: DerefOrImmediate::from(205),
        condition: CellRef { register: Register::AP, offset: 5 },
    };

    assert_eq!(jnz_insn.to_string(), "jmp rel 205 if [ap + 5] != 0");
}

#[test]
fn test_assert_eq_format() {
    let op1 = CellRef { register: Register::AP, offset: 5 };
    let op2 = ResOperand::from(205);

    let insn = AssertEqInstruction { a: op1, b: op2 };
    assert_eq!(insn.to_string(), "[ap + 5] = 205");
}

#[test]
fn test_ret_format() {
    let insn = RetInstruction {};
    assert_eq!(insn.to_string(), "ret");
}

#[test]
fn test_add_ap_format() {
    let operand = ResOperand::from(205);

    let addap_insn: InstructionBody = InstructionBody::AddAp(AddApInstruction { operand });

    assert_eq!(addap_insn.to_string(), "ap += 205");
}

#[test]
fn test_instruction_with_hint() {
    let dst = CellRef { register: Register::AP, offset: 5 };
    let abs_jmp_insn = Instruction {
        body: InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::from(3),
            relative: false,
        }),
        inc_ap: false,
        hints: vec![CoreHint::AllocSegment { dst }.into()],
    };

    assert_eq!(
        abs_jmp_insn.to_string(),
        indoc! {"
            %{ memory[ap + 5] = segments.add() %}
            jmp abs 3"
        }
    );
}
