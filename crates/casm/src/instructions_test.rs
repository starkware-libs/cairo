use indoc::indoc;

use crate::hints::Hint;
use crate::instructions::{
    AddApInstruction, AssertEqInstruction, CallInstruction, Instruction, InstructionBody,
    JnzInstruction, JumpInstruction, RetInstruction,
};
use crate::operand::{DerefOperand, DerefOrImmediate, ImmediateOperand, Register, ResOperand};

#[test]
fn test_jump_format() {
    let abs_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: 3 }),
            relative: false,
        }),
        false,
    );

    assert_eq!(abs_jmp_insn.to_string(), "jmp abs 3");

    let rel_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: -5 }),
            relative: true,
        }),
        true,
    );

    assert_eq!(rel_jmp_insn.to_string(), "jmp rel -5, ap++");
}

#[test]
fn test_call_format() {
    let abs_call_insn = CallInstruction {
        target: DerefOrImmediate::Immediate(ImmediateOperand { value: 3 }),
        relative: false,
    };

    assert_eq!(abs_call_insn.to_string(), "call abs 3");

    let rel_call_insn: InstructionBody = InstructionBody::Call(CallInstruction {
        target: DerefOrImmediate::Immediate(ImmediateOperand { value: -5 }),
        relative: true,
    });

    assert_eq!(rel_call_insn.to_string(), "call rel -5");
}

#[test]
fn test_jnz_format() {
    let jnz_insn = JnzInstruction {
        jump_offset: DerefOrImmediate::Immediate(ImmediateOperand { value: 205 }),
        condition: DerefOperand { register: Register::AP, offset: 5 },
    };

    assert_eq!(jnz_insn.to_string(), "jmp rel 205 if [ap + 5] != 0");
}

#[test]
fn test_assert_eq_format() {
    let op1 = DerefOperand { register: Register::AP, offset: 5 };
    let op2 = ResOperand::Immediate(ImmediateOperand { value: 205 });

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
    let operand = ResOperand::Immediate(ImmediateOperand { value: 205 });

    let addap_insn: InstructionBody = InstructionBody::AddAp(AddApInstruction { operand });

    assert_eq!(addap_insn.to_string(), "ap += 205");
}

#[test]
fn test_instruction_with_hint() {
    let dst = DerefOperand { register: Register::AP, offset: 5 };
    let abs_jmp_insn = Instruction {
        body: InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: 3 }),
            relative: false,
        }),
        inc_ap: false,
        hints: vec![Hint::AllocSegment { dst }],
    };

    assert_eq!(
        abs_jmp_insn.to_string(),
        indoc! {"
            %{ memory[ap + 5] = segments.add() %}
            jmp abs 3"
        }
    );
}
