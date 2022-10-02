use num_bigint::ToBigInt;

use super::InstructionRepr;
use crate::assembler::{ApUpdate, FpUpdate, Op1Addr, Opcode, PcUpdate, Res};
use crate::instructions::{
    AddApInstruction, AssertEqInstruction, CallInstruction, Instruction, InstructionBody,
    JnzInstruction, JumpInstruction, RetInstruction,
};
use crate::operand::{DerefOperand, DerefOrImmediate, ImmediateOperand, Register, ResOperand};

fn test_instruction_assemble(instruction: Instruction, expected: InstructionRepr) {
    let assembled = instruction.assemble();
    assert_eq!(assembled, expected);
}

#[test]
fn test_jump_assemble() {
    let abs_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: 3 }),
            relative: false,
        }),
        false,
    );

    assert_eq!(abs_jmp_insn.to_string(), "jmp abs 3");

    test_instruction_assemble(
        abs_jmp_insn,
        InstructionRepr {
            off0: -1,
            off1: -1,
            off2: 1,
            imm: 3.to_bigint(),
            dst_register: Register::FP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::Jump,
            ap_update: ApUpdate::Regular,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::Nop,
        },
    );

    let rel_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: -5 }),
            relative: true,
        }),
        true,
    );

    assert_eq!(rel_jmp_insn.to_string(), "jmp rel -5, ap++");

    test_instruction_assemble(
        rel_jmp_insn,
        InstructionRepr {
            off0: -1,
            off1: -1,
            off2: 1,
            imm: (-5).to_bigint(),
            dst_register: Register::FP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::JumpRel,
            ap_update: ApUpdate::Add1,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::Nop,
        },
    );
}

#[test]
fn test_call_assemble() {
    let abs_call_insn = Instruction::new(
        InstructionBody::Call(CallInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: 3 }),
            relative: false,
        }),
        false,
    );

    assert_eq!(abs_call_insn.to_string(), "call abs 3");

    test_instruction_assemble(
        abs_call_insn,
        InstructionRepr {
            off0: 0,
            off1: 1,
            off2: 1,
            imm: 3.to_bigint(),
            dst_register: Register::AP,
            op0_register: Register::AP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::Jump,
            ap_update: ApUpdate::Add2,
            fp_update: FpUpdate::ApPlus2,
            opcode: Opcode::Call,
        },
    );

    let rel_call_insn: InstructionBody = InstructionBody::Call(CallInstruction {
        target: DerefOrImmediate::Immediate(ImmediateOperand { value: -5 }),
        relative: true,
    });

    assert_eq!(rel_call_insn.to_string(), "call rel -5");
}

#[test]
fn test_jnz_assemble() {
    let jnz_insn = Instruction::new(
        InstructionBody::Jnz(JnzInstruction {
            jump_offset: DerefOrImmediate::Immediate(ImmediateOperand { value: 205 }),
            condition: DerefOperand { register: Register::AP, offset: 5 },
        }),
        false,
    );

    assert_eq!(jnz_insn.to_string(), "jmp rel 205 if [ap + 5] != 0");

    test_instruction_assemble(
        jnz_insn,
        InstructionRepr {
            off0: 5,
            off1: -1,
            off2: 1,
            imm: 205.to_bigint(),
            dst_register: Register::AP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Unconstrained,
            pc_update: PcUpdate::Jnz,
            ap_update: ApUpdate::Regular,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::Nop,
        },
    );
}

#[test]
fn test_assert_eq_assemble() {
    let op1 = DerefOperand { register: Register::AP, offset: 5 };
    let op2 = ResOperand::Immediate(ImmediateOperand { value: 205 });

    let assert_eq_insn =
        Instruction::new(InstructionBody::AssertEq(AssertEqInstruction { a: op1, b: op2 }), false);
    assert_eq!(assert_eq_insn.to_string(), "[ap + 5] = 205");

    test_instruction_assemble(
        assert_eq_insn,
        InstructionRepr {
            off0: 5,
            off1: -1,
            off2: 1,
            imm: 205.to_bigint(),
            dst_register: Register::AP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::Regular,
            ap_update: ApUpdate::Regular,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::AssertEq,
        },
    );
}

#[test]
fn test_ret_assemble() {
    let ret_insn = Instruction::new(InstructionBody::Ret(RetInstruction {}), false);
    assert_eq!(ret_insn.to_string(), "ret");

    test_instruction_assemble(
        ret_insn,
        InstructionRepr {
            off0: -2,
            off1: -1,
            off2: -1,
            imm: None,
            dst_register: Register::FP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::FP,
            res: Res::Op1,
            pc_update: PcUpdate::Jump,
            ap_update: ApUpdate::Regular,
            fp_update: FpUpdate::Dst,
            opcode: Opcode::Ret,
        },
    );
}

#[test]
fn test_add_ap_assemble() {
    let operand = ResOperand::Immediate(ImmediateOperand { value: 205 });

    let addap_insn = Instruction::new(InstructionBody::AddAp(AddApInstruction { operand }), false);

    assert_eq!(addap_insn.to_string(), "ap += 205");

    test_instruction_assemble(
        addap_insn,
        InstructionRepr {
            off0: -1,
            off1: -1,
            off2: 1,
            imm: 205.to_bigint(),
            dst_register: Register::FP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::Regular,
            ap_update: ApUpdate::Add,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::Nop,
        },
    );
}

#[test]
fn test_instruction_with_hint() {
    let abs_jmp_insn = Instruction::new(
        InstructionBody::Jump(JumpInstruction {
            target: DerefOrImmediate::Immediate(ImmediateOperand { value: 3 }),
            relative: false,
        }),
        false,
    );

    assert_eq!(abs_jmp_insn.to_string(), "jmp abs 3");

    test_instruction_assemble(
        abs_jmp_insn,
        InstructionRepr {
            off0: -1,
            off1: -1,
            off2: 1,
            imm: 3.to_bigint(),
            dst_register: Register::FP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::Jump,
            ap_update: ApUpdate::Regular,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::Nop,
        },
    );
}
