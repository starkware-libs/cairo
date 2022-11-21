use num_bigint::ToBigInt;
use pretty_assertions::assert_eq;
use test_log::test;

use super::InstructionRepr;
use crate::assembler::{ApUpdate, FpUpdate, Op1Addr, Opcode, PcUpdate, Res};
use crate::casm;
use crate::inline::CasmContext;
use crate::operand::Register;

/// Takes a casm instruction, which can be constructed using the macro casm!, and
/// returns its assembled representation.
fn assemble_instruction(mut casm: CasmContext) -> InstructionRepr {
    casm.instructions.remove(0).assemble()
}

#[test]
fn test_jump_assemble() {
    assert_eq!(
        assemble_instruction(casm!(jmp abs 3;)),
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

    assert_eq!(
        assemble_instruction(casm!(jmp rel -5, ap++;)),
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
    assert_eq!(
        assemble_instruction(casm!(call abs 3;)),
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
    assert_eq!(
        assemble_instruction(casm!(call rel (-5);)),
        InstructionRepr {
            off0: 0,
            off1: 1,
            off2: 1,
            imm: (-5).to_bigint(),
            dst_register: Register::AP,
            op0_register: Register::AP,
            op1_addr: Op1Addr::Imm,
            res: Res::Op1,
            pc_update: PcUpdate::JumpRel,
            ap_update: ApUpdate::Add2,
            fp_update: FpUpdate::ApPlus2,
            opcode: Opcode::Call,
        },
    );
}

#[test]
fn test_jnz_assemble() {
    assert_eq!(
        assemble_instruction(casm!(jmp rel 205 if [ap + 5] != 0;)),
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
    assert_eq!(
        assemble_instruction(casm!(jmp rel 2 if [ap - 2] != 0, ap++;)),
        InstructionRepr {
            off0: -2,
            off1: -1,
            off2: 1,
            imm: 2.to_bigint(),
            dst_register: Register::AP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::Imm,
            res: Res::Unconstrained,
            pc_update: PcUpdate::Jnz,
            ap_update: ApUpdate::Add1,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::Nop,
        },
    );
}

#[test]
fn test_assert_eq_assemble() {
    assert_eq!(
        assemble_instruction(casm!([ap + 5] = 205;)),
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

    assert_eq!(
        assemble_instruction(casm!([fp] = [ap - 1] + [ap - 2];)),
        InstructionRepr {
            off0: 0,
            off1: -1,
            off2: -2,
            imm: None,
            dst_register: Register::FP,
            op0_register: Register::AP,
            op1_addr: Op1Addr::AP,
            res: Res::Add,
            pc_update: PcUpdate::Regular,
            ap_update: ApUpdate::Regular,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::AssertEq,
        },
    );

    assert_eq!(
        assemble_instruction(casm!([ap + 0] = [fp + -5], ap++;)),
        InstructionRepr {
            off0: 0,
            off1: -1,
            off2: -5,
            imm: None,
            dst_register: Register::AP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::FP,
            res: Res::Op1,
            pc_update: PcUpdate::Regular,
            ap_update: ApUpdate::Add1,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::AssertEq,
        },
    );

    assert_eq!(
        assemble_instruction(casm!([ap] = [ap - 3], ap++;)),
        InstructionRepr {
            off0: 0,
            off1: -1,
            off2: -3,
            imm: None,
            dst_register: Register::AP,
            op0_register: Register::FP,
            op1_addr: Op1Addr::AP,
            res: Res::Op1,
            pc_update: PcUpdate::Regular,
            ap_update: ApUpdate::Add1,
            fp_update: FpUpdate::Regular,
            opcode: Opcode::AssertEq,
        },
    );
}

#[test]
fn test_ret_assemble() {
    assert_eq!(
        assemble_instruction(casm!(ret;)),
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
    assert_eq!(
        assemble_instruction(casm!(ap += 205;)),
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
    assert_eq!(
        assemble_instruction(casm!(jmp abs 3;)),
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
