#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use cairo_lang_utils::bigint::BigIntAsHex;
use num_traits::ToPrimitive;

use crate::instructions::{Instruction, InstructionBody};
use crate::operand::Register::*;
use crate::operand::{CellRef, DerefOrImmediate, Operation, ResOperand};

impl Instruction {
    pub fn encode31(&self) -> [u32; 4] {
        match &self.body {
            InstructionBody::AddAp(insn) => {
                let (kind, v1, v2) = insn.operand.m31_info();
                let v0 = match kind {
                    ResOperandKind::AddApAp => 0,
                    ResOperandKind::AddApFp => 1,
                    ResOperandKind::AddFpAp => 2,
                    ResOperandKind::AddFpFp => 3,
                    ResOperandKind::AddImmAp => 4,
                    ResOperandKind::AddImmFp => 5,
                    ResOperandKind::DerefAp => 6,
                    ResOperandKind::DerefFp => 7,
                    ResOperandKind::DoubleDerefAp => 8,
                    ResOperandKind::DoubleDerefFp => 9,
                    ResOperandKind::Imm => 10,
                    ResOperandKind::MulApAp => 11,
                    ResOperandKind::MulApFp => 12,
                    ResOperandKind::MulFpAp => 13,
                    ResOperandKind::MulFpFp => 14,
                    ResOperandKind::MulImmAp => 15,
                    ResOperandKind::MulImmFp => 16,
                };
                [v0, v1, v2, 0]
            }
            InstructionBody::AssertEq(insn) | InstructionBody::QM31AssertEq(insn) => {
                let (kind, v2, v3) = insn.b.m31_info();
                let v0 = match (insn.a.register, kind, self.inc_ap) {
                    (AP, ResOperandKind::AddApAp, false) => 17,
                    (AP, ResOperandKind::AddApAp, true) => 18,
                    (AP, ResOperandKind::AddApFp, false) => 19,
                    (AP, ResOperandKind::AddApFp, true) => 20,
                    (AP, ResOperandKind::AddFpAp, false) => 21,
                    (AP, ResOperandKind::AddFpAp, true) => 22,
                    (AP, ResOperandKind::AddFpFp, false) => 23,
                    (AP, ResOperandKind::AddFpFp, true) => 24,
                    (AP, ResOperandKind::AddImmAp, false) => 25,
                    (AP, ResOperandKind::AddImmAp, true) => 26,
                    (AP, ResOperandKind::AddImmFp, false) => 27,
                    (AP, ResOperandKind::AddImmFp, true) => 28,
                    (AP, ResOperandKind::DerefAp, false) => 29,
                    (AP, ResOperandKind::DerefAp, true) => 30,
                    (AP, ResOperandKind::DerefFp, false) => 31,
                    (AP, ResOperandKind::DerefFp, true) => 32,
                    (AP, ResOperandKind::DoubleDerefAp, false) => 33,
                    (AP, ResOperandKind::DoubleDerefAp, true) => 34,
                    (AP, ResOperandKind::DoubleDerefFp, false) => 35,
                    (AP, ResOperandKind::DoubleDerefFp, true) => 36,
                    (AP, ResOperandKind::Imm, false) => 37,
                    (AP, ResOperandKind::Imm, true) => 38,
                    (AP, ResOperandKind::MulApAp, false) => 39,
                    (AP, ResOperandKind::MulApAp, true) => 40,
                    (AP, ResOperandKind::MulApFp, false) => 41,
                    (AP, ResOperandKind::MulApFp, true) => 42,
                    (AP, ResOperandKind::MulFpAp, false) => 43,
                    (AP, ResOperandKind::MulFpAp, true) => 44,
                    (AP, ResOperandKind::MulFpFp, false) => 45,
                    (AP, ResOperandKind::MulFpFp, true) => 46,
                    (AP, ResOperandKind::MulImmAp, false) => 47,
                    (AP, ResOperandKind::MulImmAp, true) => 48,
                    (AP, ResOperandKind::MulImmFp, false) => 49,
                    (AP, ResOperandKind::MulImmFp, true) => 50,
                    (FP, ResOperandKind::AddApAp, false) => 51,
                    (FP, ResOperandKind::AddApAp, true) => 52,
                    (FP, ResOperandKind::AddApFp, false) => 53,
                    (FP, ResOperandKind::AddApFp, true) => 54,
                    (FP, ResOperandKind::AddFpAp, false) => 55,
                    (FP, ResOperandKind::AddFpAp, true) => 56,
                    (FP, ResOperandKind::AddFpFp, false) => 57,
                    (FP, ResOperandKind::AddFpFp, true) => 58,
                    (FP, ResOperandKind::AddImmAp, false) => 59,
                    (FP, ResOperandKind::AddImmAp, true) => 60,
                    (FP, ResOperandKind::AddImmFp, false) => 61,
                    (FP, ResOperandKind::AddImmFp, true) => 62,
                    (FP, ResOperandKind::DerefAp, false) => 63,
                    (FP, ResOperandKind::DerefAp, true) => 64,
                    (FP, ResOperandKind::DerefFp, false) => 65,
                    (FP, ResOperandKind::DerefFp, true) => 66,
                    (FP, ResOperandKind::DoubleDerefAp, false) => 67,
                    (FP, ResOperandKind::DoubleDerefAp, true) => 68,
                    (FP, ResOperandKind::DoubleDerefFp, false) => 69,
                    (FP, ResOperandKind::DoubleDerefFp, true) => 70,
                    (FP, ResOperandKind::Imm, false) => 71,
                    (FP, ResOperandKind::Imm, true) => 72,
                    (FP, ResOperandKind::MulApAp, false) => 73,
                    (FP, ResOperandKind::MulApAp, true) => 74,
                    (FP, ResOperandKind::MulApFp, false) => 75,
                    (FP, ResOperandKind::MulApFp, true) => 76,
                    (FP, ResOperandKind::MulFpAp, false) => 77,
                    (FP, ResOperandKind::MulFpAp, true) => 78,
                    (FP, ResOperandKind::MulFpFp, false) => 79,
                    (FP, ResOperandKind::MulFpFp, true) => 80,
                    (FP, ResOperandKind::MulImmAp, false) => 81,
                    (FP, ResOperandKind::MulImmAp, true) => 82,
                    (FP, ResOperandKind::MulImmFp, false) => 83,
                    (FP, ResOperandKind::MulImmFp, true) => 84,
                };
                [v0, i16_to_m31(insn.a.offset), v2, v3]
            }
            InstructionBody::Call(insn) => {
                let (kind, v1) = insn.target.m31_info();
                let v0 = match (insn.relative, kind) {
                    (false, DerefOrImmediateKind::Ap) => 85,
                    (false, DerefOrImmediateKind::Fp) => 86,
                    (false, DerefOrImmediateKind::Imm) => 87,
                    (true, DerefOrImmediateKind::Ap) => 88,
                    (true, DerefOrImmediateKind::Fp) => 89,
                    (true, DerefOrImmediateKind::Imm) => 90,
                };
                [v0, v1, 0, 0]
            }
            InstructionBody::Jump(insn) => {
                let (kind, v1) = insn.target.m31_info();
                let v0 = match (insn.relative, kind, self.inc_ap) {
                    (false, DerefOrImmediateKind::Ap, false) => 103,
                    (false, DerefOrImmediateKind::Ap, true) => 104,
                    (false, DerefOrImmediateKind::Fp, false) => 105,
                    (false, DerefOrImmediateKind::Fp, true) => 106,
                    (false, DerefOrImmediateKind::Imm, false) => 111,
                    (false, DerefOrImmediateKind::Imm, true) => 112,
                    (true, DerefOrImmediateKind::Ap, false) => 137,
                    (true, DerefOrImmediateKind::Ap, true) => 138,
                    (true, DerefOrImmediateKind::Fp, false) => 139,
                    (true, DerefOrImmediateKind::Fp, true) => 140,
                    (true, DerefOrImmediateKind::Imm, false) => 145,
                    (true, DerefOrImmediateKind::Imm, true) => 146,
                };
                [v0, v1, 0, 0]
            }
            InstructionBody::Jnz(insn) => {
                let (kind, v1) = insn.jump_offset.m31_info();
                let v0 = match (kind, insn.condition.register, self.inc_ap) {
                    (DerefOrImmediateKind::Ap, AP, false) => 159,
                    (DerefOrImmediateKind::Ap, AP, true) => 160,
                    (DerefOrImmediateKind::Ap, FP, false) => 161,
                    (DerefOrImmediateKind::Ap, FP, true) => 162,
                    (DerefOrImmediateKind::Fp, AP, false) => 163,
                    (DerefOrImmediateKind::Fp, AP, true) => 164,
                    (DerefOrImmediateKind::Fp, FP, false) => 165,
                    (DerefOrImmediateKind::Fp, FP, true) => 166,
                    (DerefOrImmediateKind::Imm, AP, false) => 167,
                    (DerefOrImmediateKind::Imm, AP, true) => 168,
                    (DerefOrImmediateKind::Imm, FP, false) => 169,
                    (DerefOrImmediateKind::Imm, FP, true) => 170,
                };
                [v0, v1, i16_to_m31(insn.condition.offset), 0]
            }
            InstructionBody::Ret(_) => [171, 0, 0, 0],
            InstructionBody::RangeCheck(inst) => {
                let v0 = match inst.cell.register {
                    AP => 172,
                    FP => 173,
                };
                [v0, inst.lower, inst.upper, i16_to_m31(inst.cell.offset)]
            }
            InstructionBody::Blake2sCompress(_) => panic!("Blake2sCompress is unsupported"),
        }
    }
}

enum DerefOrImmediateKind {
    Ap,
    Fp,
    Imm,
}
impl DerefOrImmediate {
    fn m31_info(&self) -> (DerefOrImmediateKind, u32) {
        match self {
            DerefOrImmediate::Deref(CellRef { register: AP, offset }) => {
                (DerefOrImmediateKind::Ap, i16_to_m31(*offset))
            }
            DerefOrImmediate::Deref(CellRef { register: FP, offset }) => {
                (DerefOrImmediateKind::Fp, i16_to_m31(*offset))
            }
            DerefOrImmediate::Immediate(operand) => {
                (DerefOrImmediateKind::Imm, bigint_to_m31(operand))
            }
        }
    }
}

enum ResOperandKind {
    AddApAp,
    AddApFp,
    AddFpAp,
    AddFpFp,
    AddImmAp,
    AddImmFp,
    DerefAp,
    DerefFp,
    DoubleDerefAp,
    DoubleDerefFp,
    Imm,
    MulApAp,
    MulApFp,
    MulFpAp,
    MulFpFp,
    MulImmAp,
    MulImmFp,
}

impl ResOperand {
    fn m31_info(&self) -> (ResOperandKind, u32, u32) {
        match self {
            ResOperand::Deref(CellRef { register: AP, offset }) => {
                (ResOperandKind::DerefAp, i16_to_m31(*offset), 0)
            }
            ResOperand::Deref(CellRef { register: FP, offset }) => {
                (ResOperandKind::DerefFp, i16_to_m31(*offset), 0)
            }
            ResOperand::DoubleDeref(CellRef { register: AP, offset }, inner) => {
                (ResOperandKind::DoubleDerefAp, i16_to_m31(*offset), i16_to_m31(*inner))
            }
            ResOperand::DoubleDeref(CellRef { register: FP, offset }, inner) => {
                (ResOperandKind::DoubleDerefFp, i16_to_m31(*offset), i16_to_m31(*inner))
            }
            ResOperand::Immediate(operand) => (ResOperandKind::Imm, bigint_to_m31(operand), 0),
            ResOperand::BinOp(bin) => {
                let (kind, lhs) = bin.b.m31_info();
                let kind = match (&bin.op, kind, bin.a.register) {
                    (Operation::Add, DerefOrImmediateKind::Ap, AP) => ResOperandKind::AddApAp,
                    (Operation::Add, DerefOrImmediateKind::Ap, FP) => ResOperandKind::AddApFp,
                    (Operation::Add, DerefOrImmediateKind::Fp, AP) => ResOperandKind::AddFpAp,
                    (Operation::Add, DerefOrImmediateKind::Fp, FP) => ResOperandKind::AddFpFp,
                    (Operation::Add, DerefOrImmediateKind::Imm, AP) => ResOperandKind::AddImmAp,
                    (Operation::Add, DerefOrImmediateKind::Imm, FP) => ResOperandKind::AddImmFp,
                    (Operation::Mul, DerefOrImmediateKind::Ap, AP) => ResOperandKind::MulApAp,
                    (Operation::Mul, DerefOrImmediateKind::Ap, FP) => ResOperandKind::MulApFp,
                    (Operation::Mul, DerefOrImmediateKind::Fp, AP) => ResOperandKind::MulFpAp,
                    (Operation::Mul, DerefOrImmediateKind::Fp, FP) => ResOperandKind::MulFpFp,
                    (Operation::Mul, DerefOrImmediateKind::Imm, AP) => ResOperandKind::MulImmAp,
                    (Operation::Mul, DerefOrImmediateKind::Imm, FP) => ResOperandKind::MulImmFp,
                };
                (kind, lhs, i16_to_m31(bin.a.offset))
            }
        }
    }
}

const PRIME: i32 = 0x7fffffff;

fn i32_to_m31(value: i32) -> u32 {
    (if value < 0 { value + PRIME } else { value }) as u32
}

fn i16_to_m31(value: i16) -> u32 {
    i32_to_m31(value.into())
}

fn bigint_to_m31(value: &BigIntAsHex) -> u32 {
    i32_to_m31(value.value.to_i32().unwrap())
}
