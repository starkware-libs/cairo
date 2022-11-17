use num_bigint::BigInt;

use crate::assembler::{ApUpdate, FpUpdate, InstructionRepr, Op1Addr, Opcode, PcUpdate, Res};
use crate::operand::Register;

#[cfg(test)]
#[path = "encoder_test.rs"]
mod test;

const OFFSET_BITS: u32 = 16;

const DST_REG_BIT: i32 = 0;
const OP0_REG_BIT: i32 = 1;
const OP1_IMM_BIT: i32 = 2;
const OP1_FP_BIT: i32 = 3;
const OP1_AP_BIT: i32 = 4;
const RES_ADD_BIT: i32 = 5;
const RES_MUL_BIT: i32 = 6;
const PC_JUMP_ABS_BIT: i32 = 7;
const PC_JUMP_REL_BIT: i32 = 8;
const PC_JNZ_BIT: i32 = 9;
const AP_ADD_BIT: i32 = 10;
const AP_ADD1_BIT: i32 = 11;
const OPCODE_CALL_BIT: i32 = 12;
const OPCODE_RET_BIT: i32 = 13;
const OPCODE_ASSERT_EQ_BIT: i32 = 14;

impl InstructionRepr {
    pub fn encode(&self) -> Vec<BigInt> {
        // Convert the offsets from possibly negative numbers in the range [-2^15, 2^15)
        // to positive numbers in the range [0, 2^16) centered around 2^15.
        let off0_enc: u64 = ((self.off0 as i32) + (1 << (OFFSET_BITS - 1))) as u64;
        let off1_enc: u64 = ((self.off1 as i32) + (1 << (OFFSET_BITS - 1))) as u64;
        let off2_enc: u64 = ((self.off2 as i32) + (1 << (OFFSET_BITS - 1))) as u64;

        let mut flags = 0;
        if self.dst_register == Register::FP {
            flags |= 1 << DST_REG_BIT;
        }
        if self.op0_register == Register::FP {
            flags |= 1 << OP0_REG_BIT;
        }
        assert_eq!(
            self.imm.is_some(),
            self.op1_addr == Op1Addr::Imm,
            "Immediate must appear iff op1_addr is Op1Addr.IMM"
        );
        flags |= match self.op1_addr {
            Op1Addr::Imm => 1 << OP1_IMM_BIT,
            Op1Addr::AP => 1 << OP1_AP_BIT,
            Op1Addr::FP => 1 << OP1_FP_BIT,
            Op1Addr::Op0 => 0,
        };

        flags |= match self.res {
            Res::Add => 1 << RES_ADD_BIT,
            Res::Mul => 1 << RES_MUL_BIT,
            Res::Op1 => 0,
            Res::Unconstrained => 0,
        };

        assert_eq!(
            self.res == Res::Unconstrained,
            self.pc_update == PcUpdate::Jnz,
            "res must be Unconstrained iff pc_update is Jnz"
        );

        flags |= match self.pc_update {
            PcUpdate::Jump => 1 << PC_JUMP_ABS_BIT,
            PcUpdate::JumpRel => 1 << PC_JUMP_REL_BIT,
            PcUpdate::Jnz => 1 << PC_JNZ_BIT,
            PcUpdate::Regular => 0,
        };

        assert_eq!(
            self.ap_update == ApUpdate::Add2,
            self.opcode == Opcode::Call,
            "ap_update is Add2 iff opcode is Call"
        );

        flags |= match self.ap_update {
            ApUpdate::Add => 1 << AP_ADD_BIT,
            ApUpdate::Add1 => 1 << AP_ADD1_BIT,
            ApUpdate::Add2 => 0,
            ApUpdate::Regular => 0,
        };

        assert_eq!(
            self.fp_update,
            match self.opcode {
                Opcode::Nop => FpUpdate::Regular,
                Opcode::Call => FpUpdate::ApPlus2,
                Opcode::Ret => FpUpdate::Dst,
                Opcode::AssertEq => FpUpdate::Regular,
            },
            "fp_update {:?} does not match opcode {:?}.",
            self.fp_update,
            self.opcode
        );

        flags |= match self.opcode {
            Opcode::Call => 1 << OPCODE_CALL_BIT,
            Opcode::Ret => 1 << OPCODE_RET_BIT,
            Opcode::AssertEq => 1 << OPCODE_ASSERT_EQ_BIT,
            Opcode::Nop => 0,
        };

        let mut encoding: u64 = flags << (3 * OFFSET_BITS);
        encoding |= off2_enc << (2 * OFFSET_BITS);
        encoding |= off1_enc << (OFFSET_BITS);
        encoding |= off0_enc;

        let bigint_encoding = BigInt::from(encoding);
        if let Some(imm) = self.imm.clone() {
            vec![bigint_encoding, imm]
        } else {
            vec![bigint_encoding]
        }
    }
}
