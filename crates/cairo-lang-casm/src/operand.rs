use std::fmt::Display;

use cairo_lang_utils::bigint::BigIntAsHex;
use num_bigint::BigInt;
use parity_scale_codec::{Decode, Encode, Input};
use parity_scale_codec_derive::{Decode as DecodeDerive, Encode as EncodeDerive};
use serde::{Deserialize, Serialize};

#[cfg(test)]
#[path = "operand_test.rs"]
mod test;

#[derive(
    Copy, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize, EncodeDerive, DecodeDerive,
)]
pub enum Register {
    #[codec(index = 0)]
    AP,
    #[codec(index = 1)]
    FP,
}
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::AP => write!(f, "ap"),
            Register::FP => write!(f, "fp"),
        }
    }
}

// Represents the rhs operand of an assert equal InstructionBody.
#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq)]
pub enum ResOperand {
    Deref(CellRef),
    DoubleDeref(CellRef, i16),
    Immediate(BigIntAsHex),
    BinOp(BinOpOperand),
}
impl Display for ResOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResOperand::Deref(operand) => write!(f, "{operand}"),
            ResOperand::DoubleDeref(operand, offset) => write!(f, "[{operand} + {offset}]"),
            ResOperand::Immediate(operand) => write!(f, "{}", operand.value),
            ResOperand::BinOp(operand) => write!(f, "{operand}"),
        }
    }
}
impl From<DerefOrImmediate> for ResOperand {
    fn from(x: DerefOrImmediate) -> Self {
        match x {
            DerefOrImmediate::Deref(deref) => ResOperand::Deref(deref),
            DerefOrImmediate::Immediate(imm) => ResOperand::Immediate(imm),
        }
    }
}

impl<T: Into<BigIntAsHex>> From<T> for ResOperand {
    fn from(imm: T) -> Self {
        ResOperand::Immediate(imm.into())
    }
}
// Can't derive the trait because BigIntAsHex doesn't implement Encode.
impl Encode for ResOperand {
    fn encode_to<T: parity_scale_codec::Output + ?Sized>(&self, dest: &mut T) {
        match self {
            Self::Deref(cell_ref) => {
                0u8.encode_to(dest);
                cell_ref.encode_to(dest);
            }
            Self::DoubleDeref(cell_ref, i) => {
                1u8.encode_to(dest);
                cell_ref.encode_to(dest);
                i.encode_to(dest);
            }
            Self::Immediate(hex) => {
                2u8.encode_to(dest);
                encode_bigint_to(hex, dest);
            }
            Self::BinOp(v) => {
                3u8.encode_to(dest);
                v.encode_to(dest);
            }
        };
    }
}
impl Decode for ResOperand {
    fn decode<I: Input>(input: &mut I) -> Result<Self, parity_scale_codec::Error> {
        let discriminant = input.read_byte()?;
        match discriminant {
            0 => Ok(Self::Deref(CellRef::decode(input)?)),
            1 => Ok(Self::DoubleDeref(CellRef::decode(input)?, i16::decode(input)?)),
            2 => Ok(Self::Immediate(decode_bigint(input)?)),
            3 => Ok(Self::BinOp(BinOpOperand::decode(input)?)),
            _ => Err(parity_scale_codec::Error::from("Unrecognized discriminant.")),
        }
    }
}

/// Represents an operand of the form [reg + offset].
#[derive(Serialize, Deserialize, Copy, Clone, Debug, Eq, PartialEq, EncodeDerive, DecodeDerive)]
pub struct CellRef {
    pub register: Register,
    pub offset: i16,
}
impl Display for CellRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} + {}]", self.register, self.offset)
    }
}

/// Returns an AP DerefOperand with the given offset.
pub fn ap_cell_ref(offset: i16) -> CellRef {
    CellRef { register: Register::AP, offset }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq)]
pub enum DerefOrImmediate {
    Deref(CellRef),
    Immediate(BigIntAsHex),
}
impl Display for DerefOrImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DerefOrImmediate::Deref(operand) => write!(f, "{operand}"),
            DerefOrImmediate::Immediate(operand) => write!(f, "{}", operand.value),
        }
    }
}
impl<T: Into<BigIntAsHex>> From<T> for DerefOrImmediate {
    fn from(x: T) -> Self {
        DerefOrImmediate::Immediate(x.into())
    }
}
impl From<CellRef> for DerefOrImmediate {
    fn from(x: CellRef) -> Self {
        DerefOrImmediate::Deref(x)
    }
}

impl Encode for DerefOrImmediate {
    fn encode_to<T: parity_scale_codec::Output + ?Sized>(&self, dest: &mut T) {
        match self {
            Self::Deref(cell_ref) => {
                0u8.encode_to(dest);
                cell_ref.encode_to(dest);
            }
            Self::Immediate(hex) => {
                1u8.encode_to(dest);
                encode_bigint_to(hex, dest);
            }
        };
    }
}

impl Decode for DerefOrImmediate {
    fn decode<I: Input>(input: &mut I) -> Result<Self, parity_scale_codec::Error> {
        let discriminant = input.read_byte()?;
        match discriminant {
            0 => Ok(Self::Deref(CellRef::decode(input)?)),
            1 => Ok(Self::Immediate(decode_bigint(input)?)),
            _ => Err(parity_scale_codec::Error::from("Unrecognized discriminant.")),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, EncodeDerive, DecodeDerive)]
pub enum Operation {
    #[codec(index = 0)]
    Add,
    #[codec(index = 1)]
    Mul,
}
impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Add => write!(f, "+"),
            Operation::Mul => write!(f, "*"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, EncodeDerive, DecodeDerive)]
pub struct BinOpOperand {
    pub op: Operation,
    pub a: CellRef,
    pub b: DerefOrImmediate,
}
impl Display for BinOpOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.a, self.op, self.b)
    }
}

// Can't implement encode/decode for BigIntAsHex as it is not defined in this crate.
pub(crate) fn encode_bigint_to<T: parity_scale_codec::Output + ?Sized>(
    hex: &BigIntAsHex,
    dest: &mut T,
) {
    let (sign, data) = hex.value.to_bytes_be();
    match sign {
        num_bigint::Sign::Minus => 0u8.encode_to(dest),
        num_bigint::Sign::NoSign => 1u8.encode_to(dest),
        num_bigint::Sign::Plus => 2u8.encode_to(dest),
    };
    // TODO(yair): better way to encode vec?
    let len = data.len() as u64;
    len.encode_to(dest);
    for b in data.as_slice() {
        b.encode_to(dest);
    }
}

pub(crate) fn decode_bigint<I: Input>(
    input: &mut I,
) -> Result<BigIntAsHex, parity_scale_codec::Error> {
    let discriminant = input.read_byte()?;
    let sign = match discriminant {
        0u8 => num_bigint::Sign::Minus,
        1u8 => num_bigint::Sign::NoSign,
        2u8 => num_bigint::Sign::Plus,
        _ => {
            return Err(parity_scale_codec::Error::from("Bad sign encoding."));
        }
    };
    // TODO(yair): better way to decode vec?
    let data_len = u64::decode(input)?;
    let mut bytes: Vec<u8> = Vec::with_capacity(data_len as usize);
    for _ in 0..data_len {
        bytes.push(input.read_byte()?);
    }
    Ok(BigIntAsHex { value: BigInt::from_bytes_be(sign, bytes.as_slice()) })
}
