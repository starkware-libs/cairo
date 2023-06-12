#[cfg(test)]
#[path = "bigint_test.rs"]
mod test;

use std::ops::Neg;

use num_bigint::{BigInt, BigUint, ToBigInt};
use num_traits::{Num, Signed};
use parity_scale_codec::{Decode, Encode};
use schemars::JsonSchema;
use serde::ser::Serializer;
use serde::{Deserialize, Deserializer, Serialize};

/// A wrapper for BigUint that serializes as hex.
#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BigUintAsHex {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_uint", deserialize_with = "deserialize_big_uint")]
    pub value: BigUint,
}

fn deserialize_from_str<'a, D>(s: &str) -> Result<BigUint, D::Error>
where
    D: Deserializer<'a>,
{
    match s.strip_prefix("0x") {
        Some(num_no_prefix) => BigUint::from_str_radix(num_no_prefix, 16)
            .map_err(|error| serde::de::Error::custom(format!("{error}"))),
        None => Err(serde::de::Error::custom(format!("{s} does not start with `0x` is missing."))),
    }
}

pub fn serialize_big_uint<S>(num: &BigUint, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{num:#x}"))
}

pub fn deserialize_big_uint<'a, D>(deserializer: D) -> Result<BigUint, D::Error>
where
    D: Deserializer<'a>,
{
    let s = &String::deserialize(deserializer)?;
    deserialize_from_str::<D>(s)
}

// A wrapper for BigInt that serializes as hex.
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(transparent)]
pub struct BigIntAsHex {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_int", deserialize_with = "deserialize_big_int")]
    #[schemars(schema_with = "big_int_schema")]
    pub value: BigInt,
}

// BigInt doesn't implement JsonSchema, so we need to manually define it.
fn big_int_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    #[allow(dead_code)]
    #[derive(JsonSchema)]
    pub enum Sign {
        Minus,
        NoSign,
        Plus,
    }

    #[allow(dead_code)]
    #[derive(JsonSchema)]
    pub struct BigUint {
        data: Vec<u64>, // BigDigit is u64 or u32.
    }

    #[allow(dead_code)]
    #[derive(JsonSchema)]
    struct BigInt {
        sign: Sign,
        data: BigUint,
    }

    gen.subschema_for::<BigInt>()
}

impl<T: Into<BigInt>> From<T> for BigIntAsHex {
    fn from(x: T) -> Self {
        Self { value: x.into() }
    }
}

pub fn serialize_big_int<S>(num: &BigInt, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!(
        "{}{:#x}",
        if num.is_negative() { "-" } else { "" },
        num.magnitude()
    ))
}

pub fn deserialize_big_int<'a, D>(deserializer: D) -> Result<BigInt, D::Error>
where
    D: Deserializer<'a>,
{
    let s = &String::deserialize(deserializer)?;
    match s.strip_prefix('-') {
        Some(abs_value) => Ok(deserialize_from_str::<D>(abs_value)?.to_bigint().unwrap().neg()),
        None => Ok(deserialize_from_str::<D>(s)?.to_bigint().unwrap()),
    }
}

impl Encode for BigIntAsHex {
    fn size_hint(&self) -> usize {
        // sign, len, data.
        1 + 8 + self.value.to_bytes_be().1.len()
    }
    fn encode_to<T: parity_scale_codec::Output + ?Sized>(&self, dest: &mut T) {
        let (sign, data) = self.value.to_bytes_be();
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
}

impl Decode for BigIntAsHex {
    fn decode<I: parity_scale_codec::Input>(
        input: &mut I,
    ) -> Result<Self, parity_scale_codec::Error> {
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
        Ok(Self { value: BigInt::from_bytes_be(sign, bytes.as_slice()) })
    }
}
