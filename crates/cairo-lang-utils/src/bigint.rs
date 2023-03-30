#[cfg(test)]
#[path = "bigint_test.rs"]
mod test;

use std::ops::Neg;

use num_bigint::{BigInt, BigUint, ToBigInt};
use num_traits::{Num, Signed};
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
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BigIntAsHex {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_int", deserialize_with = "deserialize_big_int")]
    pub value: BigInt,
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
