use num_bigint::BigInt;
use num_traits::Num;
use serde::ser::Serializer;
use serde::{Deserialize, Deserializer, Serialize};

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

/// Represents a contract in the StarkNet network.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractClass {
    pub sierra_program: String,
    pub entry_points_by_type: ContractEntryPoints,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<ContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<ContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<ContractEntryPoint>,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_int", deserialize_with = "deserialize_big_int")]
    pub selector: BigInt,
    /// The offset of the instruction that should be called within the contract bytecode.
    #[serde(serialize_with = "serialize_big_int", deserialize_with = "deserialize_big_int")]
    pub offset: BigInt,
}

fn serialize_big_int<S>(num: &BigInt, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{:#x}", num))
}

fn deserialize_big_int<'a, D>(deserializer: D) -> Result<BigInt, D::Error>
where
    D: Deserializer<'a>,
{
    let s = &String::deserialize(deserializer)?;
    match s.strip_prefix("0x") {
        Some(num_no_prefix) => BigInt::from_str_radix(num_no_prefix, 16)
            .map_err(|error| serde::de::Error::custom(format!("{}", error))),
        None => Err(serde::de::Error::custom(format!("{s} does not start with `0x` is missing."))),
    }
}
