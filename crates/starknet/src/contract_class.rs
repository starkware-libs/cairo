use num_bigint::BigInt;
use serde::ser::Serializer;
use serde::Serialize;

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

/// Represents a contract in the StarkNet network.
// TODO(ilya): deserialize.
#[derive(Default, Debug, Serialize)]
pub struct ContractClass {
    pub sierra_program: String,
    pub entry_points_by_type: ContractEntryPoints,
}

#[derive(Default, Debug, Serialize)]
pub struct ContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<ContractEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<ContractEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<ContractEntryPoint>,
}

#[derive(Default, Debug, Serialize)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_int")]
    pub selector: BigInt,
    /// The offset of the instruction that should be called within the contract bytecode.
    #[serde(serialize_with = "serialize_big_int")]
    pub offset: BigInt,
}

fn serialize_big_int<S>(num: &BigInt, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{:#x}", num))
}
