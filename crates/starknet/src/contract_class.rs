use num_bigint::BigInt;
use serde::ser::{SerializeStruct, Serializer};
use serde::Serialize;

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

/// Represents a contract in the StarkNet network.
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

#[derive(Default, Debug)]
pub struct ContractEntryPoint {
    /// A field element that encodes the signature of the called function.
    pub selector: BigInt,
    /// The offset of the instruction that should be called within the contract bytecode.
    pub offset: BigInt,
}

fn serialize_big_int(num: &BigInt) -> String {
    return format!("{:#x}", num);
}

impl Serialize for ContractEntryPoint {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("ContractEntryPoint", 2)?;
        s.serialize_field("selector", &serialize_big_int(&self.selector))?;
        s.serialize_field("offset", &serialize_big_int(&self.offset))?;
        s.end()
    }
}
