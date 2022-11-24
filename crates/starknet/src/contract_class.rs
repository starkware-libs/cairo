use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use sierra::{self};

use crate::abi;

#[cfg(test)]
#[path = "contract_class_test.rs"]
mod test;

/// Represents a contract in the StarkNet network.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractClass {
    pub sierra_program: sierra::program::Program,
    pub entry_points_by_type: ContractEntryPoints,
    pub abi: abi::Contract,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContractEntryPoints {
    #[serde(rename = "EXTERNAL")]
    pub external: Vec<SierraEntryPoint>,
    #[serde(rename = "L1_HANDLER")]
    pub l1_handler: Vec<SierraEntryPoint>,
    #[serde(rename = "CONSTRUCTOR")]
    pub constructor: Vec<SierraEntryPoint>,
}

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SierraEntryPoint {
    /// A field element that encodes the signature of the called function.
    pub selector: BigUint,
    // The function in the sierra program.
    pub function_id: usize,
}
