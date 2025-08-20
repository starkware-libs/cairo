use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use cairo_lang_defs::plugin::GeneratedFileAuxData;
use serde::{Deserialize, Serialize};

use super::events::EventData;

/// Contract related auxiliary data of the Starknet plugin.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct StarknetContractAuxData {
    /// A list of contracts that were processed by the plugin.
    pub contract_name: String,
}

#[typetag::serde]
impl GeneratedFileAuxData for StarknetContractAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
    fn hash_value(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}
/// Contract related auxiliary data of the Starknet plugin.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct StarknetEventAuxData {
    pub event_data: EventData,
}
#[typetag::serde]
impl GeneratedFileAuxData for StarknetEventAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
    fn hash_value(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}
