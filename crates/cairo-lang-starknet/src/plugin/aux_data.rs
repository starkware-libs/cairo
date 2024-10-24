use cairo_lang_defs::plugin::GeneratedFileAuxData;

use super::events::EventData;

/// Contract related auxiliary data of the Starknet plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct StarkNetContractAuxData {
    /// A list of contracts that were processed by the plugin.
    pub contract_name: smol_str::SmolStr,
}
impl GeneratedFileAuxData for StarkNetContractAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
/// Contract related auxiliary data of the Starknet plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct StarkNetEventAuxData {
    pub event_data: EventData,
}
impl GeneratedFileAuxData for StarkNetEventAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
