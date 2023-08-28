use cairo_lang_defs::patcher::RewriteNode;

use super::component::ComponentSpecificGenerationData;
use super::contract::ContractSpecificGenerationData;

/// The data for generating the code of a contract.
#[derive(Default)]
pub struct ContractGenerationData {
    /// Common data - relevant for all starknet modules (component/contract).
    pub common: StarknetModuleCommonGenerationData,
    /// Specific data - relevant only for contracts.
    pub specific: ContractSpecificGenerationData,
}
impl ContractGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "$common$\n$specific$",
            [
                ("common".to_string(), self.common.into_rewrite_node()),
                ("specific".to_string(), self.specific.into_rewrite_node()),
            ]
            .into(),
        )
    }
}

/// The data for generating the code of a component.
#[derive(Default)]
pub struct ComponentGenerationData {
    /// Common data - relevant for all starknet modules (component/contract).
    pub common: StarknetModuleCommonGenerationData,
    /// Specific data - relevant only for components.
    pub specific: ComponentSpecificGenerationData,
}
impl ComponentGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "$common$\n\n$specific$",
            [
                ("common".to_string(), self.common.into_rewrite_node()),
                ("specific".to_string(), self.specific.into_rewrite_node()),
            ]
            .into(),
        )
    }
}

/// Accumulated data for code generation that is common to both contracts and components.
#[derive(Default)]
pub struct StarknetModuleCommonGenerationData {
    /// The code of the state struct.
    pub state_struct_code: RewriteNode,
    /// The generated event-related code.
    pub event_code: RewriteNode,
    /// Use declarations to add to the internal submodules.
    pub extra_uses_node: RewriteNode,
}
impl StarknetModuleCommonGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "$event_code$

$state_struct_code$",
            [
                ("event_code".to_string(), self.event_code),
                ("state_struct_code".to_string(), self.state_struct_code),
            ]
            .into(),
        )
    }
}
