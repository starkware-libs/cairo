use cairo_lang_defs::patcher::RewriteNode;

use super::contract::{ComponentSpecificGenerationData, ContractSpecificGenerationData};

#[derive(Default)]
pub struct ContractGenerationData {
    pub common: StarknetModuleCommonGenerationData,
    pub specific: ContractSpecificGenerationData,
}
impl ContractGenerationData {
    pub fn to_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "$common$
$specific$",
            [
                ("common".to_string(), self.common.to_rewrite_node()),
                ("specific".to_string(), self.specific.to_rewrite_node()),
            ]
            .into(),
        )
    }
}
#[derive(Default)]
pub struct ComponentGenerationData {
    pub common: StarknetModuleCommonGenerationData,
    pub specific: ComponentSpecificGenerationData,
}
impl ComponentGenerationData {
    pub fn to_rewrite_node(self) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "$common$
$specific$",
            [
                ("common".to_string(), self.common.to_rewrite_node()),
                ("specific".to_string(), self.specific.to_rewrite_node()),
            ]
            .into(),
        )
    }
}

/// Accumulated data for generation that is common to both contracts and components.
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
    pub fn to_rewrite_node(self) -> RewriteNode {
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
