use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use salsa::Database;

use super::component::ComponentSpecificGenerationData;
use super::contract::ContractSpecificGenerationData;

/// The data for generating the code of a contract.
#[derive(Default)]
pub struct ContractGenerationData<'db> {
    /// Common data relevant for all Starknet modules (component/contract).
    pub common: StarknetModuleCommonGenerationData<'db>,
    /// Specific data - relevant only for contracts.
    pub specific: ContractSpecificGenerationData<'db>,
}
impl<'db> ContractGenerationData<'db> {
    pub fn into_rewrite_node(
        self,
        db: &'db dyn Database,
        diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    ) -> RewriteNode<'db> {
        RewriteNode::interpolate_patched(
            "$common$\n$specific$",
            &[
                ("common".to_string(), self.common.into_rewrite_node(db, diagnostics)),
                ("specific".to_string(), self.specific.into_rewrite_node(db, diagnostics)),
            ]
            .into(),
        )
    }
}

/// The data for generating the code of a component.
#[derive(Default)]
pub struct ComponentGenerationData<'db> {
    /// Common data relevant for all Starknet modules (component/contract).
    pub common: StarknetModuleCommonGenerationData<'db>,
    /// Specific data - relevant only for components.
    pub specific: ComponentSpecificGenerationData<'db>,
}
impl<'db> ComponentGenerationData<'db> {
    pub fn into_rewrite_node(
        self,
        db: &'db dyn Database,
        diagnostics: &mut [PluginDiagnostic<'db>],
    ) -> RewriteNode<'db> {
        RewriteNode::interpolate_patched(
            "$common$\n\n$specific$",
            &[
                ("common".to_string(), self.common.into_rewrite_node(db, diagnostics)),
                ("specific".to_string(), self.specific.into_rewrite_node(db, diagnostics)),
            ]
            .into(),
        )
    }
}

/// Accumulated data for code generation that is common to both contracts and components.
#[derive(Default)]
pub struct StarknetModuleCommonGenerationData<'db> {
    /// The code of the state struct.
    pub state_struct_code: RewriteNode<'db>,
    /// The generated event-related code.
    pub event_code: RewriteNode<'db>,
}
impl<'db> StarknetModuleCommonGenerationData<'db> {
    pub fn into_rewrite_node(
        self,
        _db: &'db dyn Database,
        _diagnostics: &mut [PluginDiagnostic<'db>],
    ) -> RewriteNode<'db> {
        RewriteNode::interpolate_patched(
            "$event_code$

$state_struct_code$",
            &[
                ("event_code".to_string(), self.event_code),
                ("state_struct_code".to_string(), self.state_struct_code),
            ]
            .into(),
        )
    }
}
