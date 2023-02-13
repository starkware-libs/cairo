use cairo_lang_defs::plugin::GeneratedFileAuxData;
use cairo_lang_diagnostics::DiagnosticEntry;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::patcher::Patches;
use cairo_lang_semantic::plugin::{
    AsDynGeneratedFileAuxData, PluginAuxData, PluginMappedDiagnostic,
};
use cairo_lang_semantic::SemanticDiagnostic;

/// Contract related auxiliary data of the Starknet plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct StarkNetContractAuxData {
    /// Patches of code that need translation in case they have diagnostics.
    pub patches: Patches,

    /// A list of contracts that were processed by the plugin.
    pub contracts: Vec<smol_str::SmolStr>,
}
impl GeneratedFileAuxData for StarkNetContractAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
impl AsDynGeneratedFileAuxData for StarkNetContractAuxData {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static) {
        self
    }
}
impl PluginAuxData for StarkNetContractAuxData {
    fn map_diag(
        &self,
        db: &(dyn SemanticGroup + 'static),
        diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        let Some(diag) = diag.downcast_ref::<SemanticDiagnostic>() else {return None;};
        let span = self
            .patches
            .translate(db.upcast(), diag.stable_location.diagnostic_location(db.upcast()).span)?;
        Some(PluginMappedDiagnostic { span, message: diag.format(db) })
    }
}

/// Contract related auxiliary data of the Starknet plugin.
#[derive(Debug, PartialEq, Eq)]
pub struct StarkNetABIAuxData {
    /// Patches of code that need translation in case they have diagnostics.
    pub patches: Patches,
}
impl GeneratedFileAuxData for StarkNetABIAuxData {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}
impl AsDynGeneratedFileAuxData for StarkNetABIAuxData {
    fn as_dyn_macro_token(&self) -> &(dyn GeneratedFileAuxData + 'static) {
        self
    }
}
impl PluginAuxData for StarkNetABIAuxData {
    fn map_diag(
        &self,
        db: &(dyn SemanticGroup + 'static),
        diag: &dyn std::any::Any,
    ) -> Option<PluginMappedDiagnostic> {
        let Some(diag) = diag.downcast_ref::<SemanticDiagnostic>() else {return None;};
        let span = self
            .patches
            .translate(db.upcast(), diag.stable_location.diagnostic_location(db.upcast()).span)?;
        Some(PluginMappedDiagnostic { span, message: diag.format(db) })
    }
}
