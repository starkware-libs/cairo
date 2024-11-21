use std::sync::Arc;

use cairo_lang_utils::define_short_id;

use crate::db::SemanticGroup;
use crate::plugin::AnalyzerPlugin;

// An Id allowing interning [`AnalyzerPlugin`] into Salsa database.
#[derive(Debug, Hash)]
pub struct AnalyzerPluginLongId(pub Arc<dyn AnalyzerPlugin>);

impl AnalyzerPlugin for AnalyzerPluginLongId {
    fn diagnostics(
        &self,
        db: &dyn crate::db::SemanticGroup,
        module_id: cairo_lang_defs::ids::ModuleId,
    ) -> Vec<cairo_lang_defs::plugin::PluginDiagnostic> {
        self.0.diagnostics(db, module_id)
    }

    fn declared_allows(&self) -> Vec<String> {
        self.0.declared_allows()
    }
}

impl Clone for AnalyzerPluginLongId {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}

// `PartialEq` cannot be derived because of the `Arc`, but since [`AnalyzerPlugin`] has
// `DynEq` providing `PartialEq`'s functionality, the implementation is easily delegated.
impl PartialEq for AnalyzerPluginLongId {
    fn eq(&self, other: &Self) -> bool {
        &*self.0 == &*other.0
    }
}

impl Eq for AnalyzerPluginLongId {}

define_short_id!(
    AnalyzerPluginId,
    AnalyzerPluginLongId,
    SemanticGroup,
    lookup_intern_analyzer_plugin,
    intern_analyzer_plugin
);
