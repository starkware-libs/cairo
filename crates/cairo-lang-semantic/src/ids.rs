use std::hash::{Hash, Hasher};
use std::sync::Arc;

use cairo_lang_utils::define_short_id;

use crate::db::SemanticGroup;
use crate::plugin::AnalyzerPlugin;

/// An Id allowing interning [`AnalyzerPlugin`] into Salsa database.
#[derive(Clone, Debug)]
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

// `PartialEq` and `Hash` cannot be derived on `Arc<dyn ...>`,
// but pointer-based equality and hash semantics are enough in this case.
impl PartialEq for AnalyzerPluginLongId {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for AnalyzerPluginLongId {}

impl Hash for AnalyzerPluginLongId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

define_short_id!(
    AnalyzerPluginId,
    AnalyzerPluginLongId,
    SemanticGroup,
    lookup_intern_analyzer_plugin,
    intern_analyzer_plugin
);
