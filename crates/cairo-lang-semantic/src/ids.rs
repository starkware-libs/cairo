use std::hash::{Hash, Hasher};
use std::sync::Arc;

use cairo_lang_utils::define_short_id;
use salsa::Database;

use crate::plugin::AnalyzerPlugin;

/// An Id allowing interning [`AnalyzerPlugin`] into Salsa database.
#[derive(Clone, Debug)]
pub struct AnalyzerPluginLongId(pub Arc<dyn AnalyzerPlugin>);

impl AnalyzerPlugin for AnalyzerPluginLongId {
    fn diagnostics<'db>(
        &self,
        db: &'db dyn Database,
        module_id: cairo_lang_defs::ids::ModuleId<'db>,
    ) -> Vec<cairo_lang_defs::plugin::PluginDiagnostic<'db>> {
        self.0.diagnostics(db, module_id)
    }

    fn declared_allows(&self) -> Vec<String> {
        self.0.declared_allows()
    }

    fn plugin_type_id(&self) -> std::any::TypeId {
        // Ensure the implementation for `AnalyzerPluginLongId` returns the same value
        // as the underlying plugin object.
        self.0.plugin_type_id()
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

define_short_id!(AnalyzerPluginId, AnalyzerPluginLongId);
