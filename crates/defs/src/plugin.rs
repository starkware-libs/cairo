use std::any::Any;
use std::ops::Deref;
use std::sync::Arc;

use smol_str::SmolStr;
use syntax::node::ast;
use syntax::node::db::SyntaxGroup;
use syntax::node::ids::SyntaxStablePtrId;

pub trait DiagnosticMapper: std::fmt::Debug + Sync + Send {
    fn as_any(&self) -> &dyn Any;
    fn map_diag(&self, diag: &dyn Any) -> Option<PluginDiagnostic>;
    fn eq(&self, other: &dyn DiagnosticMapper) -> bool;
}

#[derive(Clone, Debug)]
pub struct DynDiagnosticMapper(pub Arc<dyn DiagnosticMapper>);
impl DynDiagnosticMapper {
    pub fn new<T: DiagnosticMapper + 'static>(mapper: T) -> Self {
        DynDiagnosticMapper(Arc::new(mapper))
    }
}
impl Deref for DynDiagnosticMapper {
    type Target = Arc<dyn DiagnosticMapper>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl PartialEq for DynDiagnosticMapper {
    fn eq(&self, that: &DynDiagnosticMapper) -> bool {
        DiagnosticMapper::eq(&*self.0, &*that.0)
    }
}
impl Eq for DynDiagnosticMapper {}

#[derive(Debug, PartialEq, Eq)]
pub struct TrivialMapper {}

impl DiagnosticMapper for TrivialMapper {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn map_diag(&self, _diag: &dyn std::any::Any) -> Option<PluginDiagnostic> {
        None
    }

    fn eq(&self, other: &dyn DiagnosticMapper) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() { self == other } else { false }
    }
}

pub struct PluginGeneratedFile {
    pub name: SmolStr,
    pub content: String,
    pub diagnostic_mapper: DynDiagnosticMapper,
}

/// Result of plugin code generation.
#[derive(Default)]
pub struct PluginResult {
    /// Filename, content.
    pub code: Option<PluginGeneratedFile>,
    /// Diagnostics.
    pub diagnostics: Vec<PluginDiagnostic>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PluginDiagnostic {
    pub stable_ptr: SyntaxStablePtrId,
    pub message: String,
}

// TOD(spapini): Move to another place.
/// A trait for a macro plugin: external plugin that generates additional code for items.
pub trait MacroPlugin: std::fmt::Debug + Sync + Send {
    /// Generates code for an item. If no code should be generated returns None.
    /// Otherwise, returns (virtual_module_name, module_content), and a virtual submodule
    /// with that name and content should be created.
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult;
}
