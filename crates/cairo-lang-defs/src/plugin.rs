use std::any::Any;
use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_diagnostics::Severity;
use cairo_lang_filesystem::cfg::CfgSet;
use cairo_lang_filesystem::ids::CodeMapping;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use smol_str::SmolStr;

/// A trait for arbitrary data that a macro generates along with a generated file.
pub trait GeneratedFileAuxData: std::fmt::Debug + Sync + Send {
    fn as_any(&self) -> &dyn Any;
    fn eq(&self, other: &dyn GeneratedFileAuxData) -> bool;
}

#[derive(Clone, Debug)]
pub struct DynGeneratedFileAuxData(pub Arc<dyn GeneratedFileAuxData>);
impl DynGeneratedFileAuxData {
    pub fn new<T: GeneratedFileAuxData + 'static>(aux_data: T) -> Self {
        DynGeneratedFileAuxData(Arc::new(aux_data))
    }
}
impl Deref for DynGeneratedFileAuxData {
    type Target = Arc<dyn GeneratedFileAuxData>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl PartialEq for DynGeneratedFileAuxData {
    fn eq(&self, that: &DynGeneratedFileAuxData) -> bool {
        GeneratedFileAuxData::eq(&*self.0, &*that.0)
    }
}
impl Eq for DynGeneratedFileAuxData {}

/// Virtual code file generated by a plugin.
pub struct PluginGeneratedFile {
    /// Name for the virtual file. Will appear in diagnostics.
    pub name: SmolStr,
    /// Code content for the file.
    pub content: String,
    /// A code mapper, to allow more readable diagnostics that originate in plugin generated
    /// virtual files.
    pub code_mappings: Vec<CodeMapping>,
    /// Arbitrary data that the plugin generates along with the file.
    pub aux_data: Option<DynGeneratedFileAuxData>,
}

/// Result of plugin code generation.
#[derive(Default)]
pub struct PluginResult {
    /// Filename, content.
    pub code: Option<PluginGeneratedFile>,
    /// Diagnostics.
    pub diagnostics: Vec<PluginDiagnostic>,
    /// If true - the original item should be removed, if false - it should remain as is.
    pub remove_original_item: bool,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PluginDiagnostic {
    pub stable_ptr: SyntaxStablePtrId,
    pub message: String,
    pub severity: Severity,
}
impl PluginDiagnostic {
    pub fn error(stable_ptr: SyntaxStablePtrId, message: String) -> PluginDiagnostic {
        PluginDiagnostic { stable_ptr, message, severity: Severity::Error }
    }
    pub fn warning(stable_ptr: SyntaxStablePtrId, message: String) -> PluginDiagnostic {
        PluginDiagnostic { stable_ptr, message, severity: Severity::Warning }
    }
}

/// A structure containing additional info about
/// the current module item on which macro plugin operates.
pub struct MacroPluginMetadata<'a> {
    /// Config set of a crate to which the current item belongs.
    pub cfg_set: &'a CfgSet,
}

// TOD(spapini): Move to another place.
/// A trait for a macro plugin: external plugin that generates additional code for items.
pub trait MacroPlugin: std::fmt::Debug + Sync + Send {
    /// Generates code for an item. If no code should be generated returns None.
    /// Otherwise, returns (virtual_module_name, module_content), and a virtual submodule
    /// with that name and content should be created.
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult;

    /// Attributes this plugin uses.
    /// Attributes the plugin uses without declaring here are likely to cause a compilation error
    /// for unknown attribute.
    /// Note: They may not cause a diagnostic if some other plugin declares such attribute, but
    /// plugin writers should not rely on that.
    fn declared_attributes(&self) -> Vec<String>;
}

/// Result of plugin code generation.
#[derive(Default)]
pub struct InlinePluginResult {
    pub code: Option<PluginGeneratedFile>,
    /// Diagnostics.
    pub diagnostics: Vec<PluginDiagnostic>,
}

pub trait InlineMacroExprPlugin: std::fmt::Debug + Sync + Send {
    /// Generates code for an item. If no code should be generated returns None.
    /// Otherwise, returns (virtual_module_name, module_content), and a virtual submodule
    /// with that name and content should be created.
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: &ast::ExprInlineMacro,
    ) -> InlinePluginResult;
}

/// A trait for easier addition of macro plugins.
pub trait NamedPlugin: Default + 'static {
    const NAME: &'static str;
}
