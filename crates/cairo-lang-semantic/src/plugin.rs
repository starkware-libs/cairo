use std::sync::Arc;

use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::plugin::{InlineMacroExprPlugin, MacroPlugin, NamedPlugin, PluginDiagnostic};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::SemanticGroup;

/// A trait for an analyzer plugin: external plugin that generates additional diagnostics for
/// modules.
pub trait AnalyzerPlugin: std::fmt::Debug + Sync + Send {
    /// Runs the plugin on a module.
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic>;
}

/// A suite of plugins.
#[derive(Clone, Debug, Default)]
pub struct PluginSuite {
    /// The macro plugins, running on all items.
    pub plugins: Vec<Arc<dyn MacroPlugin>>,
    /// The inline macro plugins, running on matching inline macro expressions.
    pub inline_macro_plugins: OrderedHashMap<String, Arc<dyn InlineMacroExprPlugin>>,
    /// The analyzer plugins, running on all modules.
    pub analyzer_plugins: Vec<Arc<dyn AnalyzerPlugin>>,
}
impl PluginSuite {
    /// Adds a macro plugin.
    pub fn add_plugin_ex(&mut self, plugin: Arc<dyn MacroPlugin>) -> &mut Self {
        self.plugins.push(plugin);
        self
    }
    /// Adds a macro plugin.
    pub fn add_plugin<T: MacroPlugin + Default + 'static>(&mut self) -> &mut Self {
        self.add_plugin_ex(Arc::new(T::default()))
    }
    /// Adds an inline macro plugin.
    pub fn add_inline_macro_plugin_ex(
        &mut self,
        name: &str,
        plugin: Arc<dyn InlineMacroExprPlugin>,
    ) -> &mut Self {
        self.inline_macro_plugins.insert(name.into(), plugin);
        self
    }
    /// Adds an inline macro plugin.
    pub fn add_inline_macro_plugin<T: NamedPlugin + InlineMacroExprPlugin>(&mut self) -> &mut Self {
        self.add_inline_macro_plugin_ex(T::NAME, Arc::new(T::default()));
        self
    }
    /// Adds an analyzer plugin.
    pub fn add_analyzer_plugin_ex(&mut self, plugin: Arc<dyn AnalyzerPlugin>) -> &mut Self {
        self.analyzer_plugins.push(plugin);
        self
    }
    /// Adds an analyzer plugin.
    pub fn add_analyzer_plugin<T: AnalyzerPlugin + Default + 'static>(&mut self) -> &mut Self {
        self.add_analyzer_plugin_ex(Arc::new(T::default()))
    }
    /// Adds another plugin suite into this suite.
    pub fn add(&mut self, suite: PluginSuite) -> &mut Self {
        self.plugins.extend(suite.plugins);
        self.inline_macro_plugins.extend(suite.inline_macro_plugins);
        self.analyzer_plugins.extend(suite.analyzer_plugins);
        self
    }
}
