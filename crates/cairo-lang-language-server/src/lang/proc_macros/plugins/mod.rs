use std::sync::Arc;

use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{InlineMacroExprPlugin, MacroPlugin, PluginDiagnostic};
use cairo_lang_macro::{Severity, TokenStream};
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use inline::inline_macro_generate_code;
use proc_macro_server_api::methods::expand::ExpandInlineMacroParams;
use regular::macro_generate_code;

use super::cache_group::ProcMacroCacheGroup;
use super::client::ProcMacroClient;
use super::downcast::unsafe_downcast_ref;

mod inline;
mod regular;

/// Important: NEVER make it visible outside of this crate. See [`unsafe_downcast_ref`] for more
/// info.
pub(crate) fn proc_macro_plugin_suite(client: &ProcMacroClient) -> Option<PluginSuite> {
    let defined_macros = client.defined_macros()?;

    let mut plugin_suite = PluginSuite::default();

    plugin_suite.add_plugin_ex(Arc::new(ProcMacroPlugin {
        defined_attributes: defined_macros.attributes,
        defined_derives: defined_macros.derives,
        defined_executable_attributes: defined_macros.executables,
    }));

    let inline_plugin = Arc::new(InlineProcMacroPlugin);

    for inline_macro in defined_macros.inline_macros {
        plugin_suite.add_inline_macro_plugin_ex(&inline_macro, inline_plugin.clone());
    }

    Some(plugin_suite)
}

/// Important: NEVER make it public. See [`unsafe_downcast_ref`] for more info.
#[derive(Debug)]
struct ProcMacroPlugin {
    defined_attributes: Vec<String>,
    defined_derives: Vec<String>,
    defined_executable_attributes: Vec<String>,
}

impl MacroPlugin for ProcMacroPlugin {
    fn generate_code(
        &self,
        db: &dyn cairo_lang_syntax::node::db::SyntaxGroup,
        item_ast: cairo_lang_syntax::node::ast::ModuleItem,
        _metadata: &cairo_lang_defs::plugin::MacroPluginMetadata<'_>,
    ) -> cairo_lang_defs::plugin::PluginResult {
        let analysis_db = unsafe {
            // Safety: We use this plugin only in LS.
            unsafe_downcast_ref(db)
        };

        macro_generate_code(analysis_db, item_ast, &self.defined_attributes, &self.defined_derives)
    }

    fn declared_attributes(&self) -> Vec<String> {
        let mut result = self.defined_attributes.clone();

        result.extend_from_slice(&self.defined_executable_attributes);

        result
    }

    fn declared_derives(&self) -> Vec<String> {
        self.defined_derives.clone()
    }
}

/// Important: NEVER make it public. See [`unsafe_downcast_ref`] for more info.
#[derive(Debug)]
struct InlineProcMacroPlugin;

impl InlineMacroExprPlugin for InlineProcMacroPlugin {
    fn generate_code(
        &self,
        db: &dyn cairo_lang_syntax::node::db::SyntaxGroup,
        item_ast: &cairo_lang_syntax::node::ast::ExprInlineMacro,
        _metadata: &cairo_lang_defs::plugin::MacroPluginMetadata<'_>,
    ) -> cairo_lang_defs::plugin::InlinePluginResult {
        inline_macro_generate_code(db, item_ast, |db, token_stream| {
            let analysis_db = unsafe {
                // Safety: We use this plugin only in LS.
                unsafe_downcast_ref(db)
            };

            let name = item_ast.path(db).as_syntax_node().get_text(db);

            analysis_db
                .get_inline_macros_expansion(ExpandInlineMacroParams { name, item: token_stream })
        })
    }
}

// Code from scarb.
trait FromSyntaxNode {
    fn from_syntax_node(db: &dyn SyntaxGroup, node: &impl TypedSyntaxNode) -> Self;
}

impl FromSyntaxNode for TokenStream {
    fn from_syntax_node(db: &dyn SyntaxGroup, node: &impl TypedSyntaxNode) -> Self {
        let mut builder = PatchBuilder::new(db, node);
        builder.add_node(node.as_syntax_node());
        Self::new(builder.build().0)
    }
}

fn into_cairo_diagnostics(
    diagnostics: Vec<cairo_lang_macro::Diagnostic>,
    stable_ptr: SyntaxStablePtrId,
) -> Vec<PluginDiagnostic> {
    diagnostics
        .into_iter()
        .map(|diag| PluginDiagnostic {
            stable_ptr,
            message: diag.message,
            severity: match diag.severity {
                Severity::Error => cairo_lang_diagnostics::Severity::Error,
                Severity::Warning => cairo_lang_diagnostics::Severity::Warning,
            },
        })
        .collect()
}
