use std::sync::Arc;

use cairo_lang_defs::{
    patcher::PatchBuilder,
    plugin::{InlineMacroExprPlugin, MacroPlugin},
};
use cairo_lang_semantic::plugin::PluginSuite;

use super::{cache_group::ProcMacroCacheGroup, downcast::unsafe_downcast_ref};
use cairo_lang_macro::TokenStream;
use cairo_lang_syntax::node::{db::SyntaxGroup, TypedSyntaxNode};
use inline::inline_macro_generate_code;
use regular::macro_generate_code;

mod inline;
mod regular;

/// Important: NEVER make it visible outside of this crate. See [`unsafe_downcast_ref`] for more
/// info.
pub(crate) fn proc_macro_plugin_suite(
    supported_macro_attributes: Vec<String>,
    supported_derives: Vec<String>,
    supported_other_attributes: Vec<String>,
    supported_inline_macros: Vec<String>,
) -> PluginSuite {
    let mut plugin_suite = PluginSuite::default();

    plugin_suite.add_plugin_ex(Arc::new(ProcMacroPlugin {
        defined_attributes: supported_macro_attributes,
        defined_derives: supported_derives,
        defined_other_attributes: supported_other_attributes,
    }));

    let inline_plugin = Arc::new(InlineProcMacroPlugin);

    for inline_macro in supported_inline_macros {
        plugin_suite.add_inline_macro_plugin_ex(&inline_macro, inline_plugin.clone());
    }

    plugin_suite
}

/// Important: NEVER make it public. See [`unsafe_downcast_ref`] for more info.
#[derive(Debug)]
struct ProcMacroPlugin {
    defined_attributes: Vec<String>,
    defined_derives: Vec<String>,
    defined_other_attributes: Vec<String>,
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

        macro_generate_code(&analysis_db, item_ast, &self.defined_attributes, &self.defined_derives)
    }

    fn declared_attributes(&self) -> Vec<String> {
        let mut result = self.defined_attributes.clone();

        result.extend_from_slice(&self.defined_other_attributes);

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
        inline_macro_generate_code(db, item_ast, |db, token_streams| {
            let analysis_db = unsafe {
                // Safety: We use this plugin only in LS.
                unsafe_downcast_ref(db)
            };

            analysis_db.get_inline_macros_expansion(token_streams)
        })
    }
}

//Code from scarb.

//added - can not be imported as this extension is part of scarb
pub trait FromSyntaxNode {
    fn from_syntax_node(db: &dyn SyntaxGroup, node: &impl TypedSyntaxNode) -> Self;
}

impl FromSyntaxNode for TokenStream {
    fn from_syntax_node(db: &dyn SyntaxGroup, node: &impl TypedSyntaxNode) -> Self {
        let mut builder = PatchBuilder::new(db, node);
        builder.add_node(node.as_syntax_node());
        Self::new(builder.build().0)
    }
}
//end added
