use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use super::inline_macros::array::ArrayMacro;
use super::inline_macros::consteval_int::ConstevalIntMacro;

/// The result of expanding an inline macro.
#[derive(Debug, Default)]
pub struct InlineMacroExpanderData {
    pub result_code: String,
    pub code_changed: bool,
    pub diagnostics: Vec<PluginDiagnostic>,
}

/// A trait for inline macros. Each inline macro should implement the expanding logic in this trait.
pub trait InlineMacro {
    fn append_macro_code(
        &self,
        macro_expander_data: &mut InlineMacroExpanderData,
        db: &dyn SyntaxGroup,
        macro_ast: &ast::ExprInlineMacro,
    );
}

/// Returns the inline macro plugin for the given macro name, or None if no such plugin exists.
fn get_inline_macro_plugin(macro_name: &str) -> Option<Box<dyn InlineMacro>> {
    match macro_name {
        "array" => Some(Box::new(ArrayMacro)),
        "consteval_int" => Some(Box::new(ConstevalIntMacro)),
        _ => None,
    }
}

#[derive(Debug, Default)]
pub struct InlineMacroPlugin;
impl MacroPlugin for InlineMacroPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let mut expander_data = InlineMacroExpanderData::default();
        expander_data.expand_node(db, &item_ast.as_syntax_node());
        if expander_data.code_changed {
            PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "inline_macros".into(),
                    content: expander_data.result_code.clone(),
                    aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
                }),
                diagnostics: expander_data.diagnostics,
                remove_original_item: true,
            }
        } else {
            PluginResult {
                code: None,
                diagnostics: expander_data.diagnostics,
                remove_original_item: false,
            }
        }
    }
}

impl AsDynMacroPlugin for InlineMacroPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for InlineMacroPlugin {}

impl InlineMacroExpanderData {
    /// Traverse the syntax tree, accumolates any non-macro code and expand all inline macros.
    fn expand_node(&mut self, db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) {
        let node_kind = syntax_node.kind(db);
        if let SyntaxKind::ExprInlineMacro = node_kind {
            let inline_macro = ast::ExprInlineMacro::from_syntax_node(db, syntax_node.clone());
            self.handle_macro(db, &inline_macro);
        } else {
            if let Some(text) = syntax_node.text(db) {
                self.result_code.push_str(&text);
            }
            for child in syntax_node.children(db) {
                self.expand_node(db, &child);
            }
        }
    }

    /// Expand a single inline macro.
    fn handle_macro(&mut self, db: &dyn SyntaxGroup, inline_macro: &ast::ExprInlineMacro) {
        let macro_name = inline_macro.path(db).as_syntax_node().get_text(db);
        let macro_plugin = get_inline_macro_plugin(&macro_name);
        if let Some(macro_plugin) = macro_plugin {
            macro_plugin.append_macro_code(self, db, inline_macro);
        } else {
            self.result_code.push_str(&inline_macro.as_syntax_node().get_text(db));
            self.diagnostics.push(PluginDiagnostic {
                stable_ptr: inline_macro.stable_ptr().untyped(),
                message: format!("Unknown inline macro: {}", macro_name),
            });
        }
    }
}
