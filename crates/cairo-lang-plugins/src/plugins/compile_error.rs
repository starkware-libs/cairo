use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};

/// Plugin that allows writing item level `compile_error!` causing a diagnostic.
/// Useful for testing that `cfg` attributes are valid.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct CompileErrorPlugin;

impl MacroPlugin for CompileErrorPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let item_ast_ptr = item_ast.stable_ptr(db);
        if let ast::ModuleItem::InlineMacro(inline_macro_ast) = item_ast.clone() {
            let Some(legacy_inline_macro_ast) = inline_macro_ast.as_legacy_inline_macro(db) else {
                return PluginResult::diagnostic_only(not_legacy_macro_diagnostic(
                    inline_macro_ast.as_syntax_node().stable_ptr(db),
                ));
            };
            if legacy_inline_macro_ast.name(db).text(db) == "compile_error" {
                let compilation_error_arg = extract_macro_single_unnamed_arg!(
                    db,
                    &legacy_inline_macro_ast,
                    ast::WrappedArgList::ParenthesizedArgList(_),
                    item_ast_ptr
                );
                let ast::Expr::String(err_message) = compilation_error_arg.clone() else {
                    return PluginResult::diagnostic_only(PluginDiagnostic::error_with_inner_span(
                        db,
                        item_ast_ptr,
                        compilation_error_arg.as_syntax_node(),
                        "`compile_error!` argument must be an unnamed string argument.".to_string(),
                    ));
                };
                return PluginResult::diagnostic_only(PluginDiagnostic::error(
                    item_ast_ptr,
                    err_message.text(db).to_string(),
                ));
            }
        }
        PluginResult { code: None, diagnostics: vec![], remove_original_item: false }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![]
    }
}
