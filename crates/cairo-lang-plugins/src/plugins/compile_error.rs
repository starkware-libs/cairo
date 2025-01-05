use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_defs::plugin_utils::PluginResultTrait;
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
        if let ast::ModuleItem::InlineMacro(inline_macro_ast) = item_ast {
            if inline_macro_ast.name(db).text(db) == "compile_error" {
                let compilation_error_arg = extract_macro_single_unnamed_arg!(
                    db,
                    &inline_macro_ast,
                    ast::WrappedArgList::ParenthesizedArgList(_)
                );
                let ast::Expr::String(err_message) = compilation_error_arg else {
                    return PluginResult::diagnostic_only(PluginDiagnostic::error(
                        &compilation_error_arg,
                        "`compile_error!` argument must be an unnamed string argument.".to_string(),
                    ));
                };
                return PluginResult::diagnostic_only(PluginDiagnostic::error(
                    &inline_macro_ast,
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
