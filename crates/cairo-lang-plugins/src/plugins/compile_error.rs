use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use salsa::Database;

/// Plugin that allows writing item level `compile_error!` causing a diagnostic.
/// Useful for testing that `cfg` attributes are valid.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct CompileErrorPlugin;

impl MacroPlugin for CompileErrorPlugin {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        item_ast: ast::ModuleItem<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult<'db> {
        let ast::ModuleItem::InlineMacro(inline_macro_ast) = item_ast else {
            return Default::default();
        };
        if inline_macro_ast.path(db).as_syntax_node().get_text_without_trivia(db).long(db)
            != "compile_error"
        {
            return Default::default();
        }
        let item_ast_ptr = inline_macro_ast.stable_ptr(db).untyped();
        let Some(legacy_inline_macro_ast) = inline_macro_ast.as_legacy_inline_macro(db) else {
            return PluginResult::diagnostic_only(not_legacy_macro_diagnostic(item_ast_ptr));
        };
        let compilation_error_arg = extract_macro_single_unnamed_arg!(
            db,
            &legacy_inline_macro_ast,
            ast::WrappedArgList::ParenthesizedArgList(_),
            item_ast_ptr
        );
        PluginResult::diagnostic_only(
            if let ast::Expr::String(err_message) = compilation_error_arg {
                PluginDiagnostic::error(item_ast_ptr, err_message.text(db).to_string(db))
            } else {
                PluginDiagnostic::error_with_inner_span(
                    db,
                    item_ast_ptr,
                    compilation_error_arg.as_syntax_node(),
                    "`compile_error!` argument must be an unnamed string argument.".to_string(),
                )
            },
        )
    }

    fn declared_attributes<'db>(&self, _db: &'db dyn Database) -> Vec<SmolStrId<'db>> {
        vec![]
    }
}
