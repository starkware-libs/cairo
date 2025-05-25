use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_starknet_classes::keccak::starknet_keccak;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};

/// Macro for expanding a selector to a string literal.
#[derive(Debug, Default)]
pub struct SelectorMacro;
impl NamedPlugin for SelectorMacro {
    const NAME: &'static str = "selector";
}
impl InlineMacroExprPlugin for SelectorMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let Some(legacy_inline_macro) = syntax.as_legacy_inline_macro(db) else {
            return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
                syntax.as_syntax_node().stable_ptr(db),
            ));
        };
        let arg = extract_macro_single_unnamed_arg!(
            db,
            &legacy_inline_macro,
            ast::WrappedArgList::ParenthesizedArgList(_),
            syntax.stable_ptr(db)
        );

        let ast::Expr::String(input_string) = arg else {
            let diagnostics = vec![PluginDiagnostic::error(
                syntax.stable_ptr(db).untyped(),
                format!("`{}` macro argument must be a string", SelectorMacro::NAME),
            )];
            return InlinePluginResult { code: None, diagnostics };
        };
        let selector_string = input_string.string_value(db).unwrap();

        let selector = starknet_keccak(selector_string.as_bytes());
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "selector_inline_macro".into(),
                content: format!("0x{}", selector.to_str_radix(16)),
                code_mappings: vec![],
                aux_data: None,
                diagnostics_note: Default::default(),
            }),
            diagnostics: vec![],
        }
    }
}
