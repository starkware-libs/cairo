use cairo_lang_defs::extract_macro_single_unnamed_arg;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_starknet_classes::keccak::starknet_keccak;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};

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
    ) -> InlinePluginResult {
        let arg = extract_macro_single_unnamed_arg!(
            db,
            syntax,
            ast::WrappedArgList::ParenthesizedArgList(_)
        );

        let ast::Expr::String(input_string) = arg else {
            let diagnostics = vec![PluginDiagnostic::error(
                syntax.stable_ptr().untyped(),
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
            }),
            diagnostics: vec![],
        }
    }
}
