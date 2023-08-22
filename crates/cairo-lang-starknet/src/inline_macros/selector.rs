use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_semantic::inline_macros::{
    extract_single_unnamed_arg, unsupported_bracket_diagnostic,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

use crate::contract::starknet_keccak;

/// Macro for expanding a selector to a string literal.
#[derive(Debug, Default)]
pub struct SelectorMacro;
impl SelectorMacro {
    pub const NAME: &'static str = "selector";
}
impl InlineMacroExprPlugin for SelectorMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let ast::WrappedArgList::ParenthesizedArgList(args) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };

        let Some(arg) = extract_single_unnamed_arg(db, args.args(db)) else {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: syntax.stable_ptr().untyped(),
                message: format!(
                    "`{}` macro must have exactly one unnamed argument.",
                    SelectorMacro::NAME
                ),
            }];
            return InlinePluginResult { code: None, diagnostics };
        };

        let ast::Expr::String(input_string) = arg else {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: syntax.stable_ptr().untyped(),
                message: format!("`{}` macro argument must be a string", SelectorMacro::NAME),
            }];
            return InlinePluginResult { code: None, diagnostics };
        };
        let selector_string = input_string.string_value(db).unwrap();

        let selector = starknet_keccak(selector_string.as_bytes());
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "selector_inline_macro".into(),
                content: format!("0x{}", selector.to_str_radix(16)),
                diagnostics_mappings: vec![],
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
