use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::unsupported_bracket_diagnostic;
use cairo_lang_syntax::node::ast::WrappedArgList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

/// Macro for panicking with a format string.
#[derive(Default, Debug)]
pub struct PanicMacro;
impl NamedPlugin for PanicMacro {
    const NAME: &'static str = "panic";
}
impl InlineMacroExprPlugin for PanicMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let WrappedArgList::ParenthesizedArgList(arguments_syntax) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };
        let mut builder = PatchBuilder::new(db);
        let arguments = arguments_syntax.arguments(db).elements(db);
        if arguments.is_empty() {
            builder.add_str(r#"core::panics::panic_with_byte_array(@"")"#);
        } else {
            builder.add_modified(RewriteNode::interpolate_patched(
                &formatdoc! {
                    r#"
                        {{
                            let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                            core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                                write!$lparen${f}, $args$$rparen$
                            );
                            core::panics::panic_with_byte_array(@{f}.buffer)
                        }}
                    "#,
                    f = "__formatter_for_panic_macro__",
                },
                &[
                    (
                        "lparen".to_string(),
                        RewriteNode::new_trimmed(arguments_syntax.lparen(db).as_syntax_node()),
                    ),
                    (
                        "rparen".to_string(),
                        RewriteNode::new_trimmed(arguments_syntax.rparen(db).as_syntax_node()),
                    ),
                    (
                        "args".to_string(),
                        RewriteNode::interspersed(
                            arguments
                                .iter()
                                .map(|arg| RewriteNode::new_trimmed(arg.as_syntax_node())),
                            RewriteNode::text(", "),
                        ),
                    ),
                ]
                .into(),
            ));
        }
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: format!("{}_macro", Self::NAME).into(),
                content: builder.code,
                code_mappings: builder.code_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
