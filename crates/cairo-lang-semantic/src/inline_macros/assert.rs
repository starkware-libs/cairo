use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{
    escape_node, try_extract_unnamed_arg, unsupported_bracket_diagnostic,
};
use cairo_lang_syntax::node::ast::WrappedArgList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use indoc::formatdoc;

/// Macro for assertion.
#[derive(Default, Debug)]
pub struct AssertMacro;
impl NamedPlugin for AssertMacro {
    const NAME: &'static str = "assert";
}
impl InlineMacroExprPlugin for AssertMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let WrappedArgList::ParenthesizedArgList(arguments_syntax) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };
        let arguments = arguments_syntax.arguments(db).elements(db);
        let Some((value, format_args)) = arguments.split_first() else {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    arguments_syntax.lparen(db).stable_ptr().untyped(),
                    format!("Macro `{}` requires at least 1 argument.", Self::NAME),
                )],
            };
        };
        let Some(value) = try_extract_unnamed_arg(db, value) else {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    value.stable_ptr().untyped(),
                    format!("Macro `{}` requires the first argument to be unnamed.", Self::NAME),
                )],
            };
        };
        let f = "__formatter_for_assert_macro__";
        let value_escaped = escape_node(db, value.as_syntax_node());
        let mut builder = PatchBuilder::new(db);
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                r#"
                    if !($value$) {{
                        let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                "#,
            },
            &[("value".to_string(), RewriteNode::new_trimmed(value.as_syntax_node()))].into(),
        ));
        if format_args.is_empty() {
            builder.add_str(&formatdoc!(
                "core::result::ResultTrait::<(), core::fmt::Error>::unwrap(write!({f}, \
                 \"assertion failed: `{value_escaped}`.\"));\n",
            ));
        } else {
            builder.add_modified(RewriteNode::interpolate_patched(
                &formatdoc! {
                    r#"
                        core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                            write!$lparen${f}, $args$$rparen$
                        );
                    "#,
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
                            format_args
                                .iter()
                                .map(|arg| RewriteNode::new_trimmed(arg.as_syntax_node())),
                            RewriteNode::text(", "),
                        ),
                    ),
                ]
                .into(),
            ));
        }
        builder.add_str(&formatdoc! {
            "
                    core::panics::panic_with_byte_array(@{f}.buffer)
                }}
            ",
        });
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
