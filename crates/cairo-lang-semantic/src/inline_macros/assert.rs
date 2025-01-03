use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{
    escape_node, try_extract_unnamed_arg, unsupported_bracket_diagnostic,
};
use cairo_lang_syntax::node::ast::WrappedArgList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use indoc::{formatdoc, indoc};

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
        _metadata: &MacroPluginMetadata<'_>,
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
        let mut builder = PatchBuilder::new(db, syntax);
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                r#"
                    if !($value$) {{
                        let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                "#,
            },
            &[("value".to_string(), RewriteNode::from_ast_trimmed(&value))].into(),
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
                        RewriteNode::from_ast_trimmed(&arguments_syntax.lparen(db)),
                    ),
                    (
                        "rparen".to_string(),
                        RewriteNode::from_ast_trimmed(&arguments_syntax.rparen(db)),
                    ),
                    (
                        "args".to_string(),
                        RewriteNode::interspersed(
                            format_args.iter().map(RewriteNode::from_ast_trimmed),
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
        let (content, code_mappings) = builder.build();
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: format!("{}_macro", Self::NAME).into(),
                content,
                code_mappings,
                aux_data: None,
                diagnostics_note: Default::default(),
            }),
            diagnostics: vec![],
        }
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Asserts that a condition is true at runtime.
            The `assert!` macro checks a boolean expression; if it evaluates to `false`, \
            it panics with an optional custom error message. Useful for debugging and \
            ensuring conditions hold during execution.

            # Syntax
            ```cairo
            assert!(condition);
            assert!(condition, "error message");
            assert!(condition, "formatted error: {}", value);
            ```
            # Parameters
            - `condition`: A boolean expression to evaluate.
            - `format_string` (optional): A string literal for format placeholders.
            - `args` (optional): Values for placeholders in `format_string`.

            # Examples
            ```cairo
            assert!(2 + 2 == 4); // Passes, does nothing.
            assert!(2 + 2 == 5); // Panics with "assertion failed: `2 + 2 == 5`."
            let age = 18;
            assert!(age >= 21, "Age must be at least 21, found {}", age);
            // Panics with "Age must be at least 21, found 18."
            let x = -1;
            assert!(x >= 0, "Invalid value: x = {}", x);
            assert!(x >= 0, "Invalid value: x = {x}");
            // Panics with "Invalid value: x = -1."
            ```
            # Notes
            - Use to catch programming errors and enforce invariants.
            - May impact performance; consider `debug_assert!` for debug-only checks.
            - For recoverable errors, prefer using `Result` or `Option` instead of panicking.
            "#}
            .to_string(),
        )
    }
}
