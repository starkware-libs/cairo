use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{
    escape_node, try_extract_unnamed_arg, unsupported_bracket_diagnostic,
};
use cairo_lang_filesystem::cfg::Cfg;
use cairo_lang_syntax::node::ast::WrappedArgList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::formatdoc;

/// A trait for compare assertion plugin.
trait CompareAssertionPlugin: NamedPlugin {
    const OPERATOR: &'static str;

    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let WrappedArgList::ParenthesizedArgList(arguments_syntax) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };
        let arguments = arguments_syntax.arguments(db).elements(db);
        if arguments.len() < 2 {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    &arguments_syntax.lparen(db),
                    format!("Macro `{}` requires at least 2 arguments.", Self::NAME),
                )],
            };
        }
        let (lhs, rest) = arguments.split_first().unwrap();
        let (rhs, format_args) = rest.split_first().unwrap();
        let Some(lhs) = try_extract_unnamed_arg(db, lhs) else {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    lhs,
                    format!("Macro `{}` requires the first argument to be unnamed.", Self::NAME),
                )],
            };
        };
        let Some(rhs) = try_extract_unnamed_arg(db, rhs) else {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    rhs,
                    format!("Macro `{}` requires the second argument to be unnamed.", Self::NAME),
                )],
            };
        };
        let f = format!("__formatter_for_{}_macro_", Self::NAME);
        let lhs_escaped = escape_node(db, lhs.as_syntax_node());
        let rhs_escaped = escape_node(db, rhs.as_syntax_node());
        let mut builder = PatchBuilder::new(db, syntax);
        let lhs_value =
            RewriteNode::mapped_text(format!("__lhs_value_for_{}_macro__", Self::NAME), db, &lhs);
        let rhs_value =
            RewriteNode::mapped_text(format!("__rhs_value_for_{}_macro__", Self::NAME), db, &rhs);
        let operator = Self::OPERATOR;
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                r#"
                {{
                    let $lhs_value$ = $lhs$;
                    let $rhs_value$ = $rhs$;
                    if !($lhs_value$ {operator} $rhs_value$) {{
                        let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                        core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                            write!({f}, "assertion `{lhs_escaped} {operator} {rhs_escaped}` failed")
                        );
            "#,
            },
            &[
                ("lhs".to_string(), RewriteNode::from_ast_trimmed(&lhs)),
                ("rhs".to_string(), RewriteNode::from_ast_trimmed(&rhs)),
                ("lhs_value".to_string(), lhs_value.clone()),
                ("rhs_value".to_string(), rhs_value.clone()),
            ]
            .into(),
        ));
        if format_args.is_empty() {
            builder.add_str(&formatdoc!(
                "core::result::ResultTrait::<(), core::fmt::Error>::unwrap(writeln!({f}, \
                 \".\"));\n",
            ));
        } else {
            builder.add_modified(RewriteNode::interpolate_patched(
                &formatdoc! {
                    r#"
                        core::result::ResultTrait::<(), core::fmt::Error>::unwrap(write!({f}, ": "));
                        core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                            writeln!$lparen${f}, $args$$rparen$
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
                            format_args
                                .iter()
                                .map(RewriteNode::from_ast_trimmed),
                            RewriteNode::text(", "),
                        ),
                    ),
                ]
                .into(),
            ));
        }

        #[expect(clippy::literal_string_with_formatting_args)]
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                r#"
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        writeln!({f}, "{lhs_escaped}: {{:?}}", $lhs_value$)
                    );
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        write!({f}, "{rhs_escaped}: {{:?}}", $rhs_value$)
                    );
                "#,
            },
            &[("lhs_value".to_string(), lhs_value), ("rhs_value".to_string(), rhs_value)].into(),
        ));
        builder.add_str(&formatdoc! {
            "
                        core::panics::panic_with_byte_array(@{f}.buffer)
                    }}
                }}
            ",
        });
        let (content, code_mappings) = builder.build();
        let mut diagnostics = vec![];
        if !metadata.cfg_set.contains(&Cfg::kv("target", "test"))
            && !metadata.cfg_set.contains(&Cfg::name("test"))
        {
            diagnostics.push(PluginDiagnostic::error(
                syntax,
                format!("`{}` macro is only available in test mode.", Self::NAME),
            ));
        }
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: format!("{}_macro", Self::NAME).into(),
                content,
                code_mappings,
                aux_data: None,
                diagnostics_note: Default::default(),
            }),
            diagnostics,
        }
    }
}

macro_rules! define_compare_assert_macro {
    ($(#[$attr:meta])* $ident:ident, $name:tt, $operator:tt) => {
        $(#[$attr])*
        #[derive(Default, Debug)]
        pub struct $ident;
        impl NamedPlugin for $ident {
            const NAME: &'static str = $name;
        }

        impl CompareAssertionPlugin for $ident {
            const OPERATOR: &'static str = $operator;
        }

        impl InlineMacroExprPlugin for $ident {
            fn generate_code(
                &self,
                db: &dyn SyntaxGroup,
                syntax: &ast::ExprInlineMacro,
                metadata: &MacroPluginMetadata<'_>,
            ) -> InlinePluginResult {
                CompareAssertionPlugin::generate_code(self, db, syntax, metadata)
            }
        }
    };
}

define_compare_assert_macro!(
    /// Macro for equality assertion.
    AssertEqMacro,
    "assert_eq",
    "=="
);

define_compare_assert_macro!(
    /// Macro for not equality assertion.
    AssertNeMacro,
    "assert_ne",
    "!="
);

define_compare_assert_macro!(
    /// Macro for less-than assertion.
    AssertLtMacro,
    "assert_lt",
    "<"
);

define_compare_assert_macro!(
    /// Macro for less-than-or-equal assertion.
    AssertLeMacro,
    "assert_le",
    "<="
);

define_compare_assert_macro!(
    /// Macro for greater-than assertion.
    AssertGtMacro,
    "assert_gt",
    ">"
);

define_compare_assert_macro!(
    /// Macro for greater-than-or-equal assertion.
    AssertGeMacro,
    "assert_ge",
    ">="
);
