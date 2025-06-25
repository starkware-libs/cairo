use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{
    PluginResultTrait, escape_node, not_legacy_macro_diagnostic, try_extract_unnamed_arg,
    unsupported_bracket_diagnostic,
};
use cairo_lang_filesystem::cfg::Cfg;
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::ast::WrappedArgList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::formatdoc;

/// The type of value the comparison function expects to find.
enum ArgType {
    /// The type is a snapshot of the compared value.
    Snapshot,
    /// The type is the actual value.
    Value,
}

/// A trait for compare assertion plugin.
trait CompareAssertionPlugin: NamedPlugin {
    /// The operator for the panic message.
    const OPERATOR: &'static str;
    /// The actual comparison function.
    const FUNCTION: &'static str;
    /// The argument type for the comparison function.
    const ARG_TYPE: ArgType;

    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let Some(legacy_inline_macro) = syntax.as_legacy_inline_macro(db) else {
            return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
                syntax.as_syntax_node().stable_ptr(db),
            ));
        };
        let WrappedArgList::ParenthesizedArgList(arguments_syntax) =
            legacy_inline_macro.arguments(db)
        else {
            return unsupported_bracket_diagnostic(db, &legacy_inline_macro, syntax.stable_ptr(db));
        };
        let arguments = arguments_syntax.arguments(db).elements_vec(db);
        if arguments.len() < 2 {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error(
                    arguments_syntax.lparen(db).stable_ptr(db),
                    format!("Macro `{}` requires at least 2 arguments.", Self::NAME),
                )],
            };
        }
        let (lhs, rest) = arguments.split_first().unwrap();
        let (rhs, format_args) = rest.split_first().unwrap();
        let Some(lhs) = try_extract_unnamed_arg(db, lhs) else {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error_with_inner_span(
                    db,
                    syntax.stable_ptr(db),
                    lhs.as_syntax_node(),
                    format!("Macro `{}` requires the first argument to be unnamed.", Self::NAME),
                )],
            };
        };
        let Some(rhs) = try_extract_unnamed_arg(db, rhs) else {
            return InlinePluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic::error_with_inner_span(
                    db,
                    syntax.stable_ptr(db),
                    rhs.as_syntax_node(),
                    format!("Macro `{}` requires the second argument to be unnamed.", Self::NAME),
                )],
            };
        };
        let f = format!("__formatter_for_{}_macro_", Self::NAME);
        let lhs_escaped = escape_node(db, lhs.as_syntax_node());
        let rhs_escaped = escape_node(db, rhs.as_syntax_node());
        let (wrap_start, wrap_end) = match Self::ARG_TYPE {
            ArgType::Snapshot => ("@(", ")"),
            ArgType::Value => ("", ""),
        };
        let mut builder = PatchBuilder::new(db, syntax);
        let lhs_value =
            RewriteNode::mapped_text(format!("__lhs_value_for_{}_macro__", Self::NAME), db, &lhs);
        let rhs_value =
            RewriteNode::mapped_text(format!("__rhs_value_for_{}_macro__", Self::NAME), db, &rhs);
        let operator = Self::OPERATOR;
        let function = Self::FUNCTION;
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                r#"
                {{
                    let $lhs_value$ = {wrap_start}$lhs${wrap_end};
                    let $rhs_value$ = {wrap_start}$rhs${wrap_end};
                    if !{function}($lhs_value$, $rhs_value$) {{
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
        let fmt_arg = match Self::ARG_TYPE {
            ArgType::Snapshot => "",
            ArgType::Value => "@",
        };

        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                r#"
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        write!({f}, "{lhs_escaped}: ")
                    );
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        core::fmt::Debug::fmt({fmt_arg}$lhs_value$, ref {f})
                    );
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        write!({f}, "\n{rhs_escaped}: ")
                    );
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        core::fmt::Debug::fmt({fmt_arg}$rhs_value$, ref {f})
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
                syntax.stable_ptr(db),
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
    ($(#[$attr:meta])* $ident:ident, $name:literal, $operator:literal, $function:literal, $arg_type:path) => {
        $(#[$attr])*
        #[derive(Default, Debug)]
        pub struct $ident;
        impl NamedPlugin for $ident {
            const NAME: &'static str = $name;
        }

        impl CompareAssertionPlugin for $ident {
            const OPERATOR: &'static str = $operator;
            const FUNCTION: &'static str = $function;
            const ARG_TYPE: ArgType = $arg_type;
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
    "==",
    "core::traits::PartialEq::eq",
    ArgType::Snapshot
);

define_compare_assert_macro!(
    /// Macro for not equality assertion.
    AssertNeMacro,
    "assert_ne",
    "!=",
    "core::traits::PartialEq::ne",
    ArgType::Snapshot
);

define_compare_assert_macro!(
    /// Macro for less-than assertion.
    AssertLtMacro,
    "assert_lt",
    "<",
    "core::traits::PartialOrd::lt",
    ArgType::Value
);

define_compare_assert_macro!(
    /// Macro for less-than-or-equal assertion.
    AssertLeMacro,
    "assert_le",
    "<=",
    "core::traits::PartialOrd::le",
    ArgType::Value
);

define_compare_assert_macro!(
    /// Macro for greater-than assertion.
    AssertGtMacro,
    "assert_gt",
    ">",
    "core::traits::PartialOrd::gt",
    ArgType::Value
);

define_compare_assert_macro!(
    /// Macro for greater-than-or-equal assertion.
    AssertGeMacro,
    "assert_ge",
    ">=",
    "core::traits::PartialOrd::ge",
    ArgType::Value
);
