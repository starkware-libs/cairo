use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{try_extract_unnamed_arg, unsupported_bracket_diagnostic};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::ast::{Arg, WrappedArgList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use indoc::{formatdoc, indoc};
use num_bigint::BigUint;

use super::write::FELT252_BYTES;

/// Try to generate a simple panic handlic code.
/// Return true if successful and updates the builder if successful.
fn try_handle_simple_panic(
    db: &dyn SyntaxGroup,
    builder: &mut PatchBuilder<'_>,
    arguments: &[Arg],
) -> bool {
    let format_string_expr = match arguments {
        [] => {
            // Trivial panic!() with no arguments case.
            builder.add_str(
                "core::panics::panic(array![core::byte_array::BYTE_ARRAY_MAGIC, 0, 0, 0])",
            );
            return true;
        }
        [arg] => {
            let Some(ast::Expr::String(format_string_expr)) = try_extract_unnamed_arg(db, arg)
            else {
                return false;
            };
            format_string_expr
        }
        // We have more than one argument, fallback to more generic handling.
        _ => return false,
    };

    let Some(format_str) = format_string_expr.string_value(db) else {
        return false;
    };

    if format_str.find(['{', '}']).is_some() {
        return false;
    }

    builder.add_str(&format!(
        "core::panics::panic(array![core::byte_array::BYTE_ARRAY_MAGIC, {}, ",
        format_str.len() / FELT252_BYTES,
    ));

    for chunk in format_str.as_bytes().chunks(FELT252_BYTES) {
        builder.add_str(&format!("{:#x}, ", BigUint::from_bytes_be(chunk)));
    }

    let remainder_size = format_str.len() % FELT252_BYTES;
    if remainder_size == 0 {
        // Adding the empty remainder word.
        builder.add_str("0, ");
    }
    builder.add_str(&format!("{remainder_size}])"));

    true
}

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
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let WrappedArgList::ParenthesizedArgList(arguments_syntax) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };

        let mut builder = PatchBuilder::new(db, syntax);
        let arguments = arguments_syntax.arguments(db).elements(db);
        if !try_handle_simple_panic(db, &mut builder, &arguments) {
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
                        RewriteNode::from_ast_trimmed(&arguments_syntax.lparen(db)),
                    ),
                    (
                        "rparen".to_string(),
                        RewriteNode::from_ast_trimmed(&arguments_syntax.rparen(db)),
                    ),
                    (
                        "args".to_string(),
                        RewriteNode::interspersed(
                            arguments.iter().map(RewriteNode::from_ast_trimmed),
                            RewriteNode::text(", "),
                        ),
                    ),
                ]
                .into(),
            ));
        }
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
            Terminates the program immediately with an error message.
            The `panic!` macro halts execution when an unrecoverable error \
            occurs. It prints an error message and exits the program. \
            Accepts a format string and arguments, similar to `format!`, \
            for detailed error messages.

            # Syntax
            ```cairo
            panic!();
            panic!("error message");
            panic!("formatted error: {}", value);
            ```
            # Behavior
            - Without arguments, panics with a default message.
            - With a message or formatted string, panics with that message.
            - Constructs the panic message at runtime using the format string and arguments.

            # Examples
            ```cairo
            panic!(); // Panics with a default message.
            panic!("An unexpected error occurred."); // Panics with the provided message.
            let x = 10;
            let y = 20;
            if x + y != 30 {
                panic!("Math is broken: {} + {} != 30", x, y);
                // Panics with "Math is broken: 10 + 20 != 30".
            }
            let x = -1;
            assert!(x >= 0, "Invalid value: x = {}", x);
            assert!(x >= 0, "Invalid value: x = {x}");
            // Panics with "Invalid value: x = -1."
            ```

            # Notes
            - Use `panic!` only for unrecoverable errors.
            - In library code, prefer returning `Result` or `Option` to let callers handle errors.
            - Avoid using `panic!` for control flow or expected error conditions.
            "#}
            .to_string(),
        )
    }
}
