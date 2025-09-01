use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, not_legacy_macro_diagnostic};
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::{formatdoc, indoc};
use salsa::Database;

use super::write::{WriteMacro, WritelnMacro};

/// Macro for printing.
#[derive(Debug, Default)]
pub struct PrintMacro;
impl NamedPlugin for PrintMacro {
    const NAME: &'static str = "print";
}
impl InlineMacroExprPlugin for PrintMacro {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        syntax: &ast::ExprInlineMacro<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult<'db> {
        generate_code_inner(syntax, db, false)
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Prints to the standard output.
            Equivalent to the `println!` macro except that a newline is not printed at the end of \
            the message.

            # Panics
            Panics if any of the formatting of arguments fails.

            # Examples
            ```cairo
            println!(\"hello\"); // Prints "hello".
            let world: ByteArray = "world";
            println!("hello {}", world_ba); // Prints "hello world".
            println!("hello {world_ba}"); // Prints "hello world".
            ```
        "#}
            .to_string(),
        )
    }
}

/// Macro for printing with a new line.
#[derive(Debug, Default)]
pub struct PrintlnMacro;
impl NamedPlugin for PrintlnMacro {
    const NAME: &'static str = "println";
}
impl InlineMacroExprPlugin for PrintlnMacro {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        syntax: &ast::ExprInlineMacro<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult<'db> {
        generate_code_inner(syntax, db, true)
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Prints to the standard output, with a newline.
            This macro uses the same syntax as `format!`, but writes to the standard output instead.

            # Panics
            Panics if any of the formatting of arguments fails.

            # Examples
            ```cairo
            println!(); // Prints an empty line.
            println!(\"hello\"); // Prints "hello".
            let world: ByteArray = "world";
            println!("hello {}", world_ba); // Prints "hello world".
            println!("hello {world_ba}"); // Prints "hello world".
            ```
        "#}
            .to_string(),
        )
    }
}

fn generate_code_inner<'db>(
    syntax: &ast::ExprInlineMacro<'db>,
    db: &'db dyn Database,
    with_newline: bool,
) -> InlinePluginResult<'db> {
    let Some(syntax) = syntax.as_legacy_inline_macro(db) else {
        return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
            syntax.as_syntax_node().stable_ptr(db),
        ));
    };
    let arguments = syntax.arguments(db);
    let mut builder = PatchBuilder::new(db, &syntax);
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc! {
            "
                {{
                    let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                    core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                        {write_func}!$left_bracket${f}, $args$$right_bracket$
                    );
                    core::debug::print_byte_array_as_string(@{f}.buffer);
                }}
            ",
            f = "__formatter_for_print_macros__",
            write_func = if with_newline { WritelnMacro::NAME } else { WriteMacro::NAME },
        },
        &[
            (
                "left_bracket".to_string(),
                RewriteNode::new_trimmed(arguments.left_bracket_syntax_node(db)),
            ),
            (
                "right_bracket".to_string(),
                RewriteNode::new_trimmed(arguments.right_bracket_syntax_node(db)),
            ),
            (
                "args".to_string(),
                arguments
                    .arg_list(db)
                    .as_ref()
                    .map_or_else(RewriteNode::empty, RewriteNode::from_ast_trimmed),
            ),
        ]
        .into(),
    ));
    let (content, code_mappings) = builder.build();
    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: format!("{}_macro", get_macro_name(with_newline)),
            content,
            code_mappings,
            aux_data: None,
            diagnostics_note: Default::default(),
            is_unhygienic: false,
        }),
        diagnostics: vec![],
    }
}

/// Gets the macro name according to the `with_newline` flag.
fn get_macro_name(with_newline: bool) -> &'static str {
    if with_newline { PrintlnMacro::NAME } else { PrintMacro::NAME }
}
