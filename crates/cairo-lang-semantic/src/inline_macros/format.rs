use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use indoc::{formatdoc, indoc};

/// Macro for formatting.
#[derive(Default, Debug)]
pub struct FormatMacro;
impl NamedPlugin for FormatMacro {
    const NAME: &'static str = "format";
}
impl InlineMacroExprPlugin for FormatMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let arguments = syntax.arguments(db);
        let mut builder = PatchBuilder::new(db, syntax);
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                "
                    {{
                        let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                        core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                            write!$left_bracket${f}, $args$$right_bracket$
                        );
                        {f}.buffer
                    }}
                ",
                f = "__formatter_for_format_macro__",
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
            Creates a ByteArray using interpolation of runtime expressions.

            The first argument `format!` receives is a format string. \
            This must be a string literal. \
            The power of the formatting string is in the `{}`s contained. Additional parameters \
            passed to `format!` replace the `{}`s within the formatting string in the order given \
            unless named or positional parameters are used.

            A common use for `format!` is concatenation and interpolation of strings. The same convention is used with `print!` and `write!` macros, depending on the intended destination of the ByteArray.

            # Panics
            Panics if any of the formatting of arguments fails.

            # Examples
            ```cairo
            format!("hello"); // => "hello".
            let world: ByteArray = "world";
            format!("hello {}", world_ba); // => "hello world".
            format!("hello {world_ba}"); // => "hello world".
            let (x, y) = (1, 2);
            format!("{x} + {y} = 3"); // => "1 + 2 = 3"
            ```
        "#}
            .to_string(),
        )
    }
}
