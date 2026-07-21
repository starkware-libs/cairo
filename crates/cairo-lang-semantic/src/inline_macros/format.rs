use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{PluginResultTrait, extract_parenthesized_macro};
use cairo_lang_syntax::node::ast;
use indoc::{formatdoc, indoc};
use salsa::Database;

/// Macro for formatting.
#[derive(Default, Debug)]
pub struct FormatMacro;
impl NamedPlugin for FormatMacro {
    const NAME: &'static str = "format";
}
impl InlineMacroExprPlugin for FormatMacro {
    fn generate_code<'db>(
        &self,
        db: &'db dyn Database,
        syntax: &ast::ExprInlineMacro<'db>,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult<'db> {
        let args = match extract_parenthesized_macro(db, syntax) {
            Ok(args) => args,
            Err(diag) => return InlinePluginResult::diagnostic_only(diag),
        };
        let mut builder = PatchBuilder::new(db, syntax);
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc! {
                "
                    {{
                        let mut {f}: core::fmt::Formatter = core::traits::Default::default();
                        core::result::ResultTrait::<(), core::fmt::Error>::unwrap(
                            write!({f}, $args$)
                        );
                        {f}.buffer
                    }}
                ",
                f = "__formatter_for_format_macro__",
            },
            &[("args".to_string(), RewriteNode::from_ast_trimmed(&args.arguments(db)))].into(),
        ));
        let (content, code_mappings) = builder.build();
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: format!("{}_macro", Self::NAME),
                content,
                code_mappings,
                aux_data: None,
                diagnostics_note: Default::default(),
                is_unhygienic: false,
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
