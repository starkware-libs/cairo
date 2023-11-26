use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;

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
    ) -> InlinePluginResult {
        let arguments = syntax.arguments(db);
        let mut builder = PatchBuilder::new(db);
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
                    RewriteNode::new_trimmed(arguments.arg_list(db).unwrap().as_syntax_node()),
                ),
            ]
            .into(),
        ));
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
