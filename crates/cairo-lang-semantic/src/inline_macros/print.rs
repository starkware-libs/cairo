use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

use crate::extract_macro_unnamed_args;

/// Macro for printing.
#[derive(Debug, Default)]
pub struct PrintMacro;
impl PrintMacro {
    pub const NAME: &'static str = "print";
}
impl InlineMacroExprPlugin for PrintMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let [format_string, arg] = extract_macro_unnamed_args!(
            db,
            syntax,
            2,
            ast::WrappedArgList::ParenthesizedArgList(_)
        );

        if format_string.as_syntax_node().get_text(db) != r#""{}""# {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: syntax.stable_ptr().untyped(),
                message: format!(
                    r#"Currently, the `{}` macro only supports `"{{}}"` as the first argument (format string)."#,
                    PrintMacro::NAME
                ),
            }];
            return InlinePluginResult { code: None, diagnostics };
        }

        let mut builder = PatchBuilder::new(db);
        builder.add_modified(RewriteNode::interpolate_patched(
            "{
                debug::print_byte_array_as_string(@format!($format_string$, $arg$));
            }",
            &[
                (
                    "format_string".to_string(),
                    RewriteNode::new_trimmed(format_string.as_syntax_node()),
                ),
                ("arg".to_string(), RewriteNode::new_trimmed(arg.as_syntax_node())),
            ]
            .into(),
        ));

        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "print_macro".into(),
                content: builder.code,
                diagnostics_mappings: builder.diagnostics_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
