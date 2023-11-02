use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;

use super::unsupported_bracket_diagnostic;

/// Macro for panicking.
#[derive(Debug, Default)]
pub struct PanicMacro;
impl PanicMacro {
    pub const NAME: &'static str = "panic";
}
impl InlineMacroExprPlugin for PanicMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let macro_args =
            if let ast::WrappedArgList::ParenthesizedArgList(args) = syntax.arguments(db) {
                args.arguments(db)
            } else {
                return unsupported_bracket_diagnostic(db, syntax);
            };

        if macro_args.elements(db).is_empty() {
            let mut builder = PatchBuilder::new(db);
            builder.add_modified(RewriteNode::text(
                r#"{
                    panics::panic_with_byte_array(@"");
                }"#,
            ));
            return InlinePluginResult {
                code: Some(PluginGeneratedFile {
                    name: "panic_macro".into(),
                    content: builder.code,
                    diagnostics_mappings: builder.diagnostics_mappings,
                    aux_data: None,
                }),
                diagnostics: vec![],
            };
        }

        let arguments = macro_args.elements(db).iter().map(|arg| arg.arg_clause(db)).collect_vec();
        let Some(arguments) = arguments
            .into_iter()
            .map(|arg| {
                Some(RewriteNode::new_trimmed(
                    try_extract_matches!(arg, ast::ArgClause::Unnamed)?.as_syntax_node(),
                ))
            })
            .collect::<Option<Vec<_>>>()
        else {
            let diagnostics = vec![PluginDiagnostic {
                stable_ptr: syntax.stable_ptr().untyped(),
                message: format!(
                    "All arguments of the `{}` macro must be unnamed.",
                    PanicMacro::NAME
                ),
            }];
            return InlinePluginResult { code: None, diagnostics };
        };
        let format_args = RewriteNode::interspersed(arguments, RewriteNode::text(", "));

        let mut builder = PatchBuilder::new(db);
        builder.add_modified(RewriteNode::interpolate_patched(
            "{
                panics::panic_with_byte_array(@format!($format_args$));
            }",
            &[("format_args".to_string(), format_args)].into(),
        ));

        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "panic_macro".into(),
                content: builder.code,
                diagnostics_mappings: builder.diagnostics_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
