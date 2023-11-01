use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
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

        let arguments = macro_args.elements(db).iter().map(|arg| arg.arg_clause(db)).collect_vec();
        let mut builder = PatchBuilder::new(db);
        match arguments.as_slice() {
            [] => {
                builder.add_modified(RewriteNode::text(
                    r#"{
                        let ba: ByteArray = "";
                        panics::panic_with_byte_array(@ba);
                    }"#,
                ));
            }
            [ast::ArgClause::Unnamed(arg)] => {
                builder.add_modified(RewriteNode::interpolate_patched(
                    "{
                        panics::panic_with_byte_array(@$arg$);
                    }",
                    &[("arg".to_string(), RewriteNode::new_trimmed(arg.as_syntax_node()))].into(),
                ));
            }
            [ast::ArgClause::Unnamed(format_string), ast::ArgClause::Unnamed(arg)] => {
                builder.add_modified(RewriteNode::interpolate_patched(
                    "{
                        panics::panic_with_byte_array(@format!($format_string$, $arg$));
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
            }
            _ => {
                let diagnostics = vec![PluginDiagnostic {
                    stable_ptr: syntax.stable_ptr().untyped(),
                    message: format!(
                        "`{}` macro must have up to 2 unnamed arguments.",
                        PanicMacro::NAME
                    ),
                }];
                return InlinePluginResult { code: None, diagnostics };
            }
        }

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
