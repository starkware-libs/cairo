use cairo_lang_defs::plugin::{InlinePluginResult, PluginGeneratedFile};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_macro::TokenStream;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use proc_macro_server_api::methods::ProcMacroResult;

use super::{FromSyntaxNode, into_cairo_diagnostics};

// scarb code

pub(super) fn inline_macro_generate_code(
    db: &dyn SyntaxGroup,
    syntax: &ast::ExprInlineMacro,
    cb: impl FnOnce(&dyn SyntaxGroup, TokenStream) -> ProcMacroResult,
) -> InlinePluginResult {
    let origin = CodeOrigin::Span(syntax.as_syntax_node().span(db));
    let stable_ptr = syntax.clone().stable_ptr().untyped();
    let token_stream = TokenStream::from_syntax_node(db, syntax);
    // modified
    let result = cb(db, token_stream);
    // end modified
    // Handle diagnostics.
    let diagnostics = into_cairo_diagnostics(result.diagnostics, stable_ptr);
    let token_stream = result.token_stream.clone();
    if token_stream.is_empty() {
        // Remove original code
        InlinePluginResult { code: None, diagnostics }
    } else {
        let content = token_stream.to_string();
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "inline_proc_macro".into(),
                code_mappings: vec![CodeMapping {
                    origin,
                    span: TextSpan {
                        start: TextOffset::default(),
                        end: TextOffset::default().add_width(TextWidth::from_str(&content)),
                    },
                }],
                content,
                aux_data: None,
            }),
            diagnostics,
        }
    }
}
