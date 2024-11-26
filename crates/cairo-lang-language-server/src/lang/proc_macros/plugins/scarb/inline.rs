use cairo_lang_defs::plugin::{InlinePluginResult, PluginGeneratedFile};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_macro::TokenStream;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use scarb_proc_macro_server_types::methods::expand::ExpandInlineMacroParams;

use super::{FromSyntaxNode, into_cairo_diagnostics};
use crate::lang::db::AnalysisDatabase;
use crate::lang::proc_macros::db::ProcMacroGroup;

// <https://github.com/software-mansion/scarb/blob/4e81d1c4498137f80e211c6e2c6a5a6de01c66f2/scarb/src/compiler/plugin/proc_macro/host.rs#L1015-L1059>
pub fn inline_macro_generate_code(
    db: &AnalysisDatabase,
    syntax: &ast::ExprInlineMacro,
) -> InlinePluginResult {
    let origin = CodeOrigin::Span(syntax.as_syntax_node().span(db));
    let stable_ptr = syntax.clone().stable_ptr().untyped();
    let token_stream = TokenStream::from_syntax_node(db, syntax);
    // region: Modified scarb code
    let result = db.get_inline_macros_expansion(ExpandInlineMacroParams {
        name: syntax.path(db).as_syntax_node().get_text(db),
        args: token_stream,
    });
    // endregion
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
                diagnostics_note: Default::default(),
            }),
            diagnostics,
        }
    }
}
