use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_macro::{Severity, TokenStream};
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;

pub mod inline;
pub mod regular;

// <https://github.com/software-mansion/scarb/blob/4e81d1c4498137f80e211c6e2c6a5a6de01c66f2/scarb/src/compiler/plugin/proc_macro/ffi.rs#L30-L40>
trait FromSyntaxNode {
    fn from_syntax_node(db: &dyn SyntaxGroup, node: &impl TypedSyntaxNode) -> Self;
}

impl FromSyntaxNode for TokenStream {
    fn from_syntax_node(db: &dyn SyntaxGroup, node: &impl TypedSyntaxNode) -> Self {
        let mut builder = PatchBuilder::new(db, node);
        builder.add_node(node.as_syntax_node());
        Self::new(builder.build().0)
    }
}

// <https://github.com/software-mansion/scarb/blob/4e81d1c4498137f80e211c6e2c6a5a6de01c66f2/scarb/src/compiler/plugin/proc_macro/host.rs#L1068-L1083>
fn into_cairo_diagnostics(
    diagnostics: Vec<cairo_lang_macro::Diagnostic>,
    stable_ptr: SyntaxStablePtrId,
) -> Vec<PluginDiagnostic> {
    diagnostics
        .into_iter()
        .map(|diag| PluginDiagnostic {
            stable_ptr,
            message: diag.message,
            severity: match diag.severity {
                Severity::Error => cairo_lang_diagnostics::Severity::Error,
                Severity::Warning => cairo_lang_diagnostics::Severity::Warning,
            },
        })
        .collect()
}
