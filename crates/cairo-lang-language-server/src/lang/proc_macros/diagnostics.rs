use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_macro::Severity;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;

//scarb code

pub fn into_cairo_diagnostics(
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
