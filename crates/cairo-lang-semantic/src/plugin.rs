use cairo_lang_filesystem::span::TextSpan;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PluginMappedDiagnostic {
    pub span: TextSpan,
    pub message: String,
}
