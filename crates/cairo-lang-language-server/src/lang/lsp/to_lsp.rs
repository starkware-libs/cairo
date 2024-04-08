use cairo_lang_filesystem::span::TextPosition;
use tower_lsp::lsp_types::Position;

/// Convert a type into its LSP equivalent.
///
/// This trait should be used for conversions, where there is a direct mapping between the types,
/// and no extra context is needed. Many conversions may need access to the compiler database,
/// and such ones are mostly implemented in the [`LsProtoGroup`] extension trait.
///
/// [`LsProtoGroup`]: crate::lang::lsp::LsProtoGroup
pub trait ToLsp {
    /// LSP equivalent type.
    type Output;

    /// Convert the type into its LSP equivalent.
    fn to_lsp(&self) -> Self::Output;
}

impl ToLsp for TextPosition {
    type Output = Position;
    fn to_lsp(&self) -> Self::Output {
        Position { line: self.line as u32, character: self.col as u32 }
    }
}
