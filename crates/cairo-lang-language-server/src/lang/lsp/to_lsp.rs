use cairo_lang_filesystem::span::{TextPosition, TextPositionSpan};
use lsp_types::{Position, Range};

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

impl ToLsp for TextPositionSpan {
    type Output = Range;
    fn to_lsp(&self) -> Self::Output {
        Range { start: self.start.to_lsp(), end: self.end.to_lsp() }
    }
}

/// Convert an LSP type into its Cairo equivalent.
///
/// This trait should be used for conversions, where there is a direct mapping between the types,
/// and no extra context is needed. Many conversions may need access to the compiler database,
/// and such ones are mostly implemented in the [`LsProtoGroup`] extension trait.
///
/// [`LsProtoGroup`]: crate::lang::lsp::LsProtoGroup
pub trait ToCairo {
    /// Cairo equivalent type.
    type Output;

    /// Convert an LSP type into its Cairo equivalent.
    fn to_cairo(&self) -> Self::Output;
}

impl ToCairo for Position {
    type Output = TextPosition;
    fn to_cairo(&self) -> Self::Output {
        TextPosition { line: self.line as usize, col: self.character as usize }
    }
}

impl ToCairo for Range {
    type Output = TextPositionSpan;
    fn to_cairo(&self) -> Self::Output {
        TextPositionSpan { start: self.start.to_cairo(), end: self.end.to_cairo() }
    }
}
