use cairo_lang_filesystem::span::TextPosition;
use tower_lsp::lsp_types::Position;

pub trait ToLsp {
    type Output;
    fn to_lsp(&self) -> Self::Output;
}

impl ToLsp for TextPosition {
    type Output = Position;
    fn to_lsp(&self) -> Self::Output {
        Position { line: self.line as u32, character: self.col as u32 }
    }
}
