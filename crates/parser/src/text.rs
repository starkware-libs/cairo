use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct CodeSource {
    pub file_path: String,
}
impl Display for CodeSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.file_path))
    }
}

#[derive(Clone, Debug)]
pub struct CodePosition {
    pub source: CodeSource,
    pub position: TextPosition,
}
impl Display for CodePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "file {}, line {}, col {}",
            self.source, self.position.line, self.position.col
        ))
    }
}

#[derive(Clone, Debug)]
pub struct CodeSpan {
    pub source: CodeSource,
    pub span: TextSpan,
}
impl CodeSpan {
    pub fn from(&self) -> CodePosition {
        CodePosition { source: self.source.clone(), position: self.span.from }
    }
    pub fn to(&self) -> CodePosition {
        CodePosition { source: self.source.clone(), position: self.span.to }
    }
}
impl Display for CodeSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.from().fmt(f)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TextPosition {
    pub offset: u32,
    pub line: u32,
    pub col: u32,
}
impl TextPosition {
    pub fn for_source(&self, source: CodeSource) -> CodePosition {
        CodePosition { source, position: *self }
    }
}
impl Default for TextPosition {
    fn default() -> Self {
        Self { offset: 0, line: 1, col: 1 }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TextSpan {
    pub from: TextPosition,
    pub to: TextPosition,
}
impl TextSpan {
    pub fn for_source(&self, source: CodeSource) -> CodeSpan {
        CodeSpan { source, span: *self }
    }
    pub fn width(&self) -> u32 {
        self.to.offset - self.from.offset
    }
}
