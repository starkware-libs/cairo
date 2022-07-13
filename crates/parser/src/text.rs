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
    pub position: u32,
}
impl Display for CodePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("file {}, offset {}", self.source, self.position))
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
pub struct TextSpan {
    pub from: u32,
    pub to: u32,
}
impl TextSpan {
    pub fn for_source(&self, source: CodeSource) -> CodeSpan {
        CodeSpan { source, span: *self }
    }
    pub fn width(&self) -> u32 {
        self.to - self.from
    }
}
