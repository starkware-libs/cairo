use std::fmt::Display;
use std::ops::Sub;

use filesystem::ids::FileId;

#[derive(Clone, Debug)]
pub struct CodePosition {
    pub source: FileId,
    pub position: TextOffset,
}
impl Display for CodePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "file ID {:?}, offset {}", self.source, self.position.as_usize())
    }
}

#[derive(Clone, Debug)]
pub struct CodeSpan {
    pub source: FileId,
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
pub struct TextOffset(pub u32);
impl TextOffset {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}
impl Sub for TextOffset {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.as_usize() - rhs.as_usize()
    }
}
impl From<u32> for TextOffset {
    fn from(x: u32) -> Self {
        TextOffset(x)
    }
}
impl Default for TextOffset {
    fn default() -> Self {
        TextOffset(0)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TextSpan {
    pub from: TextOffset,
    pub to: TextOffset,
}
impl TextSpan {
    pub fn for_source(&self, source: FileId) -> CodeSpan {
        CodeSpan { source, span: *self }
    }
    pub fn width(&self) -> u32 {
        (self.to - self.from) as u32
    }
}
