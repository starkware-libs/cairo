#[cfg(test)]
#[path = "span_test.rs"]
mod test;

use std::iter::Sum;
use std::ops::{Add, Sub};

use crate::db::FilesGroup;
use crate::ids::FileId;

/// Byte length of a utf8 string.
// Note: The wrapped value is private to make sure no one gets confused with non utf8 sizes.
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextWidth(u32);
impl TextWidth {
    pub fn from_char(c: char) -> Self {
        Self(c.len_utf8() as u32)
    }
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Self {
        Self(s.len() as u32)
    }
    pub fn new_for_testing(value: u32) -> Self {
        Self(value)
    }
}
impl Add for TextWidth {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}
impl Sub for TextWidth {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}
impl Sum for TextWidth {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        Self(iter.map(|x| x.0).sum())
    }
}

/// Byte offset inside a utf8 string.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextOffset(TextWidth);
impl TextOffset {
    pub fn add_width(&self, width: TextWidth) -> Self {
        TextOffset(self.0 + width)
    }
    pub fn sub_width(&self, width: TextWidth) -> Self {
        TextOffset(self.0 - width)
    }
    pub fn take_from<'a>(&self, content: &'a str) -> &'a str {
        &content[(self.0.0 as usize)..]
    }
}
impl Sub for TextOffset {
    type Output = TextWidth;

    fn sub(self, rhs: Self) -> Self::Output {
        TextWidth(self.0.0 - rhs.0.0)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct TextSpan {
    pub start: TextOffset,
    pub end: TextOffset,
}
impl TextSpan {
    pub fn width(&self) -> TextWidth {
        self.end - self.start
    }
    pub fn contains(&self, other: Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
    pub fn take<'b>(&self, content: &'b str) -> &'b str {
        &content[(self.start.0.0 as usize)..(self.end.0.0 as usize)]
    }
    pub fn n_chars(&self, content: &str) -> usize {
        self.take(content).chars().count()
    }
    /// Get the span of width 0, located right after this span.
    pub fn after(&self) -> Self {
        Self { start: self.end, end: self.end }
    }
    /// Get the span of width 0, located right at the beginning of this span.
    pub fn start_only(&self) -> Self {
        Self { start: self.start, end: self.start }
    }
}

/// Human readable position inside a file, in lines and characters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextPosition {
    /// Line index, 0 based.
    pub line: usize,
    /// Character index inside the line, 0 based.
    pub col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileSummary {
    pub line_offsets: Vec<TextOffset>,
    pub last_offset: TextOffset,
}

impl TextOffset {
    pub fn get_line_number(&self, db: &dyn FilesGroup, file: FileId) -> Option<usize> {
        let summary = db.file_summary(file)?;
        assert!(
            *self <= summary.last_offset,
            "TextOffset out of range. {:?} > {:?}.",
            self.0,
            summary.last_offset.0
        );
        Some(summary.line_offsets.binary_search(self).unwrap_or_else(|x| x - 1))
    }

    pub fn position_in_file(&self, db: &dyn FilesGroup, file: FileId) -> Option<TextPosition> {
        let summary = db.file_summary(file)?;
        let line_number = self.get_line_number(db, file)?;
        let line_offset = summary.line_offsets[line_number];
        let content = db.file_content(file)?;
        let col = TextSpan { start: line_offset, end: *self }.n_chars(&content);
        Some(TextPosition { line: line_number, col })
    }
}

impl FileSummary {
    /// Gets the number of lines
    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }
}
