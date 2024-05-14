use std::iter::Sum;
use std::ops::{Add, Sub};

use crate::db::FilesGroup;
use crate::ids::FileId;

#[cfg(test)]
#[path = "span_test.rs"]
mod test;

/// Byte length of an utf8 string.
// This wrapper type is used to avoid confusion with non-utf8 sizes.
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
    pub fn add_width(self, width: TextWidth) -> Self {
        TextOffset(self.0 + width)
    }
    pub fn sub_width(self, width: TextWidth) -> Self {
        TextOffset(self.0 - width)
    }
    pub fn take_from(self, content: &str) -> &str {
        &content[(self.0.0 as usize)..]
    }
}
impl Sub for TextOffset {
    type Output = TextWidth;

    fn sub(self, rhs: Self) -> Self::Output {
        TextWidth(self.0.0 - rhs.0.0)
    }
}

/// A range of text offsets that form a span (like text selection).
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct TextSpan {
    pub start: TextOffset,
    pub end: TextOffset,
}
impl TextSpan {
    pub fn width(self) -> TextWidth {
        self.end - self.start
    }
    pub fn contains(self, other: Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
    pub fn take(self, content: &str) -> &str {
        &content[(self.start.0.0 as usize)..(self.end.0.0 as usize)]
    }
    pub fn n_chars(self, content: &str) -> usize {
        self.take(content).chars().count()
    }
    /// Get the span of width 0, located right after this span.
    pub fn after(self) -> Self {
        Self { start: self.end, end: self.end }
    }
    /// Get the span of width 0, located right at the beginning of this span.
    pub fn start_only(self) -> Self {
        Self { start: self.start, end: self.start }
    }
}

/// Human-readable position inside a file, in lines and characters.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TextPosition {
    /// Line index, 0 based.
    pub line: usize,
    /// Character index inside the line, 0 based.
    pub col: usize,
}

impl TextOffset {
    fn get_line_number(self, db: &dyn FilesGroup, file: FileId) -> Option<usize> {
        let summary = db.file_summary(file)?;
        assert!(
            self <= summary.last_offset,
            "TextOffset out of range. {:?} > {:?}.",
            self.0,
            summary.last_offset.0
        );
        Some(summary.line_offsets.binary_search(&self).unwrap_or_else(|x| x - 1))
    }

    /// Convert this offset to an equivalent [`TextPosition`] in the file.
    pub fn position_in_file(self, db: &dyn FilesGroup, file: FileId) -> Option<TextPosition> {
        let summary = db.file_summary(file)?;
        let line_number = self.get_line_number(db, file)?;
        let line_offset = summary.line_offsets[line_number];
        let content = db.file_content(file)?;
        let col = TextSpan { start: line_offset, end: self }.n_chars(&content);
        Some(TextPosition { line: line_number, col })
    }
}

impl TextPosition {
    /// Convert this position to an equivalent [`TextOffset`] in the file.
    pub fn offset_in_file(self, db: &dyn FilesGroup, file: FileId) -> Option<TextOffset> {
        let file_summary = db.file_summary(file)?;
        let content = db.file_content(file)?;

        let mut offset = *file_summary.line_offsets.get(self.line)?;

        let mut chars_it = offset.take_from(&content).chars();
        for _ in 0..self.col {
            let c = chars_it.next()?;
            offset = offset.add_width(TextWidth::from_char(c));
        }

        Some(offset)
    }
}

/// A set of offset-related information about a file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileSummary {
    /// Starting offsets of all lines in this file.
    pub line_offsets: Vec<TextOffset>,
    /// Offset of the last character in the file.
    pub last_offset: TextOffset,
}
impl FileSummary {
    /// Gets the number of lines
    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }
}
