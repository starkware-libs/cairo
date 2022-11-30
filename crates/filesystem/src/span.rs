#[cfg(test)]
#[path = "span_test.rs"]
mod test;

use std::ops::Sub;

use crate::db::FilesGroup;
use crate::ids::FileId;

// TODO(spapini): Be consistent in the project with u32 or usize offsets.

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextOffset(pub usize);
impl TextOffset {
    pub fn inc(&mut self) {
        self.0 += 1;
    }
    pub fn add(&self, width: usize) -> Self {
        TextOffset(self.0 + width)
    }
}
impl Sub for TextOffset {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 - rhs.0
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct TextSpan {
    pub start: TextOffset,
    pub end: TextOffset,
}
impl TextSpan {
    pub fn width(&self) -> u32 {
        (self.end - self.start) as u32
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextPosition {
    // Both are 0 based.
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileSummary {
    pub line_offsets: Vec<TextOffset>,
    pub total_length: usize,
}

#[allow(dead_code)]
impl TextOffset {
    pub fn get_line_number(&self, db: &dyn FilesGroup, file: FileId) -> Option<usize> {
        let summary = db.file_summary(file)?;
        assert!(
            self.0 <= summary.total_length,
            "TextOffset out of range. {} > {}.",
            self.0,
            summary.total_length
        );
        Some(summary.line_offsets.binary_search(self).unwrap_or_else(|x| x - 1))
    }

    pub fn position_in_file(&self, db: &dyn FilesGroup, file: FileId) -> Option<TextPosition> {
        let summary = db.file_summary(file)?;
        let line_number = self.get_line_number(db, file)?;
        let line_offset = summary.line_offsets[line_number];
        Some(TextPosition { line: line_number, col: *self - line_offset })
    }
}

impl FileSummary {
    /// Gets the number of lines
    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }
}
