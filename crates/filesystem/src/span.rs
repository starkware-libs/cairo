#[cfg(test)]
#[path = "span_test.rs"]
mod span_test;

use std::ops::Sub;

use crate::db::FilesGroup;
use crate::ids::FileId;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TextOffset(pub usize);
impl Sub for TextOffset {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 - rhs.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextSpan {
    pub start: TextOffset,
    pub end: TextOffset,
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
    pub fn position_in_file(&self, db: &dyn FilesGroup, file: FileId) -> Option<TextPosition> {
        let summary = db.file_summary(file)?;
        assert!(
            self.0 <= summary.total_length,
            "TextOffset out of range. {} > {}.",
            self.0,
            summary.total_length
        );
        let index = summary.line_offsets.binary_search(self).unwrap_or_else(|x| x - 1);
        let line_offset = summary.line_offsets[index];
        Some(TextPosition { line: index, col: *self - line_offset })
    }
}
