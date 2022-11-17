use std::sync::Arc;

use test_log::test;

use super::TextOffset;
use crate::db::FilesGroup;
use crate::ids::{FileLongId, VirtualFile};
use crate::span::TextPosition;
use crate::test_utils::FilesDatabaseForTesting;

const TEST_STRING: &str = "01\n23\u{1230}\r\n456\n\n\r\n789";

#[test]
fn test_span() {
    let db = FilesDatabaseForTesting::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new(TEST_STRING.into()),
    }));
    assert_eq!(TextOffset(0).position_in_file(&db, file), Some(TextPosition { line: 0, col: 0 }));
    assert_eq!(TextOffset(1).position_in_file(&db, file), Some(TextPosition { line: 0, col: 1 }));
    assert_eq!(TextOffset(2).position_in_file(&db, file), Some(TextPosition { line: 0, col: 2 }));
    assert_eq!(TextOffset(3).position_in_file(&db, file), Some(TextPosition { line: 1, col: 0 }));
    assert_eq!(TextOffset(6).position_in_file(&db, file), Some(TextPosition { line: 1, col: 3 }));
    assert_eq!(TextOffset(7).position_in_file(&db, file), Some(TextPosition { line: 1, col: 4 }));
    assert_eq!(TextOffset(8).position_in_file(&db, file), Some(TextPosition { line: 2, col: 0 }));
    assert_eq!(TextOffset(12).position_in_file(&db, file), Some(TextPosition { line: 3, col: 0 }));
    assert_eq!(TextOffset(13).position_in_file(&db, file), Some(TextPosition { line: 4, col: 0 }));
    assert_eq!(TextOffset(14).position_in_file(&db, file), Some(TextPosition { line: 4, col: 1 }));
    assert_eq!(TextOffset(15).position_in_file(&db, file), Some(TextPosition { line: 5, col: 0 }));
    assert_eq!(TextOffset(17).position_in_file(&db, file), Some(TextPosition { line: 5, col: 2 }));
    assert_eq!(TextOffset(18).position_in_file(&db, file), Some(TextPosition { line: 5, col: 3 }));
}

#[test]
#[should_panic(expected = "TextOffset out of range. 19 > 18.")]
fn test_span_out_of_range() {
    let db = FilesDatabaseForTesting::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new(TEST_STRING.into()),
    }));
    TextOffset(TEST_STRING.chars().count() + 1).position_in_file(&db, file);
}
