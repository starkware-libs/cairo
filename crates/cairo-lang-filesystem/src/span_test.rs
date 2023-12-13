use std::sync::Arc;

use test_log::test;

use super::TextOffset;
use crate::db::FilesGroup;
use crate::ids::{FileKind, FileLongId, VirtualFile};
use crate::span::{TextPosition, TextWidth};
use crate::test_utils::FilesDatabaseForTesting;

const TEST_STRING: &str = "01\n23\u{1230}\r\n456\n\n\r\n789";

#[test]
fn test_span() {
    let db = FilesDatabaseForTesting::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new(TEST_STRING.into()),
        code_mappings: Default::default(),
        kind: FileKind::Module,
    }));
    assert_eq!(
        TextOffset(TextWidth(0)).position_in_file(&db, file),
        Some(TextPosition { line: 0, col: 0 })
    );
    assert_eq!(
        TextOffset(TextWidth(1)).position_in_file(&db, file),
        Some(TextPosition { line: 0, col: 1 })
    );
    assert_eq!(
        TextOffset(TextWidth(2)).position_in_file(&db, file),
        Some(TextPosition { line: 0, col: 2 })
    );
    assert_eq!(
        TextOffset(TextWidth(3)).position_in_file(&db, file),
        Some(TextPosition { line: 1, col: 0 })
    );
    // Offsets 6,7 is inside a unicode char.
    assert_eq!(
        TextOffset(TextWidth(8)).position_in_file(&db, file),
        Some(TextPosition { line: 1, col: 3 })
    );
    assert_eq!(
        TextOffset(TextWidth(9)).position_in_file(&db, file),
        Some(TextPosition { line: 1, col: 4 })
    );
    assert_eq!(
        TextOffset(TextWidth(10)).position_in_file(&db, file),
        Some(TextPosition { line: 2, col: 0 })
    );
    assert_eq!(
        TextOffset(TextWidth(14)).position_in_file(&db, file),
        Some(TextPosition { line: 3, col: 0 })
    );
    assert_eq!(
        TextOffset(TextWidth(15)).position_in_file(&db, file),
        Some(TextPosition { line: 4, col: 0 })
    );
    assert_eq!(
        TextOffset(TextWidth(16)).position_in_file(&db, file),
        Some(TextPosition { line: 4, col: 1 })
    );
    assert_eq!(
        TextOffset(TextWidth(17)).position_in_file(&db, file),
        Some(TextPosition { line: 5, col: 0 })
    );
    assert_eq!(
        TextOffset(TextWidth(19)).position_in_file(&db, file),
        Some(TextPosition { line: 5, col: 2 })
    );
    assert_eq!(
        TextOffset(TextWidth(20)).position_in_file(&db, file),
        Some(TextPosition { line: 5, col: 3 })
    );
}

#[test]
#[should_panic(expected = "TextOffset out of range. TextWidth(21) > TextWidth(20).")]
fn should_panic_test_span_out_of_range() {
    let db = FilesDatabaseForTesting::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new(TEST_STRING.into()),
        code_mappings: Default::default(),
        kind: FileKind::Module,
    }));
    TextOffset(TextWidth(TEST_STRING.len() as u32 + 1)).position_in_file(&db, file);
}
