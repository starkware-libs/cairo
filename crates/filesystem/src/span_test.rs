use std::sync::Arc;

use super::TextOffset;
use crate::db::{FilesDatabase, FilesGroup};
use crate::ids::{FileLongId, VirtualFile};
use crate::span::FilePosition;

#[salsa::database(FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_span() {
    let db = DatabaseImpl::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new("01\n23\u{1230}\r\n456\n\n\r\n789".into()),
    }));
    assert_eq!(TextOffset(0).file_position(&db, file), Some(FilePosition { line: 0, col: 0 }));
    assert_eq!(TextOffset(1).file_position(&db, file), Some(FilePosition { line: 0, col: 1 }));
    assert_eq!(TextOffset(2).file_position(&db, file), Some(FilePosition { line: 0, col: 2 }));
    assert_eq!(TextOffset(3).file_position(&db, file), Some(FilePosition { line: 1, col: 0 }));
    assert_eq!(TextOffset(6).file_position(&db, file), Some(FilePosition { line: 1, col: 3 }));
    assert_eq!(TextOffset(7).file_position(&db, file), Some(FilePosition { line: 1, col: 4 }));
    assert_eq!(TextOffset(8).file_position(&db, file), Some(FilePosition { line: 2, col: 0 }));
    assert_eq!(TextOffset(12).file_position(&db, file), Some(FilePosition { line: 3, col: 0 }));
    assert_eq!(TextOffset(13).file_position(&db, file), Some(FilePosition { line: 4, col: 0 }));
    assert_eq!(TextOffset(14).file_position(&db, file), Some(FilePosition { line: 4, col: 1 }));
}
