use std::sync::Arc;

use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use filesystem::span::{TextOffset, TextSpan};
use indoc::indoc;
use pretty_assertions::assert_eq;

use super::get_location_marks;
use crate::DiagnosticLocation;

// Test salsa database.
#[salsa::database(FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

#[test]
fn test_location_marks() {
    let content = indoc! {"
        First line,
        Second line.
        Third line.
    "};

    let db = DatabaseImpl::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new(content.into()),
    }));
    let summary = db.file_summary(file).unwrap();
    let second_line = summary.line_offsets[1];
    let third_line = summary.line_offsets[2];

    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: third_line.add(3), end: third_line.add(4) },
    };

    assert_eq!(
        get_location_marks(&db, location) + "\n",
        indoc! {"
            Third line.
               ^
        "}
    );

    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: second_line.add(7), end: second_line.add(11) },
    };

    assert_eq!(
        get_location_marks(&db, location) + "\n",
        indoc! {"
            Second line.
                   ^**^
        "}
    );

    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: third_line.add(7), end: TextOffset(summary.total_length + 1) },
    };

    assert_eq!(
        get_location_marks(&db, location) + "\n",
        indoc! {"
            Third line.
                   ^**^
        "}
    );
}
