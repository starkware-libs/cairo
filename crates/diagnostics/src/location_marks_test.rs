use std::sync::Arc;

use filesystem::db::FilesGroup;
use filesystem::ids::{FileLongId, VirtualFile};
use filesystem::span::{TextOffset, TextSpan};
use filesystem::test_utils::FilesDatabaseForTesting;
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use super::get_location_marks;
use crate::DiagnosticLocation;

#[test]
fn test_location_marks() {
    let content = indoc! {"
        First line,
        Second line.
        Third line."};
    // Note that content does not end with '\n'.

    let db = FilesDatabaseForTesting::default();
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: Arc::new(content.into()),
    }));
    let summary = db.file_summary(file).unwrap();
    let second_line = summary.line_offsets[1];
    let third_line = summary.line_offsets[2];

    // Empty span.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: second_line.add(12), end: second_line.add(12) },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Second line.
                        ^
        "}
    );

    // Span of length 1.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: third_line.add(3), end: third_line.add(4) },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Third line.
               ^
        "}
    );

    // Span of length 2.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: third_line.add(3), end: third_line.add(5) },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Third line.
               ^^
        "}
    );

    // Span of length > 1.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: second_line.add(7), end: second_line.add(11) },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Second line.
                   ^**^
        "}
    );

    // Multiline span.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: second_line.add(7), end: third_line.add(2) },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Second line.
                   ^***^
        "}
    );

    // Span that ends past the end of the file.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: third_line.add(7), end: TextOffset(summary.total_length + 1) },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Third line.
                   ^**^
        "}
    );

    // Empty span past the end of the file.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: TextOffset(summary.total_length),
            end: TextOffset(summary.total_length),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Third line.
                       ^
        "}
    );
}
