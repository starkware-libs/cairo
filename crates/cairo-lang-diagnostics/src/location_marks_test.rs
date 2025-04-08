use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileKind, FileLongId, VirtualFile};
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_filesystem::test_utils::FilesDatabaseForTesting;
use cairo_lang_utils::Intern;
use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use super::get_location_marks;
use crate::DiagnosticLocation;

#[test]
fn test_location_marks() {
    let content = indoc! {"
        First liné,
        Second liné.
        Third liné."};
    // Note that content does not end with '\n'.

    let db = FilesDatabaseForTesting::default();
    let file = FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "name".into(),
        content: content.into(),
        code_mappings: [].into(),
        kind: FileKind::Module,
    })
    .intern(&db);
    let summary = db.file_summary(file).unwrap();
    let second_line = summary.line_offsets[1];
    let third_line = summary.line_offsets[2];

    // Empty span.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: second_line.add_width(TextWidth::new_for_testing(13)),
            end: second_line.add_width(TextWidth::new_for_testing(13)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
            Second liné.
                        ^
        "}
    );

    // Span of length 1.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: third_line.add_width(TextWidth::new_for_testing(3)),
            end: third_line.add_width(TextWidth::new_for_testing(4)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
            Third liné.
               ^
        "}
    );

    // Span of length 2.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: third_line.add_width(TextWidth::new_for_testing(3)),
            end: third_line.add_width(TextWidth::new_for_testing(5)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
            Third liné.
               ^^
        "}
    );

    // Span of length > 1.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: second_line.add_width(TextWidth::new_for_testing(7)),
            end: second_line.add_width(TextWidth::new_for_testing(12)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
            Second liné.
                   ^^^^
        "}
    );

    // Multiline span.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: second_line.add_width(TextWidth::new_for_testing(7)),
            end: third_line.add_width(TextWidth::new_for_testing(2)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
              Second liné.
             ________^
            | Third liné.
            |__^
        "}
    );

    // Span that ends past the end of the file.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: third_line.add_width(TextWidth::new_for_testing(7)),
            end: summary.last_offset.add_width(TextWidth::from_char('\n')),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
            Third liné.
                   ^^^^
        "}
    );

    // Empty span past the end of the file.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: summary.last_offset, end: summary.last_offset },
    };

    assert_eq!(
        get_location_marks(&db, &location, true) + "\n",
        indoc! {"
            Third liné.
                       ^
        "}
    );
}
