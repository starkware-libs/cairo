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
        Third liné.
        match something {
            This is a new scope
        };
        let a = SomeStruct { some_value };
        let b = if true { a } else { b };
        let a_long_variable_name_to_go_to_newline = if someverylongbooleanvalue {
            if someverylongfunctionname() {
                some_value
            } else {
                some_other_value
            }
        } else {
            someotherevenlongerfunctionnamewhichisveryveryveryveryverylong()
        };
        Scope is closed."};
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
    let match_begin = summary.line_offsets[3];
    let match_inside = summary.line_offsets[4];
    let match_end = summary.line_offsets[5];
    let struct_creation = summary.line_offsets[6];
    let conditional_assignment = summary.line_offsets[7];
    let multiline_conditional = summary.line_offsets[8];
    let close_else = summary.line_offsets[16];
    let last_line = summary.line_offsets[17];

    // Empty span.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: second_line.add_width(TextWidth::new_for_testing(13)),
            end: second_line.add_width(TextWidth::new_for_testing(13)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
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
        get_location_marks(&db, &location) + "\n",
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
        get_location_marks(&db, &location) + "\n",
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
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Second liné.
                   ^**^
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
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   Second liné.
            |   Th
            |____^
        "}
    );

    // Properly indents scopes
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: match_begin,
            end: match_end.add_width(TextWidth::new_for_testing(1)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   match something {
            |       This is a new scope
            |   }
            |___^
        "}
    );

    // Same as above but includes next line. I don't see when it could happen but it works.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: match_begin,
            end: conditional_assignment.add_width(TextWidth::new_for_testing(3)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   match something {
            |       This is a new scope
            |   };
            |   let a = SomeStruct { some_value };
            |   let
            |_____^
        "}
    );

    // Start inside scope so not ideally formatted not sure when this can happen
    // If needed TODO(Lucas): handle start from inside scope
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: match_inside,
            end: match_end.add_width(TextWidth::new_for_testing(2)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   This is a new scope
            |   };
            |____^
        "}
    );

    // Properly formats a one liner that has scopes.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: conditional_assignment,
            end: multiline_conditional.add_width(TextWidth::new_for_testing(3)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   let b = if true { a } else { b };
            |   let
            |_____^
        "}
    );
    // Properly formats struct creation
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: struct_creation,
            end: conditional_assignment.add_width(TextWidth::new_for_testing(33)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   let a = SomeStruct { some_value };
            |   let b = if true { a } else { b };
            |___________________________________^
        "}
    );

    // nested scopes
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: multiline_conditional,
            end: close_else.add_width(TextWidth::new_for_testing(2)),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            \\   let a_long_variable_name_to_go_to_newline = if someverylongbooleanvalue {
            |       if someverylongfunctionname() {
            |           some_value
            |       } else {
            |           some_other_value
            |       }
            |   } else {
            |       someotherevenlongerfunctionnamewhichisveryveryveryveryverylong()
            |   };
            |____^
        "}
    );

    // Span that ends past the end of the file.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan {
            start: last_line.add_width(TextWidth::new_for_testing(7)),
            end: summary.last_offset.add_width(TextWidth::from_char('\n')),
        },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Scope is closed.
                   ^*******^
        "}
    );

    // Empty span past the end of the file.
    let location = DiagnosticLocation {
        file_id: file,
        span: TextSpan { start: summary.last_offset, end: summary.last_offset },
    };

    assert_eq!(
        get_location_marks(&db, &location) + "\n",
        indoc! {"
            Scope is closed.
                            ^
        "}
    );
}
