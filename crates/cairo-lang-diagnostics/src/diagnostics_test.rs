use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileInfo, FileKind, FileLongId, VirtualFile};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_filesystem::test_utils::FilesDatabaseForTesting;
use indoc::indoc;
use test_log::test;

use super::{DiagnosticEntry, DiagnosticLocation, DiagnosticsBuilder};

// Test diagnostic.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct SimpleDiag {
    file_id: FileLongId,
}
impl DiagnosticEntry for SimpleDiag {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn cairo_lang_filesystem::db::FilesGroup) -> String {
        "Simple diagnostic.".into()
    }

    fn location(&self, _db: &dyn cairo_lang_filesystem::db::FilesGroup) -> DiagnosticLocation {
        DiagnosticLocation {
            file_id: self.file_id,
            span: TextSpan {
                start: TextOffset::START,
                end: TextWidth::new_for_testing(6).as_offset(),
            },
        }
    }

    fn is_same_kind(&self, _other: &Self) -> bool {
        true
    }
}

fn setup() -> (FilesDatabaseForTesting, FileLongId) {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = FileLongId::new(
        &db_val,
        FileInfo::Virtual(VirtualFile {
            parent: None,
            name: "dummy_file.sierra".into(),
            content: "abcd\nefg.\n".into(),
            code_mappings: [].into(),
            kind: FileKind::Module,
            original_item_removed: false,
        }),
    );
    (db_val, file_id)
}

#[test]
fn test_diagnostics() {
    let (db_val, file_id) = setup();

    let mut diagnostics: DiagnosticsBuilder<SimpleDiag> = DiagnosticsBuilder::default();
    let diagnostic = SimpleDiag { file_id };
    diagnostics.add(diagnostic);

    assert_eq!(
        diagnostics.build().format(&db_val),
        indoc! { "
            error: Simple diagnostic.
             --> dummy_file.sierra:1:1-2:1
              abcd
             _^
            | efg.
            |_^

        " }
    );
}
