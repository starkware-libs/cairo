use cairo_lang_filesystem::ids::{FileId, FileKind, FileLongId, SmolStrId, VirtualFile};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_filesystem::test_utils::FilesDatabaseForTesting;
use cairo_lang_test_utils::test;
use cairo_lang_utils::Intern;
use indoc::indoc;
use salsa::Database;

use super::{DiagnosticEntry, DiagnosticLocation, DiagnosticsBuilder};

// Test diagnostic.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
struct SimpleDiag<'db> {
    file_id: FileId<'db>,
}
impl<'db> DiagnosticEntry<'db> for SimpleDiag<'db> {
    fn format(&self, _db: &dyn Database) -> String {
        "Simple diagnostic.".into()
    }

    fn location(&self, _db: &'db dyn Database) -> DiagnosticLocation<'db> {
        DiagnosticLocation {
            file_id: self.file_id,
            span: TextSpan::new(TextOffset::START, TextWidth::new_for_testing(6).as_offset()),
        }
    }

    fn is_same_kind(&self, _other: &Self) -> bool {
        true
    }
}

fn setup<'db>(db: &'db FilesDatabaseForTesting) -> FileId<'db> {
    FileLongId::Virtual(VirtualFile {
        parent: None,
        name: SmolStrId::from(db, "dummy_file.sierra"),
        content: SmolStrId::from(db, "abcd\nefg.\n"),
        code_mappings: [].into(),
        kind: FileKind::Module,
        original_item_removed: false,
    })
    .intern(db)
}

#[test]
fn test_diagnostics() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut diagnostics: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
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
