use cairo_lang_filesystem::ids::{
    FileId, FileKind, FileLongId, SmolStrId, SpanInFile, VirtualFile,
};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_filesystem::test_utils::FilesDatabaseForTesting;
use cairo_lang_test_utils::test;
use cairo_lang_utils::Intern;
use indoc::indoc;
use salsa::Database;

use super::{DiagnosticEntry, DiagnosticNote, Diagnostics, DiagnosticsBuilder, Severity};

// Test diagnostic.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::Update)]
struct SimpleDiag<'db> {
    file_id: FileId<'db>,
    message: String,
    severity: Severity,
    notes: Vec<DiagnosticNote<'db>>,
}
impl<'db> SimpleDiag<'db> {
    fn new(file_id: FileId<'db>) -> Self {
        Self {
            file_id,
            message: "Simple diagnostic.".into(),
            severity: Severity::Error,
            notes: vec![],
        }
    }

    fn with_severity(file_id: FileId<'db>, severity: Severity) -> Self {
        Self { file_id, message: "Simple diagnostic.".into(), severity, notes: vec![] }
    }

    fn with_notes(file_id: FileId<'db>, notes: Vec<DiagnosticNote<'db>>) -> Self {
        Self { file_id, message: "Simple diagnostic.".into(), severity: Severity::Error, notes }
    }
}
impl<'db> DiagnosticEntry<'db> for SimpleDiag<'db> {
    fn format(&self, _db: &dyn Database) -> String {
        self.message.clone()
    }

    fn location(&self, _db: &'db dyn Database) -> SpanInFile<'db> {
        SpanInFile {
            file_id: self.file_id,
            span: TextSpan::new(TextOffset::START, TextWidth::new_for_testing(6).as_offset()),
        }
    }

    fn severity(&self) -> Severity {
        self.severity
    }

    fn notes(&self, _db: &'db dyn Database) -> &[DiagnosticNote<'db>] {
        &self.notes
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
    let diagnostic = SimpleDiag::new(file_id);
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

#[test]
fn test_empty_diagnostics() {
    let db_val = FilesDatabaseForTesting::default();
    let diagnostics: Diagnostics<'_, SimpleDiag<'_>> = Diagnostics::new();

    assert!(diagnostics.is_empty());
    assert!(!diagnostics.has_errors());
    assert!(diagnostics.check_error_free().is_ok());
    assert_eq!(diagnostics.format(&db_val), "");
}

#[test]
fn test_multiple_diagnostics() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut diagnostics: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    diagnostics.add(SimpleDiag::new(file_id));
    diagnostics.add(SimpleDiag {
        file_id,
        message: "Second diagnostic.".into(),
        severity: Severity::Error,
        notes: vec![],
    });

    let built = diagnostics.build();
    assert!(!built.is_empty());
    assert!(built.has_errors());
    assert_eq!(built.0.error_count, 2);
    assert!(built.check_error_free().is_err());
}

#[test]
fn test_warning_severity() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut diagnostics: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    diagnostics.add(SimpleDiag::with_severity(file_id, Severity::Warning));

    let built = diagnostics.build();
    assert!(!built.is_empty());
    assert!(!built.has_errors());
    assert_eq!(built.0.error_count, 0);
    assert!(built.check_error_free().is_ok());
    assert!(built.format(&db_val).contains("warning:"));
}

#[test]
fn test_extend_diagnostics() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut builder1: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    builder1.add(SimpleDiag::new(file_id));
    let diagnostics1 = builder1.build();

    let mut builder2: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    builder2.add(SimpleDiag {
        file_id,
        message: "Extended diagnostic.".into(),
        severity: Severity::Error,
        notes: vec![],
    });
    let diagnostics2 = builder2.build();

    let mut builder3: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    builder3.extend(diagnostics1);
    builder3.extend(diagnostics2);

    let merged = builder3.build();
    assert_eq!(merged.0.error_count, 2);
    assert!(merged.has_errors());
}

#[test]
fn test_merge_diagnostics() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut builder1: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    builder1.add(SimpleDiag::new(file_id));
    let diagnostics1 = builder1.build();

    let mut builder2: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    builder2.add(SimpleDiag {
        file_id,
        message: "Merged diagnostic.".into(),
        severity: Severity::Error,
        notes: vec![],
    });
    let diagnostics2 = builder2.build();

    let merged = diagnostics1.merge(diagnostics2);
    assert_eq!(merged.0.error_count, 2);
    assert!(merged.has_errors());
}

#[test]
fn test_diagnostics_with_notes() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let note = DiagnosticNote::text_only("This is a note.".to_string());
    let mut diagnostics: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    diagnostics.add(SimpleDiag::with_notes(file_id, vec![note]));

    let formatted = diagnostics.build().format(&db_val);
    assert!(formatted.contains("note:"));
    assert!(formatted.contains("This is a note."));
}

#[test]
fn test_get_diagnostics_without_duplicates() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut diagnostics: DiagnosticsBuilder<'_, SimpleDiag<'_>> = DiagnosticsBuilder::default();
    let diag = SimpleDiag::new(file_id);
    diagnostics.add(diag.clone());
    diagnostics.add(diag.clone());

    let built = diagnostics.build();
    let all = built.get_all();
    assert_eq!(all.len(), 2);

    let without_duplicates = built.get_diagnostics_without_duplicates(&db_val);
    assert_eq!(without_duplicates.len(), 1);
}
