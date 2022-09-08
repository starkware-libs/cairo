use std::sync::Arc;

use diagnostics_proc_macros::with_diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::{FileId, FileLongId, VirtualFile};
use filesystem::span::{TextOffset, TextSpan};
use filesystem::test_utils::FilesDatabaseForTesting;
use indoc::indoc;

use super::{DiagnosticEntry, DiagnosticLocation, Diagnostics, WithDiagnostics};

// Test diagnostic.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct SimpleDiag {
    file_id: FileId,
}
impl DiagnosticEntry for SimpleDiag {
    type DbType = dyn FilesGroup;

    fn format(&self, _db: &dyn filesystem::db::FilesGroup) -> String {
        "Simple diagnostic.".into()
    }

    fn location(&self, _db: &dyn filesystem::db::FilesGroup) -> DiagnosticLocation {
        DiagnosticLocation {
            file_id: self.file_id,
            span: TextSpan { start: TextOffset(0), end: TextOffset(6) },
        }
    }
}

fn setup() -> (FilesDatabaseForTesting, FileId) {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = db_val.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "dummy_file.sierra".into(),
        content: Arc::new("abcd\nefg.\n".into()),
    }));
    (db_val, file_id)
}

#[test]
fn test_diagnostics() {
    let (_db_val, file_id) = setup();

    let mut diagnostics: Diagnostics<SimpleDiag> = Diagnostics::new();
    let diagnostic = SimpleDiag { file_id };
    diagnostics.add(diagnostic);
}

#[test]
fn test_option_with_diagnostics() {
    let (db_val, file_id) = setup();
    let db = &db_val;

    let res = dummy_compute_macro(2, file_id);
    assert_eq!(
        res.diagnostics.format(db),
        indoc! { "
            error: Simple diagnostic.
             --> dummy_file.sierra:1:1
            abcd
            ^**^

        " }
    );
}
#[with_diagnostics]
fn dummy_compute_macro(
    diagnostics: &mut Diagnostics<SimpleDiag>,
    x: usize,
    file_id: FileId,
) -> Option<usize> {
    let param = WithDiagnostics::pure(Some(x * x));
    let res = param.propagate(diagnostics)?;
    // This should add one diagnostic entry, and return.
    dummy_compute_fail(file_id).propagate(diagnostics)?;
    Some(res)
}

#[with_diagnostics]
fn dummy_compute_fail(diagnostics: &mut Diagnostics<SimpleDiag>, file_id: FileId) -> Option<usize> {
    diagnostics.add(SimpleDiag { file_id });
    None
}
