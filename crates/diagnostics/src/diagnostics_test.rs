use diagnostics_proc_macros::with_diagnostics;

use super::{DiagnosticEntry, DiagnosticLocation, Diagnostics, WithDiagnostics};

struct SimpleDiag {}
impl DiagnosticEntry for SimpleDiag {
    fn format(&self, _db: &dyn filesystem::db::FilesGroup) -> String {
        unimplemented!();
    }

    fn location(&self, _db: &dyn filesystem::db::FilesGroup) -> DiagnosticLocation {
        unimplemented!();
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[test]
fn test_diagnostics() {
    let mut diagnostics = Diagnostics::default();
    let diagnostic = SimpleDiag {};
    diagnostics.add(diagnostic);
    diagnostics.0[0].as_any().downcast_ref::<SimpleDiag>().expect("Unexpected type");
}

#[test]
fn test_option_with_diagnostics() {
    assert_matches!(
        dummy_compute_macro(2),
        WithDiagnostics { value: None, diagnostics } if diagnostics.0.len() == 1);
}

#[with_diagnostics]
fn dummy_compute_macro(diagnostics: &mut Diagnostics, x: usize) -> Option<usize> {
    let param = WithDiagnostics::pure(Some(x * x));
    let res = param.unwrap(diagnostics)?;
    // This should add one diagnostic entry, and return.
    let _param2 = dummy_compute_fail().unwrap(diagnostics)?;
    Some(res)
}

#[with_diagnostics]
fn dummy_compute_fail(diagnostics: &mut Diagnostics) -> Option<usize> {
    diagnostics.add(SimpleDiag {});
    None
}
