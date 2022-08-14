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
    let mut bag = Diagnostics::default();
    let diagnostic = SimpleDiag {};
    bag.add(Box::new(diagnostic));
    bag.0[0].as_any().downcast_ref::<SimpleDiag>().expect("Unexpected type");
}

#[test]
fn test_option_with_diagnostics() {
    dummy_compute_macro(2);
}

#[with_diagnostics]
fn dummy_compute_macro(diagnostics: &mut Diagnostics, x: usize) -> Option<usize> {
    let param = WithDiagnostics::pure(Some(x * x));
    let res = param.unwrap(diagnostics)?;
    Some(res * res)
}
